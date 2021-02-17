input_data = c(AutorMatch = 'ADHPS-WebMatch/cw_patent_compustat_adhps.csv', 
               citationData = 'COMETS_data/Patent DTA/patent_cite_counts_v2.rds', 
               patentCategories = 'COMETS_data/Patent DTA/patent_zd_cats_v2.rds', 
               assigneeEntityNames = 'COMETS_data/Patent DTA/patent_assignees_v2.rds', 
               patentNumbersToYears = 'Data/YearsAndPatentNumbers.csv', 
               CompustatRawData = 'IntermediateFiles/raw_dt.feather', 
               SDCData = 'Data/SDC_data.feather')

output_files = c(patentDataForAnalysis = 'IntermediateFiles/patent_data_for_analysis.feather')


Autor_patent_match = fread(input_data['AutorMatch'], 
             colClasses = list(character = c('patent', 'gvkey')))
Autor_patent_match[, patent_id := as.numeric(patent)]

citations = readRDS(input_data['citationData'])
citations = citations[, sum(citations), patent_id]
setnames(citations, c('patent_id', 'citations'))

categories = readRDS(input_data['patentCategories'])
categories[, software := +(zd == 'com')]
categories_unique = categories[, .(software = sum(software * wt)), patent]

citations_and_categories = merge(citations, categories_unique, 
                                   by.x = 'patent_id', by.y = 'patent', 
                                   all.x = T, all.y = T)
assignees = readRDS(input_data['assigneeEntityNames'])
assignees[app_date == '', app_date := NA_character_
        ][, app_date := as.Date(fast_strptime(app_date, "%Y-%m-%d"))
        ][, grant_date := as.Date(fast_strptime(grant_date, "%Y-%m-%d"))
        ]
assignees[, diff := as.numeric(grant_date - app_date)]
setkey(assignees, patent_id)
setkey(citations_and_categories, patent_id)
citations_categories_and_assignees = merge(assignees, citations_and_categories, all.x = T, all.y = T)
citations_categories_and_assignees[is.na(citations), citations := 0
                                 ][is.na(software), software := 0]
citations_categories_and_assignees[, citation_weighted := log(citations + 2.718281828)]

app_date_mod = lm(diff ~ as.numeric(grant_date), data = citations_categories_and_assignees)

setkey(citations_categories_and_assignees, patent_id)
previous_patent_grant_date = citations_categories_and_assignees[!is.na(grant_date)
                                 ][citations_categories_and_assignees[is.na(grant_date)], grant_date, roll = T]
next_patent_grant_date = citations_categories_and_assignees[!is.na(grant_date)
                                   ][citations_categories_and_assignees[is.na(grant_date)], grant_date, roll = -Inf]

date_midpoint = function(start, end) {
  start + (end - start)/2
}
citations_categories_and_assignees[is.na(grant_date), grant_date_by_midpoint_of_patent_ids := date_midpoint(previous_patent_grant_date, next_patent_grant_date)]
YearsAndPatentNumbers = fread(input_data['patentNumbersToYears'])
setnames(YearsAndPatentNumbers, 'Patent_Number', 'first_patent'
         )[, last_patent := shift(first_patent, type = 'lead') - 1]

# # citations_categories_and_assignees[is.na(grant_date), grant_date_hat := 
# #                      YearsAndPatentNumbers[citations_categories_and_assignees[is.na(grant_date)], grant_date, roll = T]
# #                    ]
YearsAndPatentNumbers[nrow(YearsAndPatentNumbers), last_patent := 1e9]
setkey(YearsAndPatentNumbers, first_patent, last_patent)
citations_categories_and_assignees[, patent_id_duplicate := patent_id]
setkey(citations_categories_and_assignees, patent_id, patent_id_duplicate)
citations_categories_and_assignees[, c('first_patent_of_year', 'last_patent_of_year', 'grant_year') :=
                                     foverlaps(citations_categories_and_assignees, YearsAndPatentNumbers
                                               )[, .(first_patent, last_patent, Year)]
                                   ]
citations_categories_and_assignees[, patent_pct_of_year := (patent_id - first_patent_of_year) /
                                     (last_patent_of_year - first_patent_of_year)]

jan01 = function(x) {
  as.Date(fast_strptime(paste0(as.character(x), '-01-01'), "%Y-%m-%d"))
}
dec31 = function(x) {
  as.Date(fast_strptime(paste0(as.character(x), '-12-31'), "%Y-%m-%d"))
}

grant_date_by_patent_number_mod = lm(as.numeric(grant_date - jan01(year(grant_date))) ~ patent_pct_of_year, 
                                     data = citations_categories_and_assignees)
citations_categories_and_assignees[is.na(grant_date), grant_date := grant_date_by_midpoint_of_patent_ids]
citations_categories_and_assignees[is.na(grant_date), grant_date := pmax(jan01(grant_year), 
                                                                         pmin(dec31(grant_year)
                                                                           , predict(grant_date_by_patent_number_mod, 
                                                                                    newdata = citations_categories_and_assignees[is.na(grant_date)]
                                                                                    )
                                                                           )
                                                                         )]


# citations_categories_and_assignees = howsthis[, grant_date := as.Date(fast_strptime(paste0(as.character(Year), '-07-01'), "%Y-%m-%d"))]
# citations_categories_and_assignees[, pred_diff := predict(app_date_mod)]
citations_categories_and_assignees[is.na(app_date), app_date := grant_date -
                                     predict(app_date_mod, newdata = citations_categories_and_assignees[is.na(app_date)])]


Autor_patent_match[citations_categories_and_assignees, on = 'patent_id', 
                   `:=`(org_norm_name = i.org_norm_name, 
                        org_name = i.org_name, 
                        citation_weighted = i.citation_weighted, 
                        software = i.software, 
                        app_date = i.app_date)]

Compustat_dt = read_feather_dt(input_data['CompustatRawData'])
Autor_patent_match[, GlobalCompanyKey := as.numeric(gvkey)]
setkey(Compustat_dt, GlobalCompanyKey)
setkey(Autor_patent_match, GlobalCompanyKey)
Autor_patent_match[Compustat_dt, on = 'GlobalCompanyKey', cusip := i.cusip]
Autor_patent_match[is.na(org_name) | org_name == '', org_name := org_norm_name]
Autor_patent_match[, gvkeys_per_org_norm_name := uniqueN(gvkey, na.rm = T), org_name]
Autor_firms_to_cusip = Autor_patent_match[gvkeys_per_org_norm_name == 1 &!is.na(gvkey), 
                                          .(org_name, org_norm_name, cusip, gvkey)]
Autor_firms_to_cusip = unique(Autor_firms_to_cusip[, .(org_name, cusip, gvkey)], 
                              by = c('org_name', 'gvkey'))
setkey(citations_categories_and_assignees, org_name)
setkey(Autor_firms_to_cusip, org_name)
match_including_missed_patents_by_same_firms = merge(citations_categories_and_assignees, 
                                                     Autor_firms_to_cusip, 
                                                     on = 'org_name', 
                                                     all.x = F, 
                                                     all.y = F)


match_missed_patents_by_same_firms = match_including_missed_patents_by_same_firms[!Autor_patent_match, on = 'patent_id']
all_Autor_matches = rbind(Autor_patent_match, match_missed_patents_by_same_firms, fill = T)
all_Autor_matches[, cusip6 := substr(cusip, 1, 6)
                ][is.na(software), software := mean(software), cusip6
                ][is.na(citation_weighted), citation_weighted := mean(citation_weighted), cusip6
                ][is.na(software), software := mean(software)
                ][is.na(citation_weighted), citation_weighted := mean(citation_weighted)]
all_Autor_matches[is.na(appyear), appyear := year(app_date)]


unmatched = citations_categories_and_assignees[!all_Autor_matches, on = 'patent_id']

SDC_MA_data = read_feather_dt(input_data['SDCData'])
# SDC_MA_data[all_Autor_matches, on = c(`Target CUSIP` = 'cusip6'), 
#             `:=`(patent_id = i.patent_id, 
#                  software = i.software, 
#                  app_date = i.app_date, 
#                  org_name = i.org_name)]
standardize = function(x){
  gsub('\\ ', '', #remove spaces
    gsub('\\([^)]*\\)', '', #parentheticals
      trimws(
        gsub('\\ inc$|\\ corp$|\\ corporation$|\\ incorporated$|\\ company$|\\ comp$|\\ llc$|\\ ltd$|\\ co$|\\ amalgamated$|\\ conglomerate$|\\ conglomerated$|\\ agglomerated$|\\ coop$|\\ cooperative$', '', 
          gsub('[^a-z0-9\\ ]', '', tolower(x)) #caps, punctuation, and legal control terms
        )
      )
    )
  )
}

SDC_MA_data[, target_full_standardized := standardize(`Target Full Name`)]
SDC_MA_data[, target_name_standardized := standardize(`Target Name`)]
unmatched[, org_name_standardized := standardize(org_name)]
unmatched[, org_norm_name_standardized := standardize(org_norm_name)]
setkey(unmatched, org_name_standardized)
setindex(unmatched, org_norm_name_standardized)
setkey(SDC_MA_data, target_full_standardized)
setindex(SDC_MA_data, target_name_standardized)

unmatched[SDC_MA_data, on = c(org_norm_name_standardized = 'target_name_standardized'), 
          `:=`(`Date Announced` = `i.Date Announced`, 
               `Acquiror Ultimate Parent CUSIP` = `i.Acquiror Ultimate Parent CUSIP`, 
               `Target CUSIP` = `i.Target CUSIP`)]
unmatched[SDC_MA_data, on = c(org_norm_name_standardized = 'target_full_standardized'), 
          `:=`(`Date Announced` = `i.Date Announced`, 
               `Acquiror Ultimate Parent CUSIP` = `i.Acquiror Ultimate Parent CUSIP`, 
               `Target CUSIP` = `i.Target CUSIP`)]
unmatched[SDC_MA_data, on = c(org_name_standardized = 'target_name_standardized'), 
          `:=`(`Date Announced` = `i.Date Announced`, 
               `Acquiror Ultimate Parent CUSIP` = `i.Acquiror Ultimate Parent CUSIP`, 
               `Target CUSIP` = `i.Target CUSIP`)]
unmatched[SDC_MA_data, on = c(org_name_standardized = 'target_full_standardized'), 
          `:=`(`Date Announced` = `i.Date Announced`, 
               `Acquiror Ultimate Parent CUSIP` = `i.Acquiror Ultimate Parent CUSIP`, 
               `Target CUSIP` = `i.Target CUSIP`)]
unmatched[, cusip6 := `Target CUSIP`]
unmatched[, appyear := year(app_date)]

# all_Autor_matches[SDC_MA_data, on = c(cusip6 = 'Target CUSIP'), 
#                   `:=`(`Date Announced` = `i.Date Announced`, 
#                        `Acquiror Ultimate Parent CUSIP` = `i.Acquiror Ultimate Parent CUSIP`, 
#                        `Target CUSIP` = `i.Target CUSIP`)]
all_matches = rbind(all_Autor_matches[!is.na(cusip6)], unmatched[!is.na(cusip6)], fill = T)

patent_ownership_by_year = all_matches[, .(years_active = seq(0, 19)), .(appyear, citation_weighted, software, cusip6, patent_id)
                                     ][, possession_year := appyear + years_active]
depreciation_schedule = data.table(years_active = 0:19
                                    )[, remaining_value_relative_to_previous_year := c(1, rep(0.82, times = 2), rep(0.974, times = 17))
                                    ][, remaining_value := cumprod(remaining_value_relative_to_previous_year)]
patent_ownership_by_year[depreciation_schedule, on = 'years_active', remaining_value := i.remaining_value]
patent_ownership_by_year[, owner := cusip6][, owner0 := owner]
# patent_ownership_by_year[, possession_date := dec31(possession_year)]

SDC_MA_data_slim = SDC_MA_data[, .(`Target CUSIP`, `Acquiror CUSIP`, `Acquiror Immediate Parent CUSIP`, 
                                   `Acquiror Ultimate Parent CUSIP`, `Date Announced`)]
SDC_MA_data_slim[, 'Year Announced' := year(`Date Announced`)]

might_change_hands = patent_ownership_by_year[cusip6 %in% SDC_MA_data$`Target CUSIP`][, possession_date := dec31(possession_year)]
setkey(SDC_MA_data_slim, `Target CUSIP`, `Date Announced`)
setkey(might_change_hands, cusip6, possession_date)
changed_hands_in_merger = SDC_MA_data_slim[might_change_hands, 
                                           .(`Date Announced` = `x.Date Announced` + 1, `Acquiror CUSIP`, `Acquiror Immediate Parent CUSIP`, 
                                             `Acquiror Ultimate Parent CUSIP`, `Year Announced`, 
                                             patent_id, possession_year, possession_date), 
                                           roll = T] #on = .(cusip6 = `Target CUSIP`, possession_year <= `Year Announced`), 
#!!!!!!!!!!!!! non_unique !!!!!!!!!!!!! Why is it matching every forward observation instead of the next?!

changed_hands_in_merger = changed_hands_in_merger[!is.na(`Year Announced`)]
changed_hands_in_merger[, owner := `Acquiror Ultimate Parent CUSIP`]

setkey(changed_hands_in_merger, `Acquiror CUSIP`, `Date Announced`)
changed_hands_in_merger[, c('little_firm_next_acquired', 'little_firm_acquiror', 'little_firm_acquiror_im_parent', 'little_firm_acquiror_ult_parent') :=
                          SDC_MA_data_slim[changed_hands_in_merger, .(`x.Date Announced`,  `Acquiror CUSIP`, 
                                                                      `Acquiror Immediate Parent CUSIP`, `Acquiror Ultimate Parent CUSIP`), 
                                           roll = -Inf]]
setkey(changed_hands_in_merger, `Acquiror Immediate Parent CUSIP`, `Date Announced`)
changed_hands_in_merger[, c('med_firm_next_acquired', 'med_firm_acquiror', 'med_firm_acquiror_im_parent', 'med_firm_acquiror_ult_parent') :=
                          SDC_MA_data_slim[changed_hands_in_merger, .(`x.Date Announced`,  `Acquiror CUSIP`, 
                                                                      `Acquiror Immediate Parent CUSIP`, `Acquiror Ultimate Parent CUSIP`), 
                                           roll = -Inf]]
setkey(changed_hands_in_merger, `Acquiror Ultimate Parent CUSIP`, `Date Announced`)
changed_hands_in_merger[, c('big_firm_next_acquired', 'big_firm_acquiror', 'big_firm_acquiror_im_parent', 'big_firm_acquiror_ult_parent') :=
                          SDC_MA_data_slim[changed_hands_in_merger, .(`x.Date Announced`,  `Acquiror CUSIP`, 
                                                                      `Acquiror Immediate Parent CUSIP`, `Acquiror Ultimate Parent CUSIP`), 
                                           roll = -Inf]]
# changed_hands_in_merger[, c('big_firm_next_acquired', 'big_firm_new_owner') := SDC_MA_data_slim[changed_hands_in_merger, .(`x.Date Announced`, `Acquiror Ultimate Parent CUSIP`), roll = -Inf]]

changed_hands_in_merger[!is.na(little_firm_next_acquired) & year(little_firm_next_acquired) <= possession_year & !(!is.na(med_firm_next_acquired) & little_firm_next_acquired > med_firm_next_acquired) & !(!is.na(big_firm_next_acquired) & little_firm_next_acquired > big_firm_next_acquired), 
                        `:=`(active_acq = little_firm_acquiror, active_acq_im_parent = little_firm_acquiror_im_parent, active_acq_ult_parent = little_firm_acquiror_ult_parent, 
                             additional_active_acq = `Acquiror CUSIP`, 
                             owner = little_firm_acquiror_ult_parent, `Date Announced` = little_firm_next_acquired + 1)]

changed_hands_in_merger[!is.na(med_firm_next_acquired) & year(med_firm_next_acquired) <= possession_year & !(!is.na(little_firm_next_acquired) & med_firm_next_acquired >= little_firm_next_acquired) & !(!is.na(big_firm_next_acquired) & med_firm_next_acquired > big_firm_next_acquired), 
                        `:=`(active_acq = med_firm_acquiror, active_acq_im_parent = med_firm_acquiror_im_parent, active_acq_ult_parent = med_firm_acquiror_ult_parent, 
                             additional_active_acq = `Acquiror CUSIP`, additional_active_acq_im_parent = `Acquiror Immediate Parent CUSIP`, 
                             owner = med_firm_acquiror_ult_parent, `Date Announced` = med_firm_next_acquired + 1)]

changed_hands_in_merger[!is.na(big_firm_next_acquired) & year(big_firm_next_acquired) <= possession_year & !(!is.na(little_firm_next_acquired) & big_firm_next_acquired >= little_firm_next_acquired) & !(!is.na(med_firm_next_acquired) & big_firm_next_acquired <= med_firm_next_acquired), 
                        `:=`(active_acq = big_firm_acquiror, active_acq_im_parent = big_firm_acquiror_im_parent, active_acq_ult_parent = big_firm_acquiror_ult_parent, 
                             additional_active_acq = `Acquiror CUSIP`, additional_active_acq_im_parent = `Acquiror Immediate Parent CUSIP`, additional_active_acq_ult_parent = `Acquiror Ultimate Parent CUSIP`, 
                             owner = big_firm_acquiror_ult_parent, `Date Announced` = big_firm_next_acquired + 1)]

setkey(changed_hands_in_merger, active_acq, `Date Announced`)
changed_hands_in_merger[, c('little_firm_next_acquired', 'little_firm_acquiror', 'little_firm_acquiror_im_parent', 'little_firm_acquiror_ult_parent') :=
                          SDC_MA_data_slim[changed_hands_in_merger, .(`x.Date Announced`,  `Acquiror CUSIP`, 
                                                                      `Acquiror Immediate Parent CUSIP`, `Acquiror Ultimate Parent CUSIP`), 
                                           roll = -Inf]]
setkey(changed_hands_in_merger, active_acq_im_parent, `Date Announced`)
changed_hands_in_merger[, c('med_firm_next_acquired', 'med_firm_acquiror', 'med_firm_acquiror_im_parent', 'med_firm_acquiror_ult_parent') :=
                          SDC_MA_data_slim[changed_hands_in_merger, .(`x.Date Announced`,  `Acquiror CUSIP`, 
                                                                      `Acquiror Immediate Parent CUSIP`, `Acquiror Ultimate Parent CUSIP`), 
                                           roll = -Inf]]
setkey(changed_hands_in_merger, active_acq_ult_parent, `Date Announced`)
changed_hands_in_merger[, c('big_firm_next_acquired', 'big_firm_acquiror', 'big_firm_acquiror_im_parent', 'big_firm_acquiror_ult_parent') :=
                          SDC_MA_data_slim[changed_hands_in_merger, .(`x.Date Announced`,  `Acquiror CUSIP`, 
                                                                      `Acquiror Immediate Parent CUSIP`, `Acquiror Ultimate Parent CUSIP`), 
                                           roll = -Inf]]
setkey(changed_hands_in_merger, additional_active_acq, `Date Announced`)
changed_hands_in_merger[, c('little2_firm_next_acquired', 'little2_firm_acquiror', 'little2_firm_acquiror_im_parent', 'little2_firm_acquiror_ult_parent') :=
                          SDC_MA_data_slim[changed_hands_in_merger, .(`x.Date Announced`,  `Acquiror CUSIP`, 
                                                                      `Acquiror Immediate Parent CUSIP`, `Acquiror Ultimate Parent CUSIP`), 
                                           roll = -Inf]]
setkey(changed_hands_in_merger, additional_active_acq_im_parent, `Date Announced`)
changed_hands_in_merger[, c('med2_firm_next_acquired', 'med2_firm_acquiror', 'med2_firm_acquiror_im_parent', 'med2_firm_acquiror_ult_parent') :=
                          SDC_MA_data_slim[changed_hands_in_merger, .(`x.Date Announced`,  `Acquiror CUSIP`, 
                                                                      `Acquiror Immediate Parent CUSIP`, `Acquiror Ultimate Parent CUSIP`), 
                                           roll = -Inf]]
setkey(changed_hands_in_merger, additional_active_acq_ult_parent, `Date Announced`)
changed_hands_in_merger[, c('big2_firm_next_acquired', 'big2_firm_acquiror', 'big2_firm_acquiror_im_parent', 'big2_firm_acquiror_ult_parent') :=
                          SDC_MA_data_slim[changed_hands_in_merger, .(`x.Date Announced`,  `Acquiror CUSIP`, 
                                                                      `Acquiror Immediate Parent CUSIP`, `Acquiror Ultimate Parent CUSIP`), 
                                           roll = -Inf]]
changed_hands_in_merger[!is.na(little_firm_next_acquired) & year(little_firm_next_acquired) <= possession_year & !(!is.na(med_firm_next_acquired) & little_firm_next_acquired > med_firm_next_acquired) & !(!is.na(big_firm_next_acquired) & little_firm_next_acquired > big_firm_next_acquired)
                        & !(!is.na(little2_firm_next_acquired) & little_firm_next_acquired > little2_firm_next_acquired) & !(!is.na(med2_firm_next_acquired) & little_firm_next_acquired > med2_firm_next_acquired) & !(!is.na(big2_firm_next_acquired) & little_firm_next_acquired > big2_firm_next_acquired), 
                        `:=`(active_acq = little_firm_acquiror, active_acq_im_parent = little_firm_acquiror_im_parent, active_acq_ult_parent = little_firm_acquiror_ult_parent, 
                             additional_active_acq = `Acquiror CUSIP`, 
                             owner = little_firm_acquiror_ult_parent, `Date Announced` = little_firm_next_acquired + 1)]

changed_hands_in_merger[!is.na(med_firm_next_acquired) & year(med_firm_next_acquired) <= possession_year & !(!is.na(little_firm_next_acquired) & med_firm_next_acquired >= little_firm_next_acquired) & !(!is.na(big_firm_next_acquired) & med_firm_next_acquired > big_firm_next_acquired)
                        & !(!is.na(little2_firm_next_acquired) & med_firm_next_acquired >= little2_firm_next_acquired) & !(!is.na(med2_firm_next_acquired) & med_firm_next_acquired > med2_firm_next_acquired) & !(!is.na(big2_firm_next_acquired) & med_firm_next_acquired > big2_firm_next_acquired), 
                        `:=`(active_acq = med_firm_acquiror, active_acq_im_parent = med_firm_acquiror_im_parent, active_acq_ult_parent = med_firm_acquiror_ult_parent, 
                             additional_active_acq = `Acquiror CUSIP`, additional_active_acq_im_parent = `Acquiror Immediate Parent CUSIP`, 
                             owner = med_firm_acquiror_ult_parent, `Date Announced` = med_firm_next_acquired + 1)]

changed_hands_in_merger[!is.na(big_firm_next_acquired) & year(big_firm_next_acquired) <= possession_year & !(!is.na(little_firm_next_acquired) & big_firm_next_acquired >= little_firm_next_acquired) & !(!is.na(med_firm_next_acquired) & big_firm_next_acquired >= med_firm_next_acquired)
                        &  !(!is.na(little2_firm_next_acquired) & big_firm_next_acquired >= little2_firm_next_acquired) & !(!is.na(med2_firm_next_acquired) & big_firm_next_acquired >= med2_firm_next_acquired) & !(!is.na(big2_firm_next_acquired) & big_firm_next_acquired > big2_firm_next_acquired), 
                        `:=`(active_acq = big_firm_acquiror, active_acq_im_parent = big_firm_acquiror_im_parent, active_acq_ult_parent = big_firm_acquiror_ult_parent, 
                             additional_active_acq = `Acquiror CUSIP`, additional_active_acq_im_parent = `Acquiror Immediate Parent CUSIP`, 
                             owner = big_firm_acquiror_ult_parent, `Date Announced` = big_firm_next_acquired + 1)]

# 
# 
#                          `:=`(`Acquiror CUSIP` = `i.Acquiror CUSIP`, 
#                               `Acquiror Immediate Parent CUSIP` = `i.Acquiror Immediate Parent CUSIP`, 
#                               `Acquiror Ultimate Parent CUSIP` = `i.Acquiror Ultimate Parent CUSIP`, 
#                               `Year Announced` = `i.Year Announced`, 
#                               `Date Announced` = `i.Date Announced`)]
# patent_ownership_by_year[, old_year_announced := `Year Announced`][, old_date_announced := `Date Announced`][, owner1 := owner]
# 
# 
# patent_ownership_by_year[SDC_MA_data, on = .(cusip6 = `Target CUSIP`, possession_year <= `Year Announced`), 
#                          `:=`(`Acquiror CUSIP` = `i.Acquiror CUSIP`, 
#                               `Acquiror Immediate Parent CUSIP` = `i.Acquiror Immediate Parent CUSIP`, 
#                               `Acquiror Ultimate Parent CUSIP` = `i.Acquiror Ultimate Parent CUSIP`, 
#                               `Year Announced` = `i.Year Announced`, 
#                               `Date Announced` = `i.Date Announced`)]
# 
# 
# patent_ownership_by_year[SDC_MA_data, on = .(`Acquiror CUSIP` = `Target CUSIP`, `Date Announced` <= `Date Announced`), 
#                          `:=`(`Acquiror CUSIP` = `i.Acquiror CUSIP`, 
#                               `Acquiror Immediate Parent CUSIP` = `i.Acquiror Immediate Parent CUSIP`, 
#                               `Acquiror Ultimate Parent CUSIP` = `i.Acquiror Ultimate Parent CUSIP`, 
#                               `Year Announced` = `i.Year Announced`, 
#                               `Date Announced` = `i.Date Announced`)]
# patent_ownership_by_year[`Year Announced`<=possession_year, owner := `Acquiror Ultimate Parent CUSIP`]
# patent_ownership_by_year[, owner2 := owner]
# 
# 
# patent_ownership_by_year[SDC_MA_data, on = c(cusip6 = 'Target CUSIP'), 
#     `:=`(`Acquiror CUSIP` = `i.Acquiror CUSIP`, 
#          `Acquiror Immediate Parent CUSIP` = `i.Acquiror Immediate Parent CUSIP`, 
#          `Acquiror Ultimate Parent CUSIP` = `i.Acquiror Ultimate Parent CUSIP`, 
#          `Year Announced` = `i.Year Announced`)]
# patent_ownership_by_year[`Year Announced`<=possession_year, owner := `Acquiror Ultimate Parent CUSIP`]
# patent_ownership_by_year[, old_year_announced := `Year Announced`][, owner1 := owner]
# 
# patent_ownership_by_year[SDC_MA_data, on = c(`Acquiror CUSIP` = 'Target CUSIP'), 
#     `:=`(`Acquiror CUSIP` = `i.Acquiror CUSIP`, 
#          `Acquiror Immediate Parent CUSIP` = `i.Acquiror Immediate Parent CUSIP`, 
#          `Acquiror Ultimate Parent CUSIP` = `i.Acquiror Ultimate Parent CUSIP`, 
#          `Year Announced` = `i.Year Announced`)]
# patent_ownership_by_year[`Year Announced`<=possession_year, owner := `Acquiror Ultimate Parent CUSIP`]
# patent_ownership_by_year[, owner2 := owner]
# 
# patent_ownership_by_year[SDC_MA_data, on = c(`Acquiror Immediate Parent CUSIP` = 'Target CUSIP'), 
#     `:=`(`Acquiror CUSIP` = `i.Acquiror CUSIP`, 
#          `Acquiror Immediate Parent CUSIP` = `i.Acquiror Immediate Parent CUSIP`, 
#          `Acquiror Ultimate Parent CUSIP` = `i.Acquiror Ultimate Parent CUSIP`, 
#          `Year Announced` = `i.Year Announced`)]
# patent_ownership_by_year[`Year Announced`<=possession_year, owner := `Acquiror Ultimate Parent CUSIP`]
# patent_ownership_by_year[, owner3 := owner]
# 
# patent_ownership_by_year[SDC_MA_data, on = c(`Acquiror Ultimate Parent CUSIP` = 'Target CUSIP'), 
#     `:=`(`Acquiror CUSIP` = `i.Acquiror CUSIP`, 
#          `Acquiror Immediate Parent CUSIP` = `i.Acquiror Immediate Parent CUSIP`, 
#          `Acquiror Ultimate Parent CUSIP` = `i.Acquiror Ultimate Parent CUSIP`, 
#          `Year Announced` = `i.Year Announced`)]
# patent_ownership_by_year[`Year Announced`<=possession_year, owner := `Acquiror Ultimate Parent CUSIP`]
# patent_ownership_by_year[, owner4 := owner]
# 
# 
# patent_ownership_by_year[SDC_MA_data, on = c(`Acquiror CUSIP` = 'Target CUSIP'), 
#                          `:=`(`Acquiror CUSIP` = `i.Acquiror CUSIP`, 
#                               `Acquiror Immediate Parent CUSIP` = `i.Acquiror Immediate Parent CUSIP`, 
#                               `Acquiror Ultimate Parent CUSIP` = `i.Acquiror Ultimate Parent CUSIP`, 
#                               `Year Announced` = `i.Year Announced`)]
# patent_ownership_by_year[`Year Announced`<=possession_year, owner := `Acquiror Ultimate Parent CUSIP`]
# patent_ownership_by_year[, owner5 := owner]
# 

patent_ownership_by_year[changed_hands_in_merger, on = 'patent_id', owner := i.owner]

# patent_ownership_by_year = patent_ownership_by_year[!is.na(possession_year)]
# patent_ownership_by_year[is.na(accurate_cusip), accurate_cusip := cusip6]
final = patent_ownership_by_year[, .(all_patents = sum(citation_weighted * remaining_value, na.rm = T), 
                software_patents = sum(citation_weighted * software * remaining_value, na.rm = T)), 
            .(possession_year, owner)]
# test = patent_ownership_by_year
# test[, cusip := `Acquiror Ultimate Parent CUSIP`]
# merge(test, patent_ownership_by_year, all.x = T, all.y = T, by = 'cusip')

write_feather(final, output_files['patentDataForAnalysis'])















# # Autor_patent_match[citation_weighted_software, on = 'patent_id', citation_weighted := citation_weighted]
# firm_annual_patents = Autor_patent_match[, .(patents = sum(citation_weighted)), .(gvkey, appyear)]
# panel = data.table(merge(1950:2020, unique(firm_annual_patents$gvkey), allow.cartesian = T))
# setnames(panel, c('appyear', 'gvkey'))
# setkey(panel, gvkey, appyear)
# setkey(firm_annual_patents, gvkey, appyear)
# firm_software_patenting = merge(panel, firm_annual_patents, all.x = T)
# firm_software_patenting[is.na(patents), patents := 0]
# firm_software_patenting[, patents_rolling_5 := frollsum(patents, 5, na.rm = T), by = gvkey]
# 
# unmatched = merge(citation_weighted_software, Autor_patent_match[, .(assignee_clean, patent_id)], by = 'patent_id', all.x = T, all.y = F
#                   )[is.na(assignee_clean)][, assignee_clean := NULL]
# 
# 
# 
# # mod = lm(diff~grant_date, data = assignees)
# # coef(mod)
# unmatched2 = unmatched2[!is.na(app_date)]
# setkey(unmatched2, org_norm_name)
# setkey(SDC_MA_data, `Target Name`)
# 
# 
# gsub('')
# issues = assignees[, asdf := org_name %in% SDC_MA_data$`Target Name` +
#                          org_name %in% SDC_MA_data$`Target Full Name` +
#                          org_norm_name %in% SDC_MA_data$`Target Name` +
#                          org_norm_name %in% SDC_MA_data$`Target Full Name`]
# 
# howsthis = merge(unmatched2, SDC_MA_data, all.x = T, all.y = F, by.x = 'org_norm_name', by.y = 'Target Name')
# # fwrite(firm_software_patenting, 'IntermediateFiles/firm_software_patenting.csv', quote = T)
