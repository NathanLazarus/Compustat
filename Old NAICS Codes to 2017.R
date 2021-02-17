input_data = c(dtcut = 'IntermediateFiles/dtcut.feather', 
               crosswalk97_02 = 'Data/NAICS Crosswalks/1997_NAICS_to_2002_NAICS.xls', 
               crosswalk02_07 = 'Data/NAICS Crosswalks/2002_to_2007_NAICS.xls', 
               crosswalk07_12 = 'Data/NAICS Crosswalks/2007_to_2012_NAICS.xls', 
               crosswalk12_17 = 'Data/NAICS Crosswalks/2012_to_2017_NAICS.xlsx')

output_files = c(defunct_NAICS_to_2017_crosswalk = 'IntermediateFiles/Old NAICS to 2017 Crosswalk.feather')


dtcut = read_feather_dt(input_data['dtcut'])




# Add Industry Classification Codes for Firms that have them some years but not others ----------

dtcut[, NorthAmericaIndustrialClassificationSystemHistorical :=
         dtcut[!is.na(NorthAmericaIndustrialClassificationSystemHistorical)
             ][dtcut, NorthAmericaIndustrialClassificationSystemHistorical,
               on = c('GlobalCompanyKey', 'DataYearFiscal'),
               roll = 'nearest']]

dtcut[, StandardIndustrialClassificationHistorical :=
         dtcut[!is.na(StandardIndustrialClassificationHistorical)
             ][dtcut, StandardIndustrialClassificationHistorical,
               on = c('GlobalCompanyKey', 'DataYearFiscal'),
               roll = 'nearest']]

dtcut[, NorthAmericaIndustrialClassificationSystemHistorical := as.numeric(str_pad(NorthAmericaIndustrialClassificationSystemHistorical,
                                                                                   width = 6, side = 'right', pad = '0'))]
dtcut[, StandardIndustrialClassificationHistorical := as.numeric(str_pad(StandardIndustrialClassificationHistorical,
                                                                         width = 6, side = 'right', pad = '0'))]
dtcut[, SIC := StandardIndustrialClassificationHistorical]
# dtcut[is.na(SIC), SIC := currentSIC]
dtcut[, NAICS := NorthAmericaIndustrialClassificationSystemHistorical]
# dtcut[is.na(NAICS) & is.na(StandardIndustrialClassificationHistorical), NAICS := currentNAICS]
# There are only 41 obs with SIC_historical but not NAICS_historical.
# I give SIC_historical precedence over currentNAICS in those cases.



crosswalk97_02 = data.table(read_xls(input_data['crosswalk97_02'], sheet = 2)
                          )[, `1997 NAICS Code` := as.integer(NAICS97)
                          ][, `2002 NAICS Code` := NAICS02
                          ][, splits := .N > 1, `1997 NAICS Code`
                          ][, n_targets := .N, `2002 NAICS Code`]
crosswalk02_07 = data.table(read_xls(input_data['crosswalk02_07'], skip = 2)
                          )[, splits := .N > 1, `2002 NAICS Code`
                          ][, n_targets := .N, `2007 NAICS Code`]
crosswalk07_12 = data.table(read_xls(input_data['crosswalk07_12'], skip = 2)
                          )[, splits := .N > 1, `2007 NAICS Code`
                          ][, n_targets := .N, `2012 NAICS Code`]
crosswalk12_17 = data.table(read_xlsx(input_data['crosswalk12_17'], skip = 2)
                          )[, splits := .N > 1, `2012 NAICS Code`
                          ][, n_targets := .N, `2017 NAICS Code`]


all_crosswalks_long = rbind(crosswalk97_02[, .(NAICS_start = `1997 NAICS Code`, NAICS_end = `2002 NAICS Code`,
                                               n_targets, splits, year_of_NAICS_change = 2002)],
                            crosswalk02_07[, .(NAICS_start = `2002 NAICS Code`, NAICS_end = `2007 NAICS Code`,
                                               n_targets, splits, year_of_NAICS_change = 2007)],
                            crosswalk07_12[, .(NAICS_start = `2007 NAICS Code`, NAICS_end = `2012 NAICS Code`,
                                               n_targets, splits, year_of_NAICS_change = 2012)],
                            crosswalk12_17[, .(NAICS_start = `2012 NAICS Code`, NAICS_end = `2017 NAICS Code`,
                                               n_targets, splits, year_of_NAICS_change = 2017)]
                           ) #[, `:=`(start_year = year_of_NAICS_change - 5, end_year = year_of_NAICS_change - 1)
                           # ][year_of_NAICS_change == 2002, start_year := -Inf
                           # ]

crosswalk97_02[, defined_in := fifelse(`1997 NAICS Code` %in% crosswalk97_02$`2002 NAICS Code`, 2002,
                                       fifelse(`1997 NAICS Code` %in% crosswalk02_07$`2007 NAICS Code`, 2007,
                                               fifelse(`1997 NAICS Code` %in% crosswalk07_12$`2012 NAICS Code`, 2012,
                                                       fifelse(`1997 NAICS Code` %in% crosswalk12_17$`2017 NAICS Code`, 2017, Inf))))]
crosswalk02_07[, defined_in := fifelse(`2002 NAICS Code` %in% crosswalk02_07$`2007 NAICS Code`, 2007,
                                       fifelse(`2002 NAICS Code` %in% crosswalk07_12$`2012 NAICS Code`, 2012,
                                               fifelse(`2002 NAICS Code` %in% crosswalk12_17$`2017 NAICS Code`, 2017, Inf)))]
crosswalk07_12[, defined_in := fifelse(`2007 NAICS Code` %in% crosswalk07_12$`2012 NAICS Code`, 2012,
                                       fifelse(`2007 NAICS Code` %in% crosswalk12_17$`2017 NAICS Code`, 2017, Inf))]
crosswalk12_17[, defined_in := fifelse(`2012 NAICS Code` %in% crosswalk12_17$`2017 NAICS Code`, 2017, Inf)]


actual_NAICS_in_Compustat = dtcut[, .(DataYearFiscal,
                                      NAICS_historical = NorthAmericaIndustrialClassificationSystemHistorical,
                                      GlobalCompanyKey)]

overall_and_firm_specific_crosswalks = foreach(
  code_that_splits = iter(unique(all_crosswalks_long[NAICS_start != NAICS_end,
                                                     .(code = NAICS_start,
                                                       year = year_of_NAICS_change,
                                                       applicable_beginning = applicable_beginning,
                                                       applicable_ending = applicable_ending)]),
                          by = 'row')
  ) %do% {
                
    potential_NAICS = all_crosswalks_long[NAICS_start == code_that_splits$code &
                                            year_of_NAICS_change == code_that_splits$year,
                                          .(NAICS_start, NAICS_end, n_targets, year_of_NAICS_change)]
    to_find_firms_with_codes_that_split = copy(actual_NAICS_in_Compustat)
    to_find_firms_with_codes_that_split[(DataYearFiscal == code_that_splits$year - 1 | DataYearFiscal == code_that_splits$year - 2) &
                                          NAICS_historical == code_that_splits$code,
                                        was_in_industry := 1
                                      ][, follow_these := sum(was_in_industry == 1, na.rm = T) > 0, GlobalCompanyKey
                                      ][follow_these == T & DataYearFiscal >= code_that_splits$year,
                                        found_NAICS_matching_potential := NAICS_historical %in% potential_NAICS[, NAICS_end]]
    if (nrow(to_find_firms_with_codes_that_split[found_NAICS_matching_potential == T]) == 0) {
      ratiotable = potential_NAICS
      ratiotable[, relative_weight := 1 / n_targets
               ][, ratio := relative_weight / sum(relative_weight)]
      firm_crosswalk = data.table()
    } else {
      empirical_ratios =
        to_find_firms_with_codes_that_split[found_NAICS_matching_potential == T, common_subsequent_NAICS_among_potential := .N == max(.N), .(NAICS_historical, GlobalCompanyKey)
                                          ][common_subsequent_NAICS_among_potential == T, is_modal_subsequent_NAICS := DataYearFiscal == min(DataYearFiscal), GlobalCompanyKey
                                          ][to_find_firms_with_codes_that_split[is_modal_subsequent_NAICS == T], on = 'GlobalCompanyKey', future_matching_NAICS := i.NAICS_historical
                                          ][is_modal_subsequent_NAICS == T, .(num_firms = .N), NAICS_historical
                                          ][, ratio := num_firms / sum(num_firms)]
      #I don't weight by firm size or anything because the firms missing NAICS are probably more like the small firms than the large ones
      ratiotable = potential_NAICS[empirical_ratios, on = c(NAICS_end = 'NAICS_historical'), ratio := i.ratio
                                 ][is.na(ratio), ratio := 0]
      
      firm_crosswalk = to_find_firms_with_codes_that_split[NAICS_historical == code_that_splits$code &
                                                             !is.na(future_matching_NAICS),
                                                           .(GlobalCompanyKey, DataYearFiscal, NAICS_historical, future_matching_NAICS)]

    }
    list(ratiotable[, .(NAICS_start, NAICS_end, ratio, year_of_NAICS_change)], firm_crosswalk)
  }

old_to_new_NAICS_ratios_for_codes_that_change = rbindlist(lapply(overall_and_firm_specific_crosswalks, `[[`, 1))
firm_specific_crosswalks = rbindlist(lapply(overall_and_firm_specific_crosswalks, `[[`, 2))

dtcut[firm_specific_crosswalks, on = c('GlobalCompanyKey', 'DataYearFiscal'), future_matching_NAICS := i.future_matching_NAICS]
dtcut[, NAICS_updated_once := fifelse(!is.na(future_matching_NAICS), future_matching_NAICS, NorthAmericaIndustrialClassificationSystemHistorical)]


actual_NAICS_in_Compustat = dtcut[, .(DataYearFiscal,
                                      NAICS_historical = NAICS_updated_once,
                                      GlobalCompanyKey)]

overall_and_firm_specific_crosswalks = foreach(
  code_that_splits = iter(unique(all_crosswalks_long[NAICS_start != NAICS_end,
                                                     .(code = NAICS_start,
                                                       year = year_of_NAICS_change,
                                                       applicable_beginning = applicable_beginning,
                                                       applicable_ending = applicable_ending)]),
                          by = 'row')
  ) %do% {
                
    potential_NAICS = all_crosswalks_long[NAICS_start == code_that_splits$code &
                                            year_of_NAICS_change == code_that_splits$year,
                                          .(NAICS_start, NAICS_end, n_targets, year_of_NAICS_change)]
    to_find_firms_with_codes_that_split = copy(actual_NAICS_in_Compustat)
    to_find_firms_with_codes_that_split[(DataYearFiscal == code_that_splits$year - 1 | DataYearFiscal == code_that_splits$year - 2) &
                                          NAICS_historical == code_that_splits$code,
                                        was_in_industry := 1
                                      ][, follow_these := sum(was_in_industry == 1, na.rm = T) > 0, GlobalCompanyKey
                                      ][follow_these == T & DataYearFiscal >= code_that_splits$year,
                                        found_NAICS_matching_potential := NAICS_historical %in% potential_NAICS[, NAICS_end]]
    if (nrow(to_find_firms_with_codes_that_split[found_NAICS_matching_potential == T]) == 0) {
      ratiotable = potential_NAICS
      ratiotable[, relative_weight := 1 / n_targets
               ][, ratio := relative_weight / sum(relative_weight)]
      firm_crosswalk = data.table()
    } else {
      empirical_ratios =
        to_find_firms_with_codes_that_split[found_NAICS_matching_potential == T, common_subsequent_NAICS_among_potential := .N == max(.N), .(NAICS_historical, GlobalCompanyKey)
                                          ][common_subsequent_NAICS_among_potential == T, is_modal_subsequent_NAICS := DataYearFiscal == min(DataYearFiscal), GlobalCompanyKey
                                          ][to_find_firms_with_codes_that_split[is_modal_subsequent_NAICS == T], on = 'GlobalCompanyKey', future_matching_NAICS := i.NAICS_historical
                                          ][is_modal_subsequent_NAICS == T, .(num_firms = .N), NAICS_historical
                                          ][, ratio := num_firms / sum(num_firms)]
      #I don't weight by firm size or anything because the firms missing NAICS are probably more like the small firms than the large ones
      ratiotable = potential_NAICS[empirical_ratios, on = c(NAICS_end = 'NAICS_historical'), ratio := i.ratio
                                 ][is.na(ratio), ratio := 0]
      
      firm_crosswalk = to_find_firms_with_codes_that_split[NAICS_historical == code_that_splits$code &
                                                             !is.na(future_matching_NAICS),
                                                           .(GlobalCompanyKey, DataYearFiscal, NAICS_historical, future_matching_NAICS)]

    }
    list(ratiotable[, .(NAICS_start, NAICS_end, ratio, year_of_NAICS_change)], firm_crosswalk)
  }

throwaway = rbindlist(lapply(overall_and_firm_specific_crosswalks, `[[`, 1))
firm_specific_crosswalks = rbindlist(lapply(overall_and_firm_specific_crosswalks, `[[`, 2))

dtcut[, future_matching_NAICS := NULL][firm_specific_crosswalks, on = c('GlobalCompanyKey', 'DataYearFiscal'), future_matching_NAICS := i.future_matching_NAICS]
dtcut[, NAICS_updated_twice := fifelse(!is.na(future_matching_NAICS), future_matching_NAICS, NAICS_updated_once)]



dtcut[NAICS_updated_twice != NAICS_updated_once]



















# Now I want every NAICS_start code x applicable year pair to be full,
# except for NAICS_start for NAICS codes that are valid in 2017 or later

na_or_less_to_2016 = function(otherYear, thisYear) fifelse(otherYear < thisYear, 2016, otherYear, na = 2016)
na_or_greater_to_negInf = function(otherYear, thisYear) fifelse(otherYear > thisYear, -Inf, otherYear, na = -Inf)


roll_end_year_forward = function(start, end) pmin(na_or_less_to_2016(start[1] - 1, end),
                                                  na_or_less_to_2016(start[2] - 1, end),
                                                  na_or_less_to_2016(start[3] - 1, end),
                                                  na_or_less_to_2016(start[4] - 1, end))

roll_start_year_back = function(start, end) pmax(na_or_greater_to_negInf(end[1] + 1, start),
                                                 na_or_greater_to_negInf(end[2] + 1, start),
                                                 na_or_greater_to_negInf(end[3] + 1, start),
                                                 na_or_greater_to_negInf(end[4] + 1, start))

crosswalk_applicable_dates = unique(all_crosswalks_long[, .(NAICS_start, start_year, end_year)])
crosswalk_applicable_dates[, applicable_beginning := roll_start_year_back(start_year, end_year), NAICS_start]
crosswalk_applicable_dates[, applicable_ending := roll_end_year_forward(start_year, end_year), NAICS_start]
crosswalk_applicable_dates[applicable_ending == 2016 & !NAICS_start %in% defunct_NAICS_to_2017_crosswalk[, NAICS_end],
                           applicable_ending := Inf]

all_crosswalks_long[crosswalk_applicable_dates,
                    on = c('NAICS_start', 'start_year', 'end_year'),
                    `:=`(applicable_beginning = i.applicable_beginning,
                         applicable_ending = i.applicable_ending)
                  ][, c('start_year', 'end_year') := NULL]






all_crosswalks_long[old_to_new_NAICS_ratios_for_codes_that_change, on = c('NAICS_start', 'NAICS_end', 'year_of_NAICS_change'), ratio := i.ratio]
all_crosswalks_long[is.na(ratio), ratio := 1]

crosswalk97_17 = crosswalk97_02[crosswalk02_07, on = '2002 NAICS Code'
                              ][crosswalk07_12, on = '2007 NAICS Code'
                              ][crosswalk12_17, on = '2012 NAICS Code']
setnames(crosswalk97_17, gsub("Title.*", "Title", names(crosswalk97_17)))

crosswalk97_17[all_crosswalks_long[year_of_NAICS_change == 2002], on = c(`1997 NAICS Code` = 'NAICS_start', `2002 NAICS Code` = 'NAICS_end'), ratio97_02 := i.ratio
             ][all_crosswalks_long[year_of_NAICS_change == 2007], on = c(`2002 NAICS Code` = 'NAICS_start', `2007 NAICS Code` = 'NAICS_end'), ratio02_07 := i.ratio
             ][all_crosswalks_long[year_of_NAICS_change == 2012], on = c(`2007 NAICS Code` = 'NAICS_start', `2012 NAICS Code` = 'NAICS_end'), ratio07_12 := i.ratio
             ][all_crosswalks_long[year_of_NAICS_change == 2017], on = c(`2012 NAICS Code` = 'NAICS_start', `2017 NAICS Code` = 'NAICS_end'), ratio12_17 := i.ratio
             ][, ratio97_17 := ratio97_02 * ratio02_07 * ratio07_12 * ratio12_17
             ][, ratio02_17 := ratio02_07 * ratio07_12 * ratio12_17
             ][, ratio07_17 := ratio07_12 * ratio12_17]



defunct_NAICS_to_2017_crosswalk = rbind(unique(crosswalk97_17[, .(NAICS_start = `1997 NAICS Code`, NAICS_end = `2017 NAICS Code`,
                                                                  ratio = ratio97_17, year_of_NAICS_change = 2002)]),
                                        unique(crosswalk97_17[, .(NAICS_start = `2002 NAICS Code`, NAICS_end = `2017 NAICS Code`,
                                                                  ratio = ratio02_17, year_of_NAICS_change = 2007)]),
                                        unique(crosswalk97_17[, .(NAICS_start = `2007 NAICS Code`, NAICS_end = `2017 NAICS Code`,
                                                                  ratio = ratio07_17, year_of_NAICS_change = 2012)]),
                                        unique(crosswalk97_17[, .(NAICS_start = `2012 NAICS Code`, NAICS_end = `2017 NAICS Code`,
                                                                  ratio = ratio12_17, year_of_NAICS_change = 2017)])
                                      )[, `:=`(start_year = year_of_NAICS_change - 5, end_year = year_of_NAICS_change - 1)
                                      ][year_of_NAICS_change == 2002, start_year := -Inf
                                      ][ratio > 0]

defunct_NAICS_to_2017_crosswalk[crosswalk_applicable_dates,
                                on = c('NAICS_start', 'start_year', 'end_year'),
                                `:=`(applicable_beginning = i.applicable_beginning,
                                     applicable_ending = i.applicable_ending)
                              ][, c('start_year', 'end_year') := NULL]

# write_feather(defunct_NAICS_to_2017_crosswalk, output_files['defunct_NAICS_to_2017_crosswalk'])
# 
# defunct_NAICS_to_2017_crosswalk = read_feather_dt(input_data['defunct_NAICS_to_2017_crosswalk'])




dtcut[!is.na(future_matching_NAICS), NAICS := future_matching_NAICS]
dtcut[, duplicate_fiscal_year1 := DataYearFiscal][, duplicate_fiscal_year2 := DataYearFiscal]
setkey(dtcut, NAICS, duplicate_fiscal_year1, duplicate_fiscal_year2)
setkey(defunct_NAICS_to_2017_crosswalk, NAICS_start, applicable_beginning, applicable_ending)

whathappenshere = foverlaps(dtcut, defunct_NAICS_to_2017_crosswalk)
#gotta do it for header data that included defunct NAICS