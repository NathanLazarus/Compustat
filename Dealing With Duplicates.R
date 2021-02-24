max_year = 2019
#check for ETFs

# Make it so there is one unique observation for every calendar year ----------------------
#
#firms choose the date at which their fiscal year ends, and they file their annual report, and unfortunately they sometimes change these dates
#two things can happen: they can change from reporting in, say, November to reporting in, say, March, in which case there's a missing calendar year
#(November 2006 for FY 2006 and March 2008 for FY 2007 lead to no observations in 2007).
#Or, if they go from March to November, they have two reports in one calendar year
dtcut[, haspreviousfiscalyear := (DataYearFiscal - 1) %in% DataYearFiscal, GlobalCompanyKey]
dtcut[, hascalendaryear := DataYearFiscal %in% calendaryear, GlobalCompanyKey]
dtcut[, missing := haspreviousfiscalyear & !hascalendaryear]
missings = dtcut[missing == T]
missings[, calendaryear := DataYearFiscal]
dtcut = rbind(dtcut, missings)

setkey(dtcut, GlobalCompanyKey, calendaryear)
dtcut[, n_obs_calendaryear := .N, .(calendaryear, GlobalCompanyKey)]
dtcut[n_obs_calendaryear > 1, `:=`(first_obs = datadate == min(datadate),
                                   last_obs = datadate == max(datadate)), .(GlobalCompanyKey, calendaryear)]

most_recent_available_data_for_companies_that_appear_twice_in_a_calendar_year =
   merge_and_reconcile(dtcut[last_obs == T], dtcut[first_obs == T],
                    join_cols = c('GlobalCompanyKey', 'calendaryear'))
# it's possible this could create a weird balance sheet, like if sales in December are 5 and COGS are NA,
# and sales in March are 2000 and COGS are 1500, then we'd see sales = 5 and COGS = 1500 after merge_and_reconcile

dtcut = rbind(dtcut[n_obs_calendaryear == 1],
              most_recent_available_data_for_companies_that_appear_twice_in_a_calendar_year)

dtcut[, c('haspreviousfiscalyear', 'hascalendaryear', 'missing', 'n_obs_calendaryear') := NULL]

dtcut = dtcut[calendaryear <= max_year]

# Duplicates -------------------
# dtcut_without_utilities = dtcut[SIC %/% 100 == 49 & 
#                                   as.numeric(str_pad(as.character(NAICS), width = 6, side = 'right', pad = '0')) %/% 1000 != 221]

#remove duplicates:


# duplicate_checking_data = merge(
#   dtcut_without_utilities,
#   companyData[, .SD, .SDcols = !c('conm', 'costat', 'CurrentISOCountryCodeHeadquarters',
#                                   'ResearchCoReasonforDeletion', 'ResearchCompanyDeletionDate',
#                                    'CurrentPrimaryIssueTagCanada', 'PrimaryIssueTagRestofWorld',
#                                    'CurrentPrimaryIssueTagUS', 'InternationalDomesticBothIndicator',
#                                    'CurrentFiscalYearEndMonth')],
#   by.x = 'GlobalCompanyKey',
#   by.y = 'gvkey')[, .SD, .SDcols = !c('curcd', 'currentSIC', 'currentNAICS', 'SIC', 'NAICS', 'DataYearFiscal',
#                                       'indfmt', 'consol', 'popsrc', 'datafmt', 'tic', 'cusip', 'cusip6',
#                                       'laborStrength', 'laborConcern', 'laborRelations', 'Anticompetitive Practices',
#                                       'MktVal', 'preferred')]
# duplicate_checking_data[, `:=`(datadate = as.numeric(as.Date(datadate)),
#                                ipodate = as.numeric(as.Date(ipodate)))]
# duplicate_checking_data[, `:=`(cik = as.numeric(cik),
#                                GICGroups = as.numeric(GICGroups),
#                                GICIndustries = as.numeric(GICIndustries),
#                                GICSectors = as.numeric(GICSectors),
#                                GICSubIndustries = as.numeric(GICSubIndustries),
#                                PostalCode = as.numeric(gsub('-| ', '', PostalCode)),
#                                PhoneNumber = as.numeric(gsub('-| |\\(|\\)', '', PhoneNumber)),
#                                FaxNumber = as.numeric(gsub('-| |\\(|\\)', '', FaxNumber)),
#                                EmployerIdentificationNumber = as.numeric(gsub('-| ', '', EmployerIdentificationNumber)))]
#
# clusters = makeCluster(7)
# registerDoSNOW(clusters)
# 
# potential_duplicates = foreach(yr = unique(dtcut$calendaryear),
#                                .combine = rbind_and_fill,
#                                .multicombine = T,
#                                .packages = 'foreach'
#                                )%dopar%{
# 
#   checking_for_duplicates = duplicate_checking_data[calendaryear == yr]
#   checking_for_duplicates_numeric_cols = checking_for_duplicates[, .SD, .SDcols = is.numeric
#                                                                ][, .SD,  .SDcols = !c('exchg', 'EarningsPerShareDilutedIncludingExtraordinaryItems', 'calendaryear')]
#   n_firms = nrow(checking_for_duplicates)
#   numeric_cols_scores = foreach(i = 1:ncol(checking_for_duplicates_numeric_cols), .combine = `+`) %do% {
#     if(names(checking_for_duplicates_numeric_cols)[i] %in%
#                c('datadate', 'fyr', 'StandardIndustryClassificationCode', 'NorthAmericanIndustryClassificationCode',
#                  'StandardIndustrialClassificationHistorical', 'NorthAmericaIndustrialClassificationSystemHistorical',
#                  'GICGroups', 'GICIndustries', 'GICSectors', 'GICSubIndustries',
#                  'SPIndustrySectorCode', 'SPEconomicSectorCode')
#        ) const = 4 else const = 10
#     vec = checking_for_duplicates_numeric_cols[[i]]
#     mat = t(replicate(n_firms, vec))
#     is_equal = mat == vec & vec != 0
#     is_close = mat > vec * 0.95 & mat < vec * 1.05 #this doesn't work for negative numbers, but there aren't many
#     score = const * is_equal + is_close
#     replace(score, which(is.na(score)), 0)
#   }
#   diag(numeric_cols_scores) = 0
#   # numeric_cols_results = data.table(similarity_score = c(numeric_cols_scores),
#   #                                   row = rep(1:n_firms, times = n_firms),
#   #                                   col = rep(1:n_firms, each = n_firms))
# 
# 
# use tickers, so SPB = SPB.1
# 
#   checking_for_duplicates_char_cols = checking_for_duplicates[, .(StateProvince, CurrentStateProvinceofIncorporationCode,
#                                                                   AddressLine1, AddressLine2, AddressLine3, AddressLine4,
#                                                                   City, CountyCode, SPQualityRankingCurrent)] #ein fax phone
#   char_cols_scores = foreach(i = 1:ncol(checking_for_duplicates_char_cols), .combine = `+`) %do% {
#     vec = checking_for_duplicates_char_cols[[i]]
#     mat = t(replicate(n_firms, vec))
#     is_equal = mat == vec & vec != 0
#     score = 5 * is_equal
#     replace(score, which(is.na(score)), 0)
#   }
#   diag(char_cols_scores) = 0
# 
#   #wrap this in a function
#   vec = checking_for_duplicates$StockOwnershipCode
#   mat = t(replicate(n_firms, vec))
#   is_subsidiary = mat == 1 | mat == 2 | vec == 1 | vec == 2
#   score = 10 * is_subsidiary
#   subsidiary_scores = replace(score, which(is.na(score)), 0)
#   diag(subsidiary_scores) = 0
# 
#   vec = checking_for_duplicates$exchg
#   mat = t(replicate(n_firms, vec))
#   is_subsidiary_stock_code = mat == 0 | vec == 0
#   score = 5 * is_subsidiary_stock_code
#   subsidiary_stock_scores = replace(score, which(is.na(score)), 0)
#   diag(subsidiary_stock_scores) = 0
# 
#   vec = as.integer(factor(checking_for_duplicates$costat, levels = c('A', 'I')))
#   mat = t(replicate(n_firms, vec))
#   both_active = mat == 1 & vec == 1
#   score = -5 * both_active
#   both_active_scores = replace(score, which(is.na(score)), 0)
#   diag(both_active_scores) = 0
# 
#   threshold_score = 80
#   results = data.table(similarity_score = c(numeric_cols_scores + char_cols_scores + subsidiary_scores + subsidiary_stock_scores + both_active_scores),
#                                     row = rep(1:n_firms, times = n_firms),
#                                     col = rep(1:n_firms, each = n_firms),
#                                     key = 'similarity_score'
#                        )[similarity_score > threshold_score][, pair_id := pmin(row, col) * 10000 + pmax(row, col)]
# 
#   most_likely_duplicates = setorder(results[results[, .I[which.max(similarity_score)], pair_id]$V1], -similarity_score)
#   duplicates_list = data.table(checking_for_duplicates[most_likely_duplicates$row, .(conm1 = conm, GlobalCompanyKey1 = GlobalCompanyKey)],
#                                checking_for_duplicates[most_likely_duplicates$col, .(conm2 = conm, GlobalCompanyKey2 = GlobalCompanyKey)],
#                                similarity_score = most_likely_duplicates$similarity_score)
#   # write.xlsx(duplicates_list, 'duplicates_list.xlsx')
#   #conm conml weburl
#   duplicates_list[, calendaryear := yr]
# }
# stopCluster(clusters)
# 
# # potential_duplicates[dtcut, on = c(conm1 = 'conm', 'calendaryear'), GlobalCompanyKey1 := i.GlobalCompanyKey
# #                    ][dtcut, on = c(conm2 = 'conm', 'calendaryear'), GlobalCompanyKey2 := i.GlobalCompanyKey
# #                    ]
# potential_duplicates[, uniqueid := fifelse(as.numeric(GlobalCompanyKey1)<as.numeric(GlobalCompanyKey2), 
#                                            paste0(GlobalCompanyKey1, GlobalCompanyKey2), 
#                                            paste0(GlobalCompanyKey2, GlobalCompanyKey1))]
# 
# pairs = unique(potential_duplicates, by = 'uniqueid')[, .(GlobalCompanyKey1, GlobalCompanyKey2, uniqueid, k = 1)]
# years = data.table(calendaryear = unique(potential_duplicates$calendaryear), k = 1)
# all_possible_year_pairs = pairs[years, on = 'k', allow.cartesian = T][, k := NULL]
# all_possible_year_pairs[just_compustat, 
#                         on = c(GlobalCompanyKey1 = 'GlobalCompanyKey', 'calendaryear'), 
#                         real_MW1 := i.real_MW
#                       ][just_compustat, 
#                         on = c(GlobalCompanyKey2 = 'GlobalCompanyKey', 'calendaryear'), 
#                         real_MW2 := i.real_MW]
# all_possible_year_pairs[, both_firms_appear_and_have_MW := !is.na(real_MW1) & !is.na(real_MW2)]
# 
# # would_pose_an_issue = merge(
# #   merge(potential_duplicates, just_compustat[, .(conm, calendaryear, real_MW1 = real_MW)], 
# #         by.x = c('conm1', 'calendaryear'), by.y = c('conm', 'calendaryear')), 
# #   just_compustat[, .(conm, calendaryear, real_MW2 = real_MW)], 
# #   by.x = c('conm2', 'calendaryear'), by.y = c('conm', 'calendaryear'))[!is.na(real_MW1) & !is.na(real_MW2)]
# 
# would_pose_an_issue = merge(all_possible_year_pairs[both_firms_appear_and_have_MW == T], 
#                             potential_duplicates[, .SD, .SDcols = !c('GlobalCompanyKey1', 'GlobalCompanyKey2')], 
#                             by = c('uniqueid', 'calendaryear'), all.x = T, all.y = F
#                           )[, at_least_one_true_duplicate_year := sum(both_firms_appear_and_have_MW & !is.na(conm1)) > 0, uniqueid
#                           ][at_least_one_true_duplicate_year == T
#                           ][, c('at_least_one_true_duplicate_year', 'both_firms_appear_and_have_MW') := NULL]
# 
# would_pose_an_issue_with_one_row_per_firm = 
#   melt(would_pose_an_issue[, !(real_MW1:real_MW2)], 
#      measure.vars = list(c('conm1', 'conm2'), c('GlobalCompanyKey1', 'GlobalCompanyKey2')), 
#      variable.name = 'pair_id', 
#      value.name = c('conm', 'GlobalCompanyKey'))
# to_check_for_duplicates_by_hand = merge(would_pose_an_issue_with_one_row_per_firm[, .SD, .SDcols = !c('conm')], 
#                                         duplicate_checking_data, 
#                                         by = c('GlobalCompanyKey', 'calendaryear'))
# to_check_for_duplicates_by_hand[, max_similarity_score := max(similarity_score, na.rm = T), uniqueid]
# to_check_for_duplicates_by_hand[read_feather('Data/firstyears.feather'), on = c(GlobalCompanyKey = 'gvkey'), `:=`(firstyear = i.year1, finalyear = i.year2)]
# to_check_for_duplicates_by_hand[, next_year := calendaryear + 1
#                               ][just_compustat, on = c('GlobalCompanyKey', next_year = 'calendaryear'), 
#                                 `:=`(conm_next_year = i.conm, MW_next_year = i.real_MW)
#                               ][, c('Exists Next Year', 'Has MW Next Year') := .(!is.na(conm_next_year), !is.na(MW_next_year))
#                               ][, c('next_year', 'conm_next_year', 'MW_next_year') := NULL]
# setorder(to_check_for_duplicates_by_hand, -max_similarity_score, uniqueid, calendaryear, conm
#        )[, is_duplicate := +(similarity_score > 130 & !is.na(similarity_score))][, should_delete := NA_integer_]
# # would_pose_an_issue_unique = setorder(would_pose_an_issue[would_pose_an_issue[, .I[which.max(similarity_score)], uniqueid]$V1], -similarity_score)
# first_columns = c('conm', 'is_duplicate', 'should_delete', 'calendaryear', 'similarity_score', 
#                   'PropertyPlantandEquipmentTotalGross', 'RevenueTotal', 'Employees', 
#                   'Exists Next Year', 'Has MW Next Year', 'firstyear', 'finalyear', 
#                   'naics', 'sic', 'add1', 'addzip', 'city', 'phone', 'fax')
# setcolorder(to_check_for_duplicates_by_hand, c(first_columns, names(to_check_for_duplicates_by_hand)[!names(to_check_for_duplicates_by_hand) %in% first_columns]))
# # write.xlsx(to_check_for_duplicates_by_hand, 'PotentialDuplicatesToHandCode.xlsx')

asdf = data.table(read.xlsx('Data/HandCodedDuplicates.xlsx'))
asdf[, total_should_delete_firm := sum(should_delete, na.rm = T), .(uniqueid, GlobalCompanyKey)]
asdf[, total_should_delete_pair := sum(should_delete, na.rm = T), uniqueid]
asdf[((total_should_delete_firm == 0 | total_should_delete_firm == -Inf) & total_should_delete_pair > 0) & is.na(should_delete), should_delete := 0]
asdf[total_should_delete_firm > 0 & total_should_delete_pair > 0 & is.na(should_delete), should_delete := 1]
asdf[is.na(should_delete) & similarity_score > 130 & pair_id == 1, should_delete := 1]
# to_check_for_duplicates_by_hand[similarity_score > 100 & pair_id == 1, should_delete := 1]

dtcut[asdf[should_delete == 1, .(GlobalCompanyKey, calendaryear, should_delete, conm)], on = c('GlobalCompanyKey', 'calendaryear'), should_delete := i.should_delete]
# dtcut[to_check_for_duplicates_by_hand[should_delete == 1], on = c('GlobalCompanyKey', 'calendaryear'), should_delete := i.should_delete]
dtcut = dtcut[should_delete != 1 | is.na(should_delete)]


