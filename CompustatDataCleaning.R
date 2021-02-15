input_data = c(companyData = 'Data/Company Data (fixed identifying variables).rds', 
               fundamentalsData = 'Data/Annual Fundamentals (most variables, raw).rds', 
               VariableNames = 'Data/Variable Names and Descriptions.csv', 
               KLDData = 'IntermediateFiles/KLD Data Clean.rds')

output_files = c(raw_dt = 'IntermediateFiles/raw_dt.csv', 
                 dtcut = 'IntermediateFiles/dtcut.csv')

#TRANE CO, TRANE INC? Duplicates?

#This leaves financial firms and firms with missing market values in the data

companyData = readRDS(input_data['companyData'])
fundamentalsData = readRDS(input_data['fundamentalsData'])

merge_and_reconcile = function(prioritized_data, deprioritized_data, join_cols, all_prioritized = T, all_deprioritized = T) {
  merged_wide_with_duplicates = merge(prioritized_data, deprioritized_data, by = join_cols, 
                                      all.x = all_prioritized, all.y = all_deprioritized, 
                                      suffixes = c('.from_prioritized', '.from_deprioritized'))
  
  dupe_cols = gsub('\\.from_prioritized', '', grep('\\.from_prioritized', names(merged_wide_with_duplicates), value = T))
  non_duplicated_cols = names(merged_wide_with_duplicates)[!grepl('\\.from_prioritized|\\.from_deprioritized', names(merged_wide_with_duplicates))]
  
  foreach(var = dupe_cols) %do% {
    var_prioritized = paste0(var, '.from_prioritized')
    var_deprioritized = paste0(var, '.from_deprioritized')
    merged_wide_with_duplicates[, (var) := fifelse(!is.na(get(var_prioritized)) & get(var_prioritized) != 0, get(var_prioritized), 
                                                   fifelse(!is.na(get(var_prioritized)) & get(var_prioritized) == 0, 
                                                           fifelse(!is.na(get(var_deprioritized)) & get(var_deprioritized) != 0, get(var_deprioritized), get(var_prioritized)), 
                                                           fifelse(!is.na(get(var_deprioritized)), get(var_deprioritized), get(var_prioritized))))]
    NULL
  }
  
  return(merged_wide_with_duplicates[, c(non_duplicated_cols, dupe_cols), with = F])
  
}


# library(haven)
# dt_deloecker_eeckhout = fundamentalsData[companyData, on = 'gvkey'
#                          ][datafmt == 'STD' & consol == 'C' &
#                              between(datadate, as.Date('1955-01-01'), as.Date('2016-12-31'))]
# 
# dt_deloecker_eeckhout[, (grep('i\\.', names(dt_deloecker_eeckhout), value = T)) := NULL]
# write_dta(dt_deloecker_eeckhout[, .(gvkey, datadate, fyear, indfmt, consol, popsrc, datafmt, conm, curcd, 
#                                 cogs, csho, dvt, emp, intan, oibdp, ppegt, ppent, sale, xad, xlr, xrd, 
#                                 xsga, costat, fic, prcc_c, mkvalt, prcc_f, naics)], 
#           'C:/Users/Nathan/Downloads/DeloeckerEeckhout_v3/data/datafile.dta')

# TFP
# write_dta(companyData[fundamentalsData[, c('year1', 'year2') := .(min(fyear), max(fyear)), gvkey], on = 'gvkey', `:=`(year1 = i.year1, year2 = i.year2)], 
#           'C:/Users/Nathan/Downloads/Programs-20201123T183843Z-001/Programs/compustat_names.dta')
# write_dta(fundamentalsData, 'C:/Users/Nathan/Downloads/Programs-20201123T183843Z-001/Programs/compustat_data.dta')


companyData[, sic := as.numeric(sic)]
companyData[, naics := as.numeric(naics)]
varnames = fread(input_data['VariableNames'])

varnames[, shortVarName := tolower(shortVarName)
       ][, cleanDescriptiveVarName := gsub("[^[:alnum:]]", "", fullDescriptiveVarName)]
descriptive_variable_names_for_fundamentalsData = data.table(shortVarName = tolower(names(fundamentalsData))
                                                           )[varnames, on = 'shortVarName', cleanDescriptiveVarName := i.cleanDescriptiveVarName
                                                           ][!is.na(cleanDescriptiveVarName)]
setnames(fundamentalsData, 
         descriptive_variable_names_for_fundamentalsData[, shortVarName], 
         descriptive_variable_names_for_fundamentalsData[, cleanDescriptiveVarName])

descriptive_variable_names_for_companyData = data.table(shortVarName = tolower(names(companyData))
                                                      )[varnames, on = 'shortVarName', cleanDescriptiveVarName := i.cleanDescriptiveVarName
                                                      ][!is.na(cleanDescriptiveVarName)]
setnames(companyData, 
         descriptive_variable_names_for_companyData[, shortVarName], 
         descriptive_variable_names_for_companyData[, cleanDescriptiveVarName])

# setnames(varnames, c('combined', 'varname', 'varfull'))
# varnames[, lowervarname := tolower(varname)]
# merge = varnames[data.table(lowervarname = tolower(names(fundamentalsData))), on = 'lowervarname', nomatch = 0]
# merge[, UpperCamel := gsub("[^[:alnum:]]", "", varfull)]
# setnames(fundamentalsData, merge$lowervarname, merge$UpperCamel)

fundamentalsData[companyData, on = 'GlobalCompanyKey', 
   `:=`(currentsic = i.StandardIndustryClassificationCode, 
        currentnaics = i.NorthAmericanIndustryClassificationCode, 
        loc = i.CurrentISOCountryCodeHeadquarters)]
fundamentalsData[, SIC := StandardIndustrialClassificationHistorical]
fundamentalsData[is.na(SIC), SIC := currentsic]
fundamentalsData[, NAICS := NorthAmericaIndustrialClassificationSystemHistorical]
fundamentalsData[is.na(NAICS), NAICS := currentnaics]

fundamentalsData[, calendaryear := year(datadate)]
fundamentalsData[, cusip6 := substr(cusip, 1, 6)]

KLD_data_clean = readRDS(input_data['KLDData'])

fundamentalsData[KLD_data_clean, on = c('cusip6', calendaryear = 'year'), `:=`(
  laborStrength = i.laborStrength, laborConcern = i.laborConcern, 
  laborRelations = i.laborRelations, `Anticompetitive Practices` = `i.Anticompetitive Practices`
)]

fwrite(fundamentalsData, output_files['raw_dt'])


fundamentalsData = fundamentalsData[consol == 'C']
#remove duplicative entries like pro forma or unconsolidated pre-FASB statements

with_restatements = merge_and_reconcile(fundamentalsData[datafmt == 'SUMM_STD' & indfmt == 'INDL'], 
                                        fundamentalsData[datafmt == 'STD' & indfmt == 'INDL'], 
                                        join_cols = c('GlobalCompanyKey', 'datadate'), 
                                        all_prioritized = F, 
                                        all_deprioritized = T)

with_financial_format_statements = merge_and_reconcile(with_restatements, 
                                                       fundamentalsData[datafmt == 'STD' & indfmt == 'FS'], 
                                                       join_cols = c('GlobalCompanyKey', 'datadate'))

# asdf = merge(fundamentalsData[datafmt == 'SUMM_STD' & indfmt == 'INDL'], fundamentalsData[datafmt == 'STD' & indfmt == 'INDL'], 
#              by = c('GlobalCompanyKey', 'datadate'), all.x = F, all.y = T, 
#              #important to not include SUMM_STD entries when there's no corresponding STD (duplicative after a merger or non-public)
#              suffixes = c('.summstd', '.std'))

# join_cols = c('GlobalCompanyKey', 'datadate')
# dupe_cols = gsub('.summstd', '', grep('.summstd', names(asdf), value = T))

# foreach(var = dupe_cols) %do% {
#   var_prioritized = paste0(var, '.summstd')
#   var_deprioritized = paste0(var, '.std')
#   asdf[, (var) := fifelse(!is.na(get(var_prioritized)) & get(var_prioritized) != 0, get(var_prioritized), 
#                                                   fifelse(!is.na(get(var_prioritized)) & get(var_prioritized) == 0, fifelse(!is.na(get(var_deprioritized)) & get(var_deprioritized) != 0, get(var_deprioritized), get(var_prioritized)), 
#                                                           fifelse(!is.na(get(var_deprioritized)), get(var_deprioritized), get(var_prioritized))))]
#   NULL
# }


# jkl = merge(asdf[, c(join_cols, dupe_cols), with = F], fundamentalsData[datafmt == 'STD' & indfmt == 'FS'], 
#              by = c('GlobalCompanyKey', 'datadate'), all.x = T, all.y = T, 
#              suffixes = c('.indl', '.fs'))

# join_cols = c('GlobalCompanyKey', 'datadate')
# dupe_cols = gsub('.indl', '', grep('.indl', names(jkl), value = T))

# foreach(var = dupe_cols) %do% {
#   var_prioritized = paste0(var, '.indl')
#   var_deprioritized = paste0(var, '.fs')
#   jkl[, (var) := fifelse(!is.na(get(var_prioritized)) & get(var_prioritized) != 0, get(var_prioritized), 
#                                                   fifelse(!is.na(get(var_prioritized)) & get(var_prioritized) == 0, fifelse(!is.na(get(var_deprioritized)) & get(var_deprioritized) != 0, get(var_deprioritized), get(var_prioritized)), 
#                                                           fifelse(!is.na(get(var_deprioritized)), get(var_deprioritized), get(var_prioritized))))]
#   NULL
# }

# good = jkl[, c(join_cols, dupe_cols), with = F]

# testdt = data.table(AssetsTotal.x = rep(c(0, 1, NA), times = 3), AssetsTotal.y = rep(c(0, 1, NA), each = 3))

# dtcut = fundamentalsData[curcd == 'USD'&!is.na(curcd)
#            &loc == 'USA'
#            &consol == 'C'
#            &datafmt == 'STD'
#            &AssetsTotal != 0
#            &!is.na(SIC)] #the only firms missing SIC codes are firms that have yet to IPO. I don't understand the connection. They have NAICS codes.
# 
# dtcut = dtcut[dtcut[, .I[sum(indfmt == 'INDL') == 0|indfmt == 'INDL'], .(GlobalCompanyKey, DataYearFiscal)]$V1] #I use indfmt FS and INDL and then drop FS reports when they're duplicated

dtcut = with_financial_format_statements[(curcd == 'USD' & !is.na(curcd)) & loc == 'USA' & (AssetsTotal != 0 | is.na(AssetsTotal)) & !is.na(SIC)]
#the only firms missing SIC codes are firms that have yet to IPO. I don't understand the connection. They have NAICS codes.
nrow(with_financial_format_statements[is.na(AssetsTotal)])

dtcut[conm == 'DELHAIZE AMERICA INC' & calendaryear == 2001 & CommonSharesOutstanding == 91125.785, 
      CommonSharesOutstanding := dtcut[conm == 'DELHAIZE AMERICA INC' & calendaryear == 2000]$CommonSharesOutstanding]
dtcut[, MktVal := MarketValueTotalFiscal]
dtcut[is.na(MktVal), MktVal := PriceCloseAnnualFiscal * CommonSharesOutstanding]
dtcut[is.na(MktVal), MktVal := PriceCloseAnnualCalendar * CommonSharesOutstanding]
dtcut[is.na(PreferredPreferenceStockCapitalTotal) & !is.na(PreferredPreferenceStockRedeemable), 
      PreferredPreferenceStockCapitalTotal := PreferredPreferenceStockRedeemable]

dtcut[, preferred := pmax(PreferredPreferenceStockCapitalTotal, PreferredStockLiquidatingValue, PreferredStockRedemptionValue, PreferredStockConvertible, na.rm = T)]
dtcut[!is.na(preferred), MktVal := MktVal + preferred]
dtcut = dtcut[MktVal != 0 | is.na(MktVal)] #drop about 100 firms with 0 common shares outstanding, mostly firms in the process of dissolving

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
dtcut[, keep := datadate == max(datadate), .(calendaryear, GlobalCompanyKey)]
dtcut = dtcut[keep == T][, c('haspreviousfiscalyear', 'hascalendaryear', 'missing', 'keep') := NULL]

dtcut = dtcut[calendaryear < 2020]

dtcut_without_utilities = dtcut[SIC %/% 100 == 49 & 
                                  as.numeric(str_pad(as.character(NAICS), width = 6, side = 'right', pad = '0')) %/% 1000 != 221]

#remove duplicates:


# duplicate_checking_data = merge(
#   dtcut_without_utilities, 
#   companyData[, .SD, .SDcols = !c('conm', 'costat', 'CurrentISOCountryCodeHeadquarters', 
#                                   'ResearchCoReasonforDeletion', 'ResearchCompanyDeletionDate', 
#                                    'CurrentPrimaryIssueTagCanada', 'PrimaryIssueTagRestofWorld', 
#                                    'CurrentPrimaryIssueTagUS', 'InternationalDomesticBothIndicator', 
#                                    'CurrentFiscalYearEndMonth')], 
#   by.x = 'GlobalCompanyKey', 
#   by.y = 'gvkey')[, .SD, .SDcols = !c('curcd', 'currentsic', 'currentnaics', 'SIC', 'NAICS', 'DataYearFiscal', 'indfmt', 'consol', 'popsrc', 'datafmt', 'tic', 'cusip', 'cusip6', 'laborStrength', 'laborConcern', 'laborRelations', 'Anticompetitive Practices', 'MktVal', 'preferred')]
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
#   checking_for_duplicates_char_cols = checking_for_duplicates[, .(StateProvince, CurrentStateProvinceofIncorporationCode, AddressLine1, AddressLine2, AddressLine3, AddressLine4, City, CountyCode, SPQualityRankingCurrent)] #ein fax phone
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
# to_check_for_duplicates_by_hand[readRDS('Data/firstyears.rds'), on = c(GlobalCompanyKey = 'gvkey'), `:=`(firstyear = i.year1, finalyear = i.year2)]
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
dtcut[, AssetsOther := AssetsOther - na0(DeferredCharges) - na0(PrepaidExpenses)]
dtcut[, intangibleratio := IntangibleAssetsTotal/AssetsTotal]
setkey(dtcut, GlobalCompanyKey, calendaryear)

# dtcut[, intangiblesadded:= +is.na(IntangibleAssetsTotal)]
dtcut_no_NA_intangibles = dtcut[!is.na(IntangibleAssetsTotal)]
setkey(dtcut_no_NA_intangibles, GlobalCompanyKey, calendaryear)
dtcut[, intangibleratio := dtcut_no_NA_intangibles[dtcut, intangibleratio, roll = 'nearest']
    ][is.na(IntangibleAssetsTotal), `:=`(IntangibleAssetsTotal = pmin(intangibleratio*AssetsTotal, na0(Goodwill), pmax(na0(AssetsOther), 0)), 
                                        AssetsOther = na0(AssetsOther) - na0(pmin(intangibleratio*AssetsTotal, na0(Goodwill), pmax(na0(AssetsOther), 0))))]

dtcut[, twodigitsic := as.character(floor(SIC/100))]

intangiblemod = lm(intangibleratio~factor(calendaryear)+twodigitsic, data = dtcut)
dtcut[, predictedintangibleratio := pmax(predict(intangiblemod, dtcut), 0)
    ][is.na(IntangibleAssetsTotal), `:=`(IntangibleAssetsTotal = pmin(predictedintangibleratio*AssetsTotal, na0(Goodwill), pmax(na0(AssetsOther), 0)), 
                                        AssetsOther = na0(AssetsOther) - na0(pmin(predictedintangibleratio*AssetsTotal, na0(Goodwill), pmax(na0(AssetsOther), 0))))]



fwrite(dtcut, output_files['dtcut'], quote = T)