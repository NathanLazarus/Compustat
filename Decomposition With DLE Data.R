input_data = c(dtcutForSpreadsheets = 'IntermediateFiles/dtcut_for_spreadsheets.feather',
               FREDData = 'Data/FRED Data (Inflation and Interest Rates).feather')

output_files = c(MWTWDecomposition = 'SpreadsheetOutputs/MWTW_decomposition.xlsx')

dtcut = read_feather_dt(input_data['dtcutForSpreadsheets'])






dtcut[,                                                       two_digit_sic := floor(SIC/100)*100]
dtcut[substr(as.character(SIC), nchar(SIC) - 1, nchar(SIC) - 1) != 0, three_digit_sic := floor(SIC/10)*10]
dtcut[substr(as.character(SIC), nchar(SIC) - 0, nchar(SIC) - 0) != 0, four_digit_sic := floor(SIC/1)*1]
dt_for_crosswalk = dtcut[!is.na(SIC) & !is.na(NAICS)]
dt_for_crosswalk = dt_for_crosswalk[
  dt_for_crosswalk[, .(ind = .I[which.min(calendaryear)]), .(GlobalCompanyKey, SIC)]$ind, 
  .(two_digit_sic, three_digit_sic, four_digit_sic, NAICS)]


possibleSICvalues = na.omit(unique(c(dtcut$two_digit_sic, dtcut$three_digit_sic, dtcut$four_digit_sic)))

SICtoNAICS = foreach(val = possibleSICvalues, .combine = rbind) %do% {
  if (val >= 9900) {
    customcrosswalk = data.table(NAICS = 999990, numfirms = NA_integer_, sic = val, sic_ratio = 1)
  } else {
    customcrosswalk = dt_for_crosswalk[two_digit_sic == val | three_digit_sic == val | four_digit_sic == val, 
                                       .(numfirms = .N), NAICS]
    customcrosswalk[, sic := val][, sic_ratio := numfirms/sum(numfirms)]
  }
  customcrosswalk
}
setnames(SICtoNAICS, 'NAICS', 'naics')
SICtoNAICS[, numfirms := NULL]
# additional_SICs = rbind(additional_SICs, data.table(sic = 9900, naics = 999990, sic_ratio = 1))
# SICtoNAICS = rbind(SICtoNAICS_bls, additional_SICs, fill = T)

# suppressWarnings(dtcut[, sic := NULL])
missingNAICS = dtcut[is.na(NAICS)]
missingNAICS[SIC %in% SICtoNAICS$sic, sic := SIC]
missingNAICS[is.na(sic) & (SIC - SIC %% 10) %in% SICtoNAICS$sic, sic := SIC - SIC %% 10]
missingNAICS[is.na(sic) & (SIC - SIC %% 100) %in% SICtoNAICS$sic, sic := SIC - SIC %% 100]
# setkey(dtcut, sic)
# setkey(SICtoNAICS, sic)
# rbind(missingNAICS[SICtoNAICS, eval(.(names...)), by = .EACHI], missingNAICS[sic>9000])
addingNAICS = merge(missingNAICS, SICtoNAICS, by = 'sic', all.x = T, allow.cartesian = T)
addingNAICS[, NAICSadded := 1]
# addingNAICS[, `:=`(`CES SIC Tabulating Code`=NULL, 
#                   `SIC Industry`=NULL, 
#                   `CES NAICS Tabulating Code`=NULL, 
#                   `NAICS Industry`=NULL, 
#                   `SIC to NAICS Employment Ratio`=NULL)]
setnames(addingNAICS, 'naics', 'imputed_NAICS')
withNAICS = rbind(dtcut, addingNAICS, fill = T)
withNAICS[is.na(NAICS) & is.na(NAICSadded), sic_ratio := 0]
#why did I do this? I left all the NA NAICS firms in there, with ratio equal to 0.
#I do this the whole time. It's so confusing and bad.
withNAICS[, realfirm := is.na(NAICSadded)]
withNAICS[!is.na(NAICS) & is.na(NAICSadded), sic_ratio := 1]





withNAICS[is.na(NAICS), NAICS := imputed_NAICS]
withNAICS[, my_two_digit_NAICS := roundDown(NAICS, 10000)]
production_function_coefs = setnames(data.table(read_dta('Data/theta_W_s_window.dta')), 
                                     c('ind2d', 'theta_WI1_ct'), c('two_digit_NAICS', 'theta_v')
                                   )[, my_two_digit_NAICS := 10000 * two_digit_NAICS]
withNAICS[production_function_coefs, on = c('my_two_digit_NAICS', DataYearFiscal = 'year'), theta_v := i.theta_v]
withNAICS[, DLE_markup := theta_v * SalesTurnoverNet / CostofGoodsSold]
theta_vx = 0.95
withNAICS[, Traina_markup := theta_vx * SalesTurnoverNet / (CostofGoodsSold + SellingGeneralandAdministrativeExpense)]
#is kexp in profits?
withNAICS[, DLE_profit := (SalesTurnoverNet - CostofGoodsSold -
                                 SellingGeneralandAdministrativeExpense) / SalesTurnoverNet]

withTwoDigit = withNAICS[sic_ratio > 0]


withTwoDigit[totalwealth < 0, totalwealth := 0]
withTwoDigit[is.na(SalesTurnoverNet) | SalesTurnoverNet < 0, SalesTurnoverNet := 0]
two_digit_one_obs_per_firm = withTwoDigit[sic_ratio > 0, .(monopolywealth = first(monopolywealth), 
                                               totalwealth = first(totalwealth), 
                                               DLE_markup = first(DLE_markup), 
                                               SalesTurnoverNet = first(SalesTurnoverNet), 
                                               sic_ratio = sum(sic_ratio)), 
                      .(GlobalCompanyKey, calendaryear, my_two_digit_NAICS)]
# every_firm_year_combo = unique(two_digit_one_obs_per_firm, by = c('GlobalCompanyKey', 'calendaryear'))[, my_two_digit_NAICS := NULL]
# every_firm_industry_combo = unique(two_digit_one_obs_per_firm[, .(GlobalCompanyKey, my_two_digit_NAICS)])
# firm_industry_balanced_panel = merge(every_firm_year_combo, every_firm_industry_combo, 
#                                      by = 'GlobalCompanyKey', allow.cartesian = T)[, sic_ratio := 0]
# data = firm_industry_balanced_panel[two_digit_one_obs_per_firm, 
#                                     on = c('GlobalCompanyKey', 'calendaryear', 'my_two_digit_NAICS'), 
#                                     sic_ratio := i.sic_ratio]
# data[is.na(sic_ratio), `:=`(monopolywealth = 0, sic_ratio = 0)]
# data[unique(dtcut[, .(GlobalCompanyKey, calendaryear, SIC)]), on = c('GlobalCompanyKey', 'calendaryear'), SIC := i.SIC]



firms = withNAICS[!roundDown(SIC, 100) %in% c(49) & !between(SIC, 6000, 6499) & !is.na(monopolywealth) & !is.na(totalwealth), 
             .(firm_MW = sum(monopolywealth * sic_ratio), 
               firm_TW = sum(totalwealth * sic_ratio), 
               firm_sales = sum(SalesTurnoverNet * sic_ratio), 
               DLE_markup = first(DLE_markup)), 
             .(GlobalCompanyKey, calendaryear)][firm_TW > 0 & !is.na(firm_sales)]
firms[, size := firm_sales/sum(firm_sales), calendaryear]
# firms[, mwtw_ratio := firm_MW/firm_TW]

DLE_data = data.table(read_dta('Data/temp_file.dta'))


yearvecs = list(years = 1980:2016, years = c(1980, 1990, 2000, 2010, 2016),
                years = c(1980, 1998, 2016), years = c(1980, 2016))
# years = 1980:2016
# years = c(1980, 1990, 2000, 2010, 2016)
# years = c(1980, 1998, 2016)
# years = c(1980, 2016)
foreach(years = yearvecs)%do%{
over_time_decomposition_firm = foreach(yr_index = 2:length(years), .combine = rbind_and_fill) %do% {
  firms_stay_the_same = merge(firms[calendaryear == years[yr_index - 1]], firms[calendaryear == years[yr_index]], all.x = F, all.y = F, by = 'GlobalCompanyKey', suffixes = c('_old', '_new'))
  initial = firms_stay_the_same[, sum(firm_sales_old/sum(firm_sales_old) * firm_MW_old) / sum(firm_sales_old/sum(firm_sales_old) * firm_TW_old)]
  change_mw = firms_stay_the_same[, sum(firm_sales_old/sum(firm_sales_old) * firm_MW_new) / sum(firm_sales_old/sum(firm_sales_old) * firm_TW_new)]
  change_market_shares = firms_stay_the_same[, sum(firm_sales_new/sum(firm_sales_new) * firm_MW_old) / sum(firm_sales_new/sum(firm_sales_new) * firm_TW_old)]
  final = firms_stay_the_same[, sum(firm_sales_new/sum(firm_sales_new) * firm_MW_new) / sum(firm_sales_new/sum(firm_sales_new) * firm_TW_new)]
  actual_mw_old = firms[calendaryear == years[yr_index - 1], sum(size * firm_MW) / sum(size * firm_TW)]
  actual_mw_new = firms[calendaryear == years[yr_index], sum(size * firm_MW) / sum(size * firm_TW)]
  data.table(year = years[yr_index], initial = initial, change_mw = change_mw, change_market_shares = change_market_shares, 
             linear =  change_mw + change_market_shares - initial, final = final, 
             entry_exit = (actual_mw_new - actual_mw_old) - (final - initial)
  )[, `:=`(reallocation = change_market_shares - initial, 
           within_firm_rise = change_mw - initial, 
           cross_term = final - linear)][]
}

over_time_decomposition_long = melt(over_time_decomposition_firm[, .(year, reallocation, within_firm_rise, 
                                                                     cross_term, entry_exit)], 
                                    id.vars = 'year', variable.name = 'component', value.name = 'mwtw_rise')
over_time_decomposition_long[, cumulative := cumsum(mwtw_rise), component]
ggplot(over_time_decomposition_long, aes(x = year, y = cumulative, color = component)) +
  geom_line(size = 1, aes(linetype = component))





# firms_DLE = firms[!is.na(DLE_markup) & DLE_markup < 20 & DLE_markup > -1]
  # for (i in names(firms))
  #   firms[is.nan(get(i)), (i) := 0]

firms_DLE = DLE_data[, .(firm_sales = sale_D, DLE_markup = mu_spec1, size = sale_D/sum(sale_D), GlobalCompanyKey = gvkey), year]
setnames(firms_DLE, 'year', 'calendaryear')

over_time_decomposition_DLE = foreach(yr_index = 2:length(years), .combine = rbind_and_fill) %do% {
  firms_stay_the_same = merge(firms_DLE[calendaryear == years[yr_index - 1]], firms_DLE[calendaryear == years[yr_index]], all.x = F, all.y = F, 
                              by = c('GlobalCompanyKey'), suffixes = c('_old', '_new'))
  
  initial = firms_stay_the_same[, Hmisc::wtd.mean(DLE_markup_old, size_old)]
  change_market_shares = firms_stay_the_same[, Hmisc::wtd.mean(DLE_markup_old, size_new)]
  change_mw = firms_stay_the_same[, Hmisc::wtd.mean(DLE_markup_new, size_old)]
  final = firms_stay_the_same[, Hmisc::wtd.mean(DLE_markup_new, size_new)]
  
  actual_mw_old = firms_DLE[calendaryear == years[yr_index - 1], Hmisc::wtd.mean(DLE_markup, size)]
  actual_mw_new = firms_DLE[calendaryear == years[yr_index], Hmisc::wtd.mean(DLE_markup, size)]
  
  data.table(year = years[yr_index], initial = initial, change_mw = change_mw, change_market_shares = change_market_shares, 
             linear =  change_mw + change_market_shares - initial, final = final, 
             entry_exit = (actual_mw_new - actual_mw_old) - (final - initial)
           )[, `:=`(reallocation = change_market_shares - initial, 
                    within_firm_rise = change_mw - initial, 
                    cross_term = final - linear)][]
}
over_time_decomposition_long = melt(over_time_decomposition_DLE[, .(year, reallocation, within_firm_rise, 
                                                                         cross_term, entry_exit)], 
                                    id.vars = 'year', variable.name = 'component', value.name = 'mwtw_rise')
over_time_decomposition_long[, cumulative := cumsum(mwtw_rise), component]
ggplot(over_time_decomposition_long, aes(x = year, y = cumulative, color = component)) + geom_line(size = 1, aes(linetype = component))
list(years, over_time_decomposition_long[year == 2016, .(year, component, cumulative)])
}