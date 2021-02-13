rbind_and_fill = function(...) rbind(...,fill=T)

getCharCols = function(x) {
  second_line = readLines(x,n = 2)[2]
  cols = strsplit(second_line, ',')[[1]]
  grep('"',cols)
}

fread_and_getCharCols = function(x) {
  fread(x, colClasses = list(character = getCharCols(x)))
}


data = fread_and_getCharCols('foranalysis.csv')


data[, my_three_digit_NAICS := true_six_digit_NAICS %/% 1000]
data[totalwealth < 0, totalwealth := 0]
data[is.na(SalesTurnoverNet) | SalesTurnoverNet < 0, SalesTurnoverNet := 0]







firms = data[!my_three_digit_NAICS %in% c(221, 521, 522, 523, 524, 525) & !is.na(my_three_digit_NAICS) & !is.na(monopolywealth) &!is.na(totalwealth),
            .(firm_MW = sum(monopolywealth * six_digit_ratio),
              firm_TW = sum(totalwealth * six_digit_ratio),
              firm_sales = sum(SalesTurnoverNet * six_digit_ratio)),
            .(GlobalCompanyKey, calendaryear)][firm_TW > 0]
firms[, size := firm_sales/sum(firm_sales), calendaryear]
# firms[, mwtw_ratio := firm_MW/firm_TW]

over_time_decomposition_firm = foreach(yr = 1986:2019, .combine = rbind_and_fill)%do%{
  firms_stay_the_same = merge(firms[calendaryear == yr-1], firms[calendaryear == yr], all.x = F, all.y = F, by = 'GlobalCompanyKey', suffixes = c('_old', '_new'))
  initial = firms_stay_the_same[, sum(firm_sales_old/sum(firm_sales_old) * firm_MW_old) / sum(firm_sales_old/sum(firm_sales_old) * firm_TW_old)]
  change_mw = firms_stay_the_same[, sum(firm_sales_old/sum(firm_sales_old) * firm_MW_new) / sum(firm_sales_old/sum(firm_sales_old) * firm_TW_new)]
  change_market_shares = firms_stay_the_same[, sum(firm_sales_new/sum(firm_sales_new) * firm_MW_old) / sum(firm_sales_new/sum(firm_sales_new) * firm_TW_old)]
  final = firms_stay_the_same[, sum(firm_sales_new/sum(firm_sales_new) * firm_MW_new) / sum(firm_sales_new/sum(firm_sales_new) * firm_TW_new)]
  actual_mw_old = firms[calendaryear == yr-1, sum(size * firm_MW) / sum(size * firm_TW)]
  actual_mw_new = firms[calendaryear == yr, sum(size * firm_MW) / sum(size * firm_TW)]
  data.table(year = yr, initial = initial, change_mw = change_mw, change_market_shares = change_market_shares,
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
ggplot(over_time_decomposition_long, aes(x = year, y = cumulative, color = component)) + geom_line(size = 1, aes(linetype = component))




firms_and_industries = data[!my_three_digit_NAICS %in% c(221, 521, 522, 523, 524, 525) & !is.na(my_three_digit_NAICS) & !is.na(monopolywealth) &!is.na(totalwealth),
             .(firm_MW = sum(monopolywealth * six_digit_ratio),
               firm_TW = sum(totalwealth * six_digit_ratio),
               firm_sales = sum(SalesTurnoverNet * six_digit_ratio),
               DLE_markup = first(DLE_markup)),
             .(GlobalCompanyKey, calendaryear, my_three_digit_NAICS)][firm_TW > 0]
firms_and_industries[, size := firm_sales/sum(firm_sales), calendaryear]
# firms_and_industries[, mwtw_ratio := firm_MW/firm_TW]
firms_and_industries[, my_two_digit_NAICS := my_three_digit_NAICS %/% 10]

library(iterators)
library(snow)
library(doSNOW)

clusters=makeCluster(7)
registerDoSNOW(clusters)

which_industries_three_digit = foreach(industry_held_constant = unique(firms_and_industries$my_three_digit_NAICS), .combine = rbind_and_fill)%dopar%{
  industry_held_constant_share_1985 = firms_and_industries[calendaryear == 1985 & my_three_digit_NAICS == industry_held_constant, sum(firm_sales)/sum(firms_and_industries[calendaryear == 1985]$firm_sales)]
  industry_held_constant_share_2019 = firms_and_industries[calendaryear == 2019 & my_three_digit_NAICS == industry_held_constant, sum(firm_sales)/sum(firms_and_industries[calendaryear == 2019]$firm_sales)]
  to_allocate = 1 - industry_held_constant_share_1985
  all_firms_to_fix_industry_share = copy(firms_and_industries)
  all_firms_to_fix_industry_share[, firm_share_of_industry := firm_sales/sum(firm_sales), .(my_three_digit_NAICS, calendaryear)]
  all_firms_to_fix_industry_share[is.nan(firm_share_of_industry), firm_share_of_industry := 0]
  all_firms_to_fix_industry_share[my_three_digit_NAICS == industry_held_constant, industry_share_of_economy := industry_held_constant_share_1985]
  all_firms_to_fix_industry_share[my_three_digit_NAICS != industry_held_constant, total_industry_sales := sum(firm_sales),
                                  .(my_three_digit_NAICS, calendaryear)
                                ][my_three_digit_NAICS != industry_held_constant, industry_share_of_economy := to_allocate * total_industry_sales / sum(firm_sales), calendaryear]
  data.table(rise_in_mwtw_if_industry_share_held_constant = all_firms_to_fix_industry_share[calendaryear == 2019, sum(firm_share_of_industry * industry_share_of_economy * firm_MW) / sum(firm_share_of_industry * industry_share_of_economy * firm_TW)]
             - all_firms_to_fix_industry_share[calendaryear == 1985, sum(firm_share_of_industry * industry_share_of_economy * firm_MW) / sum(firm_share_of_industry * industry_share_of_economy * firm_TW)],
             industry_held_constant = industry_held_constant,
             industry_share_1985 = industry_held_constant_share_1985,
             industry_share_2019 = industry_held_constant_share_2019,
             change_in_industry_size = industry_held_constant_share_2019 - industry_held_constant_share_1985,
             industry_2019_mwtw_ratio_above_average = firms_and_industries[calendaryear == 2019 & my_three_digit_NAICS == industry_held_constant, sum(firm_MW)/sum(firm_TW)] - firms_and_industries[calendaryear == 2019, sum(firm_MW)/sum(firm_TW)],
             industry_1985_mwtw_ratio_above_average = firms_and_industries[calendaryear == 1985 & my_three_digit_NAICS == industry_held_constant, sum(firm_MW)/sum(firm_TW)] - firms_and_industries[calendaryear == 1985, sum(firm_MW)/sum(firm_TW)])
}
stopCluster(clusters)
setkey(which_industries_three_digit,rise_in_mwtw_if_industry_share_held_constant)[]

clusters=makeCluster(7)
registerDoSNOW(clusters)

which_industries_two_digit = foreach(industry_held_constant = unique(firms_and_industries$my_two_digit_NAICS), .combine = rbind_and_fill)%dopar%{
  industry_held_constant_share_1985 = firms_and_industries[calendaryear == 1985 & my_two_digit_NAICS == industry_held_constant, sum(firm_sales)/sum(firms_and_industries[calendaryear == 1985]$firm_sales)]
  industry_held_constant_share_2019 = firms_and_industries[calendaryear == 2019 & my_two_digit_NAICS == industry_held_constant, sum(firm_sales)/sum(firms_and_industries[calendaryear == 2019]$firm_sales)]
  to_allocate = 1 - industry_held_constant_share_1985
  all_firms_to_fix_industry_share = copy(firms_and_industries)
  all_firms_to_fix_industry_share[, firm_share_of_industry := firm_sales/sum(firm_sales), .(my_two_digit_NAICS, calendaryear)]
  all_firms_to_fix_industry_share[is.nan(firm_share_of_industry), firm_share_of_industry := 0]
  all_firms_to_fix_industry_share[my_two_digit_NAICS == industry_held_constant, industry_share_of_economy := industry_held_constant_share_1985]
  all_firms_to_fix_industry_share[my_two_digit_NAICS != industry_held_constant, total_industry_sales := sum(firm_sales),
                                  .(my_two_digit_NAICS, calendaryear)
                                  ][my_two_digit_NAICS != industry_held_constant, industry_share_of_economy := to_allocate * total_industry_sales / sum(firm_sales), calendaryear]
  data.table(rise_in_mwtw_if_industry_share_held_constant = all_firms_to_fix_industry_share[calendaryear == 2019, sum(firm_share_of_industry * industry_share_of_economy * firm_MW) / sum(firm_share_of_industry * industry_share_of_economy * firm_TW)]
             - all_firms_to_fix_industry_share[calendaryear == 1985, sum(firm_share_of_industry * industry_share_of_economy * firm_MW) / sum(firm_share_of_industry * industry_share_of_economy * firm_TW)],
             industry_held_constant = industry_held_constant,
             industry_share_1985 = industry_held_constant_share_1985,
             industry_share_2019 = industry_held_constant_share_2019,
             change_in_industry_size = industry_held_constant_share_2019 - industry_held_constant_share_1985,
             industry_2019_mwtw_ratio_above_average = firms_and_industries[calendaryear == 2019 & my_two_digit_NAICS == industry_held_constant, sum(firm_MW)/sum(firm_TW)] - firms_and_industries[calendaryear == 2019, sum(firm_MW)/sum(firm_TW)],
             industry_1985_mwtw_ratio_above_average = firms_and_industries[calendaryear == 1985 & my_two_digit_NAICS == industry_held_constant, sum(firm_MW)/sum(firm_TW)] - firms_and_industries[calendaryear == 1985, sum(firm_MW)/sum(firm_TW)])
}
stopCluster(clusters)
setkey(which_industries_two_digit,rise_in_mwtw_if_industry_share_held_constant)[]



over_time_decomposition_industry = foreach(yr = 1986:2019, .combine = rbind_and_fill)%do%{
  firms_stay_the_same = merge(firms_and_industries[calendaryear == yr-1], firms_and_industries[calendaryear == yr], all.x = F, all.y = F, by = c('GlobalCompanyKey', 'my_three_digit_NAICS'), suffixes = c('_old', '_new'))[, issue := sum(firm_sales_old) == 0 | sum(firm_sales_new) == 0, my_three_digit_NAICS][issue == F]
  firms_stay_the_same[, firm_share_of_industry_old := firm_sales_old/sum(firm_sales_old), my_three_digit_NAICS]
  firms_stay_the_same[, firm_share_of_industry_new := firm_sales_new/sum(firm_sales_new), my_three_digit_NAICS]
  # firms_stay_the_same[my_three_digit_NAICS == industry_held_constant, industry_share_of_economy_old := industry_held_constant_share_1985]
  # firms_stay_the_same[my_three_digit_NAICS == industry_held_constant, industry_share_of_economy_new := industry_held_constant_share_1985]
  # firms_stay_the_same[my_three_digit_NAICS != industry_held_constant, industry_share_of_economy_old := sum(firm_sales_old)/sum(firms_stay_the_same$firm_sales_old), my_three_digit_NAICS]
  # firms_stay_the_same[my_three_digit_NAICS != industry_held_constant, industry_share_of_economy_new := to_allocate * sum(firm_sales_new)/sum(firms_stay_the_same$firm_sales_new), my_three_digit_NAICS]
  firms_stay_the_same[, industry_share_of_economy_old := sum(firm_sales_old)/sum(firms_stay_the_same$firm_sales_old), my_three_digit_NAICS]
  firms_stay_the_same[, industry_share_of_economy_new := sum(firm_sales_new)/sum(firms_stay_the_same$firm_sales_new), my_three_digit_NAICS]
  initial = firms_stay_the_same[, sum(firm_share_of_industry_old * industry_share_of_economy_old * firm_MW_old) / sum(firm_share_of_industry_old * industry_share_of_economy_old * firm_TW_old)]
  change_firm_shares_of_industries = firms_stay_the_same[, sum(firm_share_of_industry_new * industry_share_of_economy_old * firm_MW_old) / sum(firm_share_of_industry_new * industry_share_of_economy_old * firm_TW_old)]
  change_industry_shares_of_economy = firms_stay_the_same[, sum(firm_share_of_industry_old * industry_share_of_economy_new * firm_MW_old) / sum(firm_share_of_industry_old * industry_share_of_economy_new * firm_TW_old)]
  change_both_shares_full_reallocation = firms_stay_the_same[, sum(firm_share_of_industry_new * industry_share_of_economy_new * firm_MW_old) / sum(firm_share_of_industry_new * industry_share_of_economy_new * firm_TW_old)]
  change_mw = firms_stay_the_same[, sum(firm_share_of_industry_old * industry_share_of_economy_old * firm_MW_new) / sum(firm_share_of_industry_old * industry_share_of_economy_old * firm_TW_new)]
  final = firms_stay_the_same[, sum(firm_share_of_industry_new * industry_share_of_economy_new * firm_MW_new) / sum(firm_share_of_industry_new * industry_share_of_economy_new * firm_TW_new)]
  # actual_mw_old = all_firms_to_fix_industry_share[calendaryear == yr-1, sum(firm_share_of_industry * industry_share_of_economy * firm_MW) / sum(firm_share_of_industry * industry_share_of_economy * firm_TW)]
  # actual_mw_new = all_firms_to_fix_industry_share[calendaryear == yr, sum(firm_share_of_industry * industry_share_of_economy * firm_MW) / sum(firm_share_of_industry * industry_share_of_economy * firm_TW)]
  actual_mw_old = firms[calendaryear == yr-1, sum(size * firm_MW) / sum(size * firm_TW)]
  actual_mw_new = firms[calendaryear == yr, sum(size * firm_MW) / sum(size * firm_TW)]
  data.table(year = yr, initial = initial, change_mw = change_mw, change_both_shares_full_reallocation = change_both_shares_full_reallocation,
             linear =  change_mw + change_both_shares_full_reallocation - initial, final = final,
             entry_exit = (actual_mw_new - actual_mw_old) - (final - initial)
  )[, `:=`(reallocation = change_both_shares_full_reallocation - initial,
           within_firm_rise = change_mw - initial,
           cross_term = final - linear,
           firm_reallocation = change_firm_shares_of_industries - initial,
           industry_reallocation = change_industry_shares_of_economy - initial,
           cross_term_firm_industry_reallocation = change_both_shares_full_reallocation + initial - change_firm_shares_of_industries - change_industry_shares_of_economy)][]
}
# over_time_decomposition_industry[, industry_held_constant := industry_held_constant][, total := reallocation + within_firm_rise + cross_term + entry_exit]
# 
# which_industries_cumulative = which_industries[, lapply(.SD,cumsum), by = industry_held_constant,
#                                                                       .SDcols = c('reallocation', 'firm_reallocation',
#                                                                                   'industry_reallocation', 'cross_term_firm_industry_reallocation',
#                                                                                   'within_firm_rise', 'cross_term', 'entry_exit', 'total'
#                                                                       )][, year := which_industries$year]
# setkey(which_industries_cumulative[year == 2019, .(industry_held_constant, total)],total)[]

over_time_decomposition_cumulative = over_time_decomposition_industry[, lapply(.SD,cumsum),
                                                             .SDcols = c('reallocation', 'firm_reallocation',
                                                                         'industry_reallocation', 'cross_term_firm_industry_reallocation',
                                                                         'within_firm_rise', 'cross_term', 'entry_exit'
                                                             )][, year := over_time_decomposition_industry$year]
write.xlsx(over_time_decomposition_cumulative, 'MWTW_decomposition.xlsx')

over_time_decomposition_long = melt(over_time_decomposition_industry[, .(year, reallocation, within_firm_rise,
                                                                 cross_term, entry_exit)],
                                     id.vars = 'year', variable.name = 'component', value.name = 'mwtw_rise')
over_time_decomposition_long[, cumulative := cumsum(mwtw_rise), component]
ggplot(over_time_decomposition_long, aes(x = year, y = cumulative, color = component)) + geom_line(size = 1, aes(linetype = component))

over_time_decomposition_long2 = melt(over_time_decomposition_industry[, .(year, firm_reallocation, industry_reallocation,
                                                                 cross_term_firm_industry_reallocation, within_firm_rise,
                                                                 cross_term, entry_exit)],
                                     id.vars = 'year', variable.name = 'component', value.name = 'mwtw_rise')
over_time_decomposition_long2[, cumulative := cumsum(mwtw_rise), component]
ggplot(over_time_decomposition_long2, aes(x = year, y = cumulative, color = component)) + geom_line(size = 1, aes(linetype = component))




#DLE
firms_and_industries = data[!my_three_digit_NAICS %in% c(221, 521, 522, 523, 524, 525) & !is.na(my_three_digit_NAICS) & !is.na(monopolywealth) &!is.na(totalwealth),
                            .(firm_MW = sum(monopolywealth * six_digit_ratio),
                              firm_TW = sum(totalwealth * six_digit_ratio),
                              firm_sales = sum(SalesTurnoverNet * six_digit_ratio),
                              DLE_markup = first(DLE_markup)),
                            .(GlobalCompanyKey, calendaryear, my_three_digit_NAICS)][firm_TW > 0]
firms_and_industries[, size := firm_sales/sum(firm_sales), calendaryear]
firms_and_industries[, mwtw_ratio := firm_MW/firm_TW]
firms_and_industries = firms_and_industries[!is.na(DLE_markup) & DLE_markup < 20 & DLE_markup > -1]

over_time_decomposition_DLE = foreach(yr = 1986:2019, .combine = rbind_and_fill)%do%{
  firms_stay_the_same = merge(firms_and_industries[calendaryear == yr-1], firms_and_industries[calendaryear == yr], all.x = F, all.y = F, by = c('GlobalCompanyKey', 'my_three_digit_NAICS'), suffixes = c('_old', '_new'))[, issue := sum(firm_sales_old) == 0 | sum(firm_sales_new) == 0, my_three_digit_NAICS][issue == F]
  firms_stay_the_same[, firm_share_of_industry_old := firm_sales_old/sum(firm_sales_old), my_three_digit_NAICS]
  firms_stay_the_same[, firm_share_of_industry_new := firm_sales_new/sum(firm_sales_new), my_three_digit_NAICS]
  firms_stay_the_same[, industry_share_of_economy_old := sum(firm_sales_old)/sum(firms_stay_the_same$firm_sales_old), my_three_digit_NAICS]
  firms_stay_the_same[, industry_share_of_economy_new := sum(firm_sales_new)/sum(firms_stay_the_same$firm_sales_new), my_three_digit_NAICS]
  initial = firms_stay_the_same[, wtd.mean(DLE_markup_old, firm_share_of_industry_old * industry_share_of_economy_old)]
  change_firm_shares_of_industries = firms_stay_the_same[, wtd.mean(DLE_markup_old, firm_share_of_industry_new * industry_share_of_economy_old)]
  change_industry_shares_of_economy = firms_stay_the_same[, wtd.mean(DLE_markup_old, firm_share_of_industry_old * industry_share_of_economy_new)]
  change_both_shares_full_reallocation = firms_stay_the_same[, wtd.mean(DLE_markup_old, firm_share_of_industry_new * industry_share_of_economy_new)]
  change_mw = firms_stay_the_same[, wtd.mean(DLE_markup_new, firm_share_of_industry_old * industry_share_of_economy_old)]
  final = firms_stay_the_same[, wtd.mean(DLE_markup_new, firm_share_of_industry_new * industry_share_of_economy_new)]
  actual_mw_old = firms_and_industries[calendaryear == yr-1, wtd.mean(DLE_markup, size)]
  actual_mw_new = firms_and_industries[calendaryear == yr, wtd.mean(DLE_markup, size)]
  data.table(year = yr, initial = initial, change_mw = change_mw, change_both_shares_full_reallocation = change_both_shares_full_reallocation,
             linear =  change_mw + change_both_shares_full_reallocation - initial, final = final,
             entry_exit = (actual_mw_new - actual_mw_old) - (final - initial)
  )[, `:=`(reallocation = change_both_shares_full_reallocation - initial,
           within_firm_rise = change_mw - initial,
           cross_term = final - linear,
           firm_reallocation = change_firm_shares_of_industries - initial,
           industry_reallocation = change_industry_shares_of_economy - initial,
           cross_term_firm_industry_reallocation = change_both_shares_full_reallocation + initial - change_firm_shares_of_industries - change_industry_shares_of_economy)][]
}


over_time_decomposition_long = melt(over_time_decomposition_DLE[, .(year, reallocation, within_firm_rise,
                                                                cross_term, entry_exit)],
                                    id.vars = 'year', variable.name = 'component', value.name = 'mwtw_rise')
over_time_decomposition_long[, cumulative := cumsum(mwtw_rise), component]
ggplot(over_time_decomposition_long, aes(x = year, y = cumulative, color = component)) + geom_line(size = 1, aes(linetype = component))

over_time_decomposition_long2 = melt(over_time_decomposition_DLE[, .(year, firm_reallocation, industry_reallocation,
                                                                 cross_term_firm_industry_reallocation, within_firm_rise,
                                                                 cross_term, entry_exit)],
                                     id.vars = 'year', variable.name = 'component', value.name = 'mwtw_rise')
over_time_decomposition_long2[, cumulative := cumsum(mwtw_rise), component]
ggplot(over_time_decomposition_long2, aes(x = year, y = cumulative, color = component)) + geom_line(size = 1, aes(linetype = component))



data[, biggest_six_digit_ratio := six_digit_ratio == max(six_digit_ratio), .(GlobalCompanyKey, calendaryear)][, adj_my_three_digit_NAICS := my_three_digit_NAICS * biggest_six_digit_ratio]
lets_look_at_firms = data[!my_three_digit_NAICS %in% c(221, 521, 522, 523, 524, 525) & !is.na(my_three_digit_NAICS) & !is.na(monopolywealth) &!is.na(totalwealth),
                            .(firm_MW = sum(monopolywealth * six_digit_ratio),
                              firm_TW = sum(totalwealth * six_digit_ratio),
                              firm_sales = sum(SalesTurnoverNet * six_digit_ratio),
                              DLE_markup = first(DLE_markup),
                              conm = first(conm),
                              industry = max(adj_my_three_digit_NAICS)),
                            .(GlobalCompanyKey, calendaryear)]
lets_look_at_firms[, mwtw := firm_MW / firm_TW]
lets_look_at_firms[, sales_1985 := sum(firm_sales * (calendaryear == 1985)), GlobalCompanyKey
                 ][, sales_share_1985 := sales_1985/sum(firm_sales * (calendaryear == 1985))
                 ][, mwtw_1985 := sum(mwtw * (calendaryear == 1985)), GlobalCompanyKey]
lets_look_at_firms[, sales_2019 := sum(firm_sales * (calendaryear == 2019)), GlobalCompanyKey
                 ][, sales_share_2019 := sales_2019/sum(firm_sales * (calendaryear == 2019))
                 ][, mwtw_2019 := sum(mwtw * (calendaryear == 2019)), GlobalCompanyKey]
p90_1985 = quantile(lets_look_at_firms[calendaryear == 1985, firm_sales], seq(0,1,0.1))['90%']
p90_2019 = quantile(lets_look_at_firms[calendaryear == 2019, firm_sales], seq(0,1,0.1))['90%']
asdf = setkey(lets_look_at_firms[sales_1985 > p90_1985 & sales_2019 > p90_2019 & calendaryear == 2019], sales_1985)[]
jkl = asdf[mwtw_2019 - mwtw_1985 > 0.3 & sales_share_1985 / sales_share_2019 > 0.5 & sales_share_1985 / sales_share_2019 < 2]
jkl[grep('RAYTHEON|LOCKHEED|NORTHROP|DYNAMICS', conm)][, mwtw_rise := mwtw_2019 - mwtw_1985][, .(sum(sales_share_1985), sum(sales_share_2019 - sales_share_1985), mean(mwtw_rise))][, growth := V2 / (V1 + V2)][]

p80_1985_mwtw = quantile(lets_look_at_firms[calendaryear == 1985, mwtw], seq(0,1,0.1))['60%']
p80_2019_mwtw = quantile(lets_look_at_firms[calendaryear == 2019, mwtw], seq(0,1,0.1))['60%']
asdf = setkey(lets_look_at_firms[mwtw_1985 > p80_1985_mwtw & mwtw_2019 > p80_2019_mwtw & calendaryear == 2019], sales_2019)[]
jkl = asdf[abs(mwtw_2019 - mwtw_1985) < 0.3 & sales_share_1985 / sales_share_2019 < 0.5]
asdf[conm == 'INTEL CORP', sales_share_2019/sales_share_1985]
