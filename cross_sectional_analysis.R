library(data.table)
library(foreach)
library(iterators)
library(snow)
library(doSNOW)
library(Hmisc)
library(plm)
library(multiwayvcov)
library(lmtest)
library(stringr)
library(openxlsx)
library(readxl)
library(RCurl)
library(gtools)
library(ggplot2)

# import the function from repository
url_robust = "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)


rbind_and_fill = function(...) rbind(...,fill=T)

z_score = function(x, weights){
  (x - wtd.mean(x, weights = weights))/sqrt(wtd.var(x, weights = weights))
}


getCharCols = function(x) {
  jkl = readLines(x,n = 2)[2]
  cols = strsplit(jkl,',')[[1]]
  grep('"',cols)
}

fread_and_getCharCols = function(x) {
  fread(x, colClasses = list(character = getCharCols(x)))
}


setwd("C:/Users/Nathan/Downloads/Compustat")
data = fread_and_getCharCols('foranalysis.csv')

# SDC_MA_data = readRDS('SDC_data.rds')
data[, cusip6 := substr(cusip, 1, 6)]
setkey(data, cusip6)
# setkey(SDC_MA_data, `Acquiror Ultimate Parent CUSIP`)
# merged = SDC_MA_data[data]
data[, software_patents := NULL]
patent_data = readRDS('patent_data_for_analysis.RDS')
data[patent_data, `:=`(all_patents = all_patents, software_patents = software_patents),
     on = c(cusip6 = 'owner', calendaryear = 'possession_year')]
data[is.na(software_patents), `:=`(software_patents = 0, all_patents = 0)]

tfp_data = fread('TFPData_updated_2019.csv')
data[tfp_data, on = c(DataYearFiscal = 'fyear', GlobalCompanyKey = 'gvkey'), tfp := i.TFP]

data[, mwv := monopolywealth / MktVal]
data[, mwtw := monopolywealth / totalwealth]

data[!is.na(SalesTurnoverNet) & !is.na(CostofGoodsSold) & !is.na(kexp),
     industry_DLE_markup := sum(SalesTurnoverNet * six_digit_ratio) / sum((CostofGoodsSold + kexp) * six_digit_ratio),
     .(naics, calendaryear)]
data[!is.na(SalesTurnoverNet) & !is.na(CostofGoodsSold) & !is.na(kexp) & !is.na(SellingGeneralandAdministrativeExpense),
     industry_DLE_profit := sum((SalesTurnoverNet - CostofGoodsSold - SellingGeneralandAdministrativeExpense - kexp) * six_digit_ratio) / sum(SalesTurnoverNet * six_digit_ratio),
     .(naics, calendaryear)]
data[!is.na(SalesTurnoverNet) & !is.na(CostofGoodsSold) & !is.na(kexp) & CostofGoodsSold + kexp != 0,
     firm_DLE_markup := SalesTurnoverNet / (CostofGoodsSold + kexp),
     .(GlobalCompanyKey, calendaryear)]
data[!is.na(SalesTurnoverNet) & !is.na(CostofGoodsSold) & !is.na(kexp) & CostofGoodsSold + kexp != 0,
     firm_DLE_profit := SalesTurnoverNet / (CostofGoodsSold + SellingGeneralandAdministrativeExpense + kexp),
     .(GlobalCompanyKey, calendaryear)]
data[!is.na(SalesTurnoverNet) & !is.na(CostofGoodsSold) & !is.na(kexp) & CostofGoodsSold + kexp != 0,
     industry_total_sales := sum(SalesTurnoverNet * six_digit_ratio),
     .(three_digit_NAICS, calendaryear)]
data[!is.na(SalesTurnoverNet) & !is.na(CostofGoodsSold) & !is.na(kexp) & CostofGoodsSold + kexp != 0,
     sales_share_3d := (SalesTurnoverNet * six_digit_ratio) / sum(SalesTurnoverNet * six_digit_ratio),
     .(three_digit_NAICS, calendaryear)]
data[,
     industry_3d_DLE_markup_salesw := sum(sales_share_3d * firm_DLE_markup, na.rm = T),
     .(three_digit_NAICS, calendaryear)]
data[,
     industry_3d_DLE_profit_salesw := sum(sales_share_3d * firm_DLE_profit, na.rm = T),
     .(three_digit_NAICS, calendaryear)]
data[!is.na(monopolywealth) & !is.na(totalwealth) & !is.na(MktVal),
     `:=`(industry_3d_mwtw = sum(monopolywealth)/sum(totalwealth),
          industry_3d_mwv = sum(monopolywealth)/sum(MktVal)),
     .(three_digit_NAICS, calendaryear)]

# 
# NAICS_codes_to_names = data.table(read_excel('2017_NAICS_Codes.xlsx'))
# suppressWarnings(NAICS_codes_to_names[,Code := as.numeric(str_pad(Code, width=6, side='right', pad='0'))])
# NAICS_codes_to_names = rbind(NAICS_codes_to_names[!is.na(Code)],
#                              data.table(Code = c(233000, 234000, 235000),
#                                         Name = c('Building, Developing, and General Contracting',
#                                                  'Heavy Construction',
#                                                  'Special Trade Contractors'
# )))
# 
# asdf = unique(data[!is.na(industry_3d_mwtw) & !is.na(industry_3d_DLE_markup_salesw) & !is.na(three_digit_NAICS) & !is.na(industry_total_sales),
#                    .(industry_3d_DLE_markup_salesw, industry_3d_DLE_profit_salesw, industry_3d_mwtw, industry_3d_mwv, three_digit_NAICS, calendaryear, industry_total_sales)
#                 ])[NAICS_codes_to_names, on = c(three_digit_NAICS = 'Code'), NAICS_3d_description := i.Name]
# 
# asdf[, size := industry_total_sales / sum(industry_total_sales, na.rm = T), calendaryear]
# correlationtable = asdf[,  cov.wt(cbind(industry_3d_mwv, industry_3d_mwtw, industry_3d_DLE_markup_salesw, industry_3d_DLE_profit_salesw),
#                                   wt = size, cor = T)$cor[1:2,3:4], .(calendaryear)]
# correlationtable[, variables := rep(c('MW/Value, Markup', 'MW/TotalW, Markup', 'MW/Value, Profit Rate', 'MW/TotalW, Profit Rate'), times = nrow(correlationtable) / 4)]
# library(ggplot2)
# library(cowplot)
# p = ggplot(correlationtable[calendaryear>1961], aes(x=calendaryear, y=V1, color = variables, linetype = variables)) +
#   geom_line(size = 1.05) +
#   theme_cowplot() +
#   theme(legend.position="bottom")+
#   theme(legend.title = element_blank())+
#   guides(color=guide_legend(nrow=2,byrow=TRUE)) +
#   xlab('Year') +
#   ylab('Correlation') +
#   ggtitle('Correlations Between Monopoly\n Wealth and Markups', subtitle = '(By Industry, Weighted)')
# p
# ggsave('Markup Monopoly Wealth Industry Correlations.png')
# 
# jkl = merge(asdf[calendaryear == 1985],asdf[calendaryear == 2015],
#             by = 'three_digit_NAICS', all.x = F, all.y = T,
#             suffixes = c('1985', '2015'))
# # jkl[, size_1985 := industry_total_sales1985 / sum(industry_total_sales1985, na.rm = T)]
# # jkl[, size_2015 := industry_total_sales2015 / sum(industry_total_sales2015, na.rm = T)]
# setorder(jkl, -industry_3d_mwtw2015)
# setcolorder(jkl, c(names(jkl)[1:7],'size_1985', names(jkl)[8:15], 'size_2015'))
# jkl[, `:=`(industry_total_sales1985 = NULL, industry_total_sales2015 = NULL, NAICS_3d_description1985 = NULL)]
# write.xlsx(jkl, 'industry_markups_and_monopoly_wealth.xlsx')  
# 
# 
# 
# firmdata = unique(
#   data[!is.na(mwv) & !is.na(mwtw) & !is.na(firm_DLE_markup) & !is.na(firm_DLE_profit) & !is.na(SalesTurnoverNet) & SalesTurnoverNet > 0],
#   by = c('GlobalCompanyKey', 'calendaryear'))
# firmdata[, sales_share := SalesTurnoverNet / sum(SalesTurnoverNet), calendaryear]
# correlationtable_firm = firmdata[,  cov.wt(cbind(mwv, mwtw, firm_DLE_markup, firm_DLE_profit),
#                                   wt = sales_share, cor = T)$cor[1:2,3:4], .(calendaryear)]
# correlationtable_firm[, variables := rep(c('MW/Value, Markup', 'MW/TotalW, Markup', 'MW/Value, Profit Rate', 'MW/TotalW, Profit Rate'), times = nrow(correlationtable) / 4)]
# p = ggplot(correlationtable_firm[calendaryear>1961], aes(x=calendaryear, y=V1, color = variables, linetype = variables)) +
#   geom_line(size = 1.05) +
#   theme_cowplot() +
#   theme(legend.position="bottom", legend.title = element_blank())+
#   guides(color=guide_legend(nrow=2,byrow=TRUE)) +
#   xlab('Year') +
#   ylab('Correlation') +
#   ggtitle('Correlations Between Monopoly\n Wealth and Markups', subtitle = '(By Firm, Weighted)')
# p
# ggsave('Markup Monopoly Wealth Firm Correlations.png')
# 
# 







IT_employment_data = data[!is.na(IT_employment),
                                .(
                                  monopolywealth = sum(monopolywealth*six_digit_ratio, na.rm = T),
                                  totalwealth = sum(totalwealth*six_digit_ratio, na.rm = T),
                                  MktVal = sum(MktVal * six_digit_ratio, na.rm = T),
                                  DLE_markup = median(industry_DLE_markup, na.rm = T),
                                  DLE_profit = median(industry_DLE_profit, na.rm = T),
                                  IT_employment = median(IT_employment, na.rm = T),
                                  SalesTurnoverNet = sum(SalesTurnoverNet * six_digit_ratio, na.rm = T)
                                  ),
                                .(naics, calendaryear)]
# IT_employment_data[, mwv := monopolywealth / MktVal]
# IT_employment_data[, mwtw := monopolywealth / totalwealth]
# # IT_employment_data[, DLE_markup := SalesTurnoverNet / (CostofGoodsSold + kexp)]
# # IT_employment_data[, Traina_markup := SalesTurnoverNet / (CostofGoodsSold + SellingGeneralandAdministrativeExpense + kexp)]
# # IT_employment_data[, DLE_profit := (SalesTurnoverNet - CostofGoodsSold -
# #                                 SellingGeneralandAdministrativeExpense - kexp) / SalesTurnoverNet]
# 
# IT_employment_data[mwv > -6 & mwv < 6 &is.finite(IT_employment),
#                    mwv_normalized := z_score(mwv, SalesTurnoverNet)]
# IT_employment_data[mwtw > -1 & mwtw < 2 &is.finite(IT_employment),
#                    mwtw_normalized := z_score(mwtw, SalesTurnoverNet)]
# IT_employment_data[DLE_markup > 0 & DLE_markup < 6 &is.finite(IT_employment),
#                    DLE_markup_normalized := z_score(DLE_markup, SalesTurnoverNet)]
# IT_employment_data[DLE_profit > -1 & DLE_markup < 2 &is.finite(IT_employment),
#                    DLE_profit_normalized := z_score(DLE_profit, SalesTurnoverNet)]
# IT_employment_data[, IT_employment_normalized := z_score(IT_employment, SalesTurnoverNet)]
# 
# IT_employment_data[,factor_naics := factor(as.character(naics))]
# 
# IT_employment_data[, consumer_manufacturing := substr(as.character(naics),1,3) %in% c('311', '312', '315')]
# 
# plm.model1 = plm(mwv_normalized ~ IT_employment_normalized + consumer_manufacturing,
#                 data = IT_employment_data,
#                 # weights = SalesTurnoverNet,
#                 model = 'pooling',
#                 index = c('factor_naics', 'calendaryear'))
# summary(plm.model1)
# plm.model2 = plm(mwtw_normalized ~ IT_employment_normalized + consumer_manufacturing,
#                 data = IT_employment_data,
#                 # weights = SalesTurnoverNet,
#                 model = 'pooling',
#                 index = c('factor_naics', 'calendaryear'))
# summary(plm.model2)
# plm.model3 = plm(DLE_markup_normalized ~ IT_employment_normalized + consumer_manufacturing,
#                 data = IT_employment_data,
#                 # weights = SalesTurnoverNet,
#                 model = 'pooling',
#                 index = c('factor_naics', 'calendaryear'))
# summary(plm.model3)
# plm.model4 = plm(DLE_profit_normalized ~ IT_employment_normalized + consumer_manufacturing,
#                 data = IT_employment_data,
#                 # weights = SalesTurnoverNet,
#                 model = 'pooling',
#                 index = c('factor_naics', 'calendaryear'))
# summary(plm.model4)
# # asdf = lm(mwv_normalized ~ IT_employment_normalized,
# #            data = IT_employment_data,
# #            weights = SalesTurnoverNet)
# emp_normalized_regression_ouptuts = foreach(i = 1:4, .combine = rbind)%do%{
#   reg = eval(parse(text=paste0('plm.model',i)))
#   data.table(dv = gsub('_normalized','',as.character(reg$call[[2]][[2]])),
#              iv = gsub('_normalized','',as.character(reg$call[[2]][[3]][[2]])),
#              intercept = reg$coefficients['(Intercept)'],
#              IT_employment = reg$coefficients[2],
#              consumer_manufacturing = reg$coefficients[3],
#              se = coef(summary(reg))[2,2] * sqrt(2317)/sqrt(2317/19),
#              p_val = 2*(1 - pt(abs(
#                reg$coefficients[2]/
#                  (coef(summary(reg))[2,2] * sqrt(2317)/sqrt(2317/19))
#                ),120)),
#              r_squared = summary(reg)$r.squared[1]
#              )
# }
# 
# write.xlsx(emp_normalized_regression_ouptuts,
#            'IT_employment_regression_ouptuts.xlsx')


# ivs = c('software_patents_rolling_5', 'IT_employment', 'it_input_elasticity', 'it_capital_1987')

ivs = c('software_patents', 'all_patents')
dvs = c('mwv', 'mwtw', 'DLE_markup', 'Traina_markup') #'DLE_profit', 

data[is.na(software_patents_rolling_5), software_patents_rolling_5 := 0]

specifications = data.table(merge(ivs,dvs,allow.cartesian = T))
setnames(specifications,c('iv','dv'))

GDP_deflator = fread('GDPDEF (1).csv')
GDP_deflator[, calendaryear := year(DATE)]
Inv_deflator = fread('Investment Deflator FRED.csv')
Inv_deflator[, calendaryear := year(DATE)]
data[AssetsTotal < 0, AssetsTotal := 0]
data[totalwealth < 0, totalwealth := 0]
data[GDP_deflator, on = 'calendaryear', gdp_def := i.GDPDEF]
data[Inv_deflator, on = 'calendaryear', InvDef := i.InvDef]
data[, `:=`(software_patents_assets = software_patents / (0.01 + AssetsTotal/InvDef),
            all_patents_assets = all_patents / (0.01 + AssetsTotal/InvDef))]

data[, software_capital_ratio_average := rowMeans(.SD, na.rm = T),
     .SDcols = names(data)[grep('software_capital_ratio_[0-9]', names(data))]]
median_it_use = wtd.quantile(data[it_producing == 0, software_capital_ratio_average], weights = data[it_producing == 0, six_digit_ratio * SalesTurnoverNet], na.rm = T)[3]
data[is.na(software_capital_ratio_average), software_capital_ratio_average := wtd.mean(software_capital_ratio_average, weights = six_digit_ratio * SalesTurnoverNet)]
data[, high_it_use := it_producing == 0 & software_capital_ratio_average >= median_it_use]

roll_year_function = function(x) {
  if(x == 1987) out = 1950:1987
  if(x > 1987 & x < 2018) out = x
  if(x == 2018) out = 2018:2020
  out
}
foreach(yr = 1987:2018)%do%{
  
  eval(parse(text = paste0('median_it_use_for_the_year = wtd.quantile(data[calendaryear == yr & it_producing == 0, software_capital_ratio_',yr,'], weights = data[calendaryear == yr & it_producing == 0, six_digit_ratio * SalesTurnoverNet], na.rm = T)[3]')))
  eval(parse(text = paste0('mean_it_use_for_the_year = wtd.mean(data[calendaryear == yr, software_capital_ratio_',yr,'], weights = data[calendaryear == yr, six_digit_ratio * SalesTurnoverNet], na.rm = T)')))
  eval(parse(text = paste0('data[calendaryear %in% ',roll_year_function(yr),'& is.na(software_capital_ratio_',yr,'), software_capital_ratio_',yr,' := mean_it_use_for_the_year]')))
  eval(parse(text = paste0('data[calendaryear %in% ',roll_year_function(yr),', high_it_use_for_the_year := it_producing == 0 & software_capital_ratio_', yr, ' >= median_it_use_for_the_year]')))
  eval(parse(text = paste0('data[calendaryear %in% ',roll_year_function(yr),', it_use_for_the_year_continuous := software_capital_ratio_', yr, ' - mean_it_use_for_the_year]')))
  NULL
}
data[, airline := true_six_digit_NAICS %/% 100 == 4811]
data[, beverage := true_six_digit_NAICS %/% 100 == 3121]
data[, telecom := true_six_digit_NAICS %/% 1000 == 517]
data[is.na(SalesTurnoverNet) | SalesTurnoverNet < 0, SalesTurnoverNet := 0]

normalized = data[mwtw > -1 & mwtw < 2 & AssetsTotal > 0 & !((six_digit_NAICS %/% 1000) %in% c(221, 521, 522, 523, 524, 525)),
                  .(mwtw = z_score(mwtw, weights = six_digit_ratio * SalesTurnoverNet),
                    all_patents_assets = z_score(all_patents_assets, weights = six_digit_ratio * SalesTurnoverNet),
                    software_patents_assets = z_score(software_patents_assets, weights = six_digit_ratio * SalesTurnoverNet),
                    six_digit_ratio, it_producing, software_capital_ratio_average, airline, beverage, telecom, GlobalCompanyKey, high_it_use, six_digit_NAICS, SalesTurnoverNet)]
normalized_Traina = data[data.table::between(Traina_markup, 0, 5) & AssetsTotal > 0 & !((six_digit_NAICS %/% 1000) %in% c(221, 521, 522, 523, 524, 525)),
                  .(Traina_markup = z_score(Traina_markup, weights = six_digit_ratio * SalesTurnoverNet),
                    all_patents_assets = z_score(all_patents_assets, weights = six_digit_ratio * SalesTurnoverNet),
                    software_patents_assets = z_score(software_patents_assets, weights = six_digit_ratio * SalesTurnoverNet),
                    six_digit_ratio, it_producing, software_capital_ratio_average, airline, beverage, telecom, GlobalCompanyKey, high_it_use, six_digit_NAICS, SalesTurnoverNet)]
# mod1 =  lm(mwtw ~ software_patents_assets, data = as.data.frame(data[!((six_digit_NAICS %/% 1000) %in% c(221, 521, 522, 523, 524, 525)) & mwtw > -1 & mwtw < 2, .(software_patents_assets, all_patents_assets, mwtw, Traina_markup,it_producing, software_capital_ratio_average, airline, beverage, telecom, GlobalCompanyKey, high_it_use, six_digit_NAICS, six_digit_ratio, SalesTurnoverNet)]), weights = six_digit_ratio * SalesTurnoverNet)
# mod2 =  lm(mwtw ~ all_patents_assets, data = as.data.frame(data[!((six_digit_NAICS %/% 1000) %in% c(221, 521, 522, 523, 524, 525)) & mwtw > -1 & mwtw < 2, .(software_patents_assets, all_patents_assets, mwtw, Traina_markup,it_producing, software_capital_ratio_average, airline, beverage, telecom, GlobalCompanyKey, high_it_use, six_digit_NAICS, six_digit_ratio, SalesTurnoverNet)]), weights = six_digit_ratio * SalesTurnoverNet)
# mod3 =  lm(mwtw ~ software_patents_assets + all_patents_assets + it_producing + software_capital_ratio_average + airline + beverage + telecom, data = as.data.frame(data[!((six_digit_NAICS %/% 1000) %in% c(221, 521, 522, 523, 524, 525)) & mwtw > -1 & mwtw < 2, .(software_patents_assets, all_patents_assets, mwtw, Traina_markup,it_producing, software_capital_ratio_average, airline, beverage, telecom, GlobalCompanyKey, high_it_use, six_digit_NAICS, six_digit_ratio, SalesTurnoverNet)]), weights = six_digit_ratio * SalesTurnoverNet)
# mod3_ =  lm(mwtw ~ software_patents_assets + all_patents_assets + it_producing + high_it_use + airline + beverage + telecom, data = as.data.frame(data[calendaryear > 1986 & !((six_digit_NAICS %/% 1000) %in% c(221, 521, 522, 523, 524, 525)) & mwtw > -1 & mwtw < 2, .(software_patents_assets, all_patents_assets, mwtw, Traina_markup,it_producing, software_capital_ratio_average, airline, beverage, telecom, GlobalCompanyKey, high_it_use, six_digit_NAICS, six_digit_ratio, SalesTurnoverNet)]), weights = six_digit_ratio * SalesTurnoverNet)
# mod3_good =  lm(mwtw ~ software_patents_assets + all_patents_assets + it_producing + high_it_use_for_the_year + airline + beverage + telecom, data = as.data.frame(data[calendaryear > 1986 & !((six_digit_NAICS %/% 1000) %in% c(221, 521, 522, 523, 524, 525)) & mwtw > -1 & mwtw < 2, .(software_patents_assets, all_patents_assets, mwtw, Traina_markup,it_producing, software_capital_ratio_average, airline, beverage, telecom, GlobalCompanyKey, high_it_use, six_digit_NAICS, six_digit_ratio, SalesTurnoverNet, high_it_use_for_the_year)]), weights = six_digit_ratio * SalesTurnoverNet)
# mod3_not_missing_employment =  lm(mwtw ~ software_patents_assets + all_patents_assets + it_producing + high_it_use_for_the_year + airline + beverage + telecom, data = as.data.frame(data[!is.na(IT_employment) & calendaryear > 1986 & !((six_digit_NAICS %/% 1000) %in% c(221, 521, 522, 523, 524, 525)) & mwtw > -1 & mwtw < 2, .(software_patents_assets, all_patents_assets, mwtw, Traina_markup,it_producing, software_capital_ratio_average, airline, beverage, telecom, GlobalCompanyKey, high_it_use, six_digit_NAICS, six_digit_ratio, SalesTurnoverNet, high_it_use_for_the_year, IT_employment)]), weights = six_digit_ratio * SalesTurnoverNet)
# mod3_with_employment =  lm(mwtw ~ IT_employment + software_patents_assets + all_patents_assets + it_producing + high_it_use_for_the_year + airline + beverage + telecom, data = as.data.frame(data[!is.na(IT_employment) & calendaryear > 1986 & !((six_digit_NAICS %/% 1000) %in% c(221, 521, 522, 523, 524, 525)) & mwtw > -1 & mwtw < 2, .(software_patents_assets, all_patents_assets, mwtw, Traina_markup,it_producing, software_capital_ratio_average, airline, beverage, telecom, GlobalCompanyKey, high_it_use, six_digit_NAICS, six_digit_ratio, SalesTurnoverNet, high_it_use_for_the_year, IT_employment)]), weights = six_digit_ratio * SalesTurnoverNet)
# mod4 =  lm(mwtw ~ software_patents_assets + all_patents_assets + it_producing + software_capital_ratio_average + high_it_use + airline + beverage + telecom +
#              all_patents_assets * software_capital_ratio_average + software_patents_assets * software_capital_ratio_average,
#            data = as.data.frame(data[!((six_digit_NAICS %/% 1000) %in% c(221, 521, 522, 523, 524, 525)) & mwtw > -1 & mwtw < 2, .(software_patents_assets, all_patents_assets, mwtw, Traina_markup,it_producing, software_capital_ratio_average, airline, beverage, telecom, GlobalCompanyKey, high_it_use, six_digit_NAICS, six_digit_ratio, SalesTurnoverNet)]), weights = six_digit_ratio * SalesTurnoverNet)
# mod5 =  lm(mwtw ~ software_patents_assets + all_patents_assets + it_producing + software_capital_ratio_average + high_it_use + airline + beverage + telecom +
#              all_patents_assets * software_capital_ratio_average + software_patents_assets * software_capital_ratio_average +
#              all_patents_assets * high_it_use + software_patents_assets * high_it_use +
#              all_patents_assets * it_producing + software_patents_assets * it_producing + it_producing * software_capital_ratio_average,
#            data = as.data.frame(data[!((six_digit_NAICS %/% 1000) %in% c(221, 521, 522, 523, 524, 525)) & mwtw > -1 & mwtw < 2, .(software_patents_assets, all_patents_assets, mwtw, Traina_markup,it_producing, software_capital_ratio_average, airline, beverage, telecom, GlobalCompanyKey, high_it_use, six_digit_NAICS, six_digit_ratio, SalesTurnoverNet)]), weights = six_digit_ratio * SalesTurnoverNet)
# mod6 =  lm(Traina_markup ~ software_patents_assets, data = as.data.frame(data[!((six_digit_NAICS %/% 1000) %in% c(221, 521, 522, 523, 524, 525)) & data.table::between(Traina_markup, 0, 5), .(software_patents_assets, all_patents_assets, mwtw, Traina_markup,it_producing, software_capital_ratio_average, airline, beverage, telecom, GlobalCompanyKey, high_it_use, six_digit_NAICS, six_digit_ratio, SalesTurnoverNet)]), weights = six_digit_ratio * SalesTurnoverNet)
# mod7 =  lm(Traina_markup ~ all_patents_assets, data = as.data.frame(data[!((six_digit_NAICS %/% 1000) %in% c(221, 521, 522, 523, 524, 525)) & data.table::between(Traina_markup, 0, 5), .(software_patents_assets, all_patents_assets, mwtw, Traina_markup,it_producing, software_capital_ratio_average, airline, beverage, telecom, GlobalCompanyKey, high_it_use, six_digit_NAICS, six_digit_ratio, SalesTurnoverNet)]), weights = six_digit_ratio * SalesTurnoverNet)
# mod8 =  lm(Traina_markup ~ software_patents_assets + all_patents_assets + it_producing + software_capital_ratio_average + airline + beverage + telecom, data = as.data.frame(data[!((six_digit_NAICS %/% 1000) %in% c(221, 521, 522, 523, 524, 525)) & data.table::between(Traina_markup, 0, 5), .(software_patents_assets, all_patents_assets, mwtw, Traina_markup,it_producing, software_capital_ratio_average, airline, beverage, telecom, GlobalCompanyKey, high_it_use, six_digit_NAICS, six_digit_ratio, SalesTurnoverNet)]), weights = six_digit_ratio * SalesTurnoverNet)
# mod8_ =  lm(Traina_markup ~ software_patents_assets + all_patents_assets + high_it_use + software_capital_ratio_average + airline + beverage + telecom, data = as.data.frame(data[!((six_digit_NAICS %/% 1000) %in% c(221, 521, 522, 523, 524, 525)) & data.table::between(Traina_markup, 0, 5), .(software_patents_assets, all_patents_assets, mwtw, Traina_markup,it_producing, software_capital_ratio_average, airline, beverage, telecom, GlobalCompanyKey, high_it_use, six_digit_NAICS, six_digit_ratio, SalesTurnoverNet)]), weights = six_digit_ratio * SalesTurnoverNet)
# mod9 =  lm(Traina_markup ~ software_patents_assets + all_patents_assets + it_producing + software_capital_ratio_average + airline + beverage + telecom +
#              all_patents_assets * software_capital_ratio_average + software_patents_assets * software_capital_ratio_average,
#            data = as.data.frame(data[!((six_digit_NAICS %/% 1000) %in% c(221, 521, 522, 523, 524, 525)) & data.table::between(Traina_markup, 0, 5), .(software_patents_assets, all_patents_assets, mwtw, Traina_markup,it_producing, software_capital_ratio_average, airline, beverage, telecom, GlobalCompanyKey, high_it_use, six_digit_NAICS, six_digit_ratio, SalesTurnoverNet)]), weights = six_digit_ratio * SalesTurnoverNet)
# mod10 =  lm(Traina_markup ~ software_patents_assets + all_patents_assets + it_producing + software_capital_ratio_average + high_it_use + airline + beverage + telecom +
#              all_patents_assets * software_capital_ratio_average + software_patents_assets * software_capital_ratio_average +
#              all_patents_assets * high_it_use + software_patents_assets * high_it_use +
#              all_patents_assets * it_producing + software_patents_assets * it_producing + it_producing * software_capital_ratio_average,
#            data = as.data.frame(data[!((six_digit_NAICS %/% 1000) %in% c(221, 521, 522, 523, 524, 525)) & data.table::between(Traina_markup, 0, 5), .(software_patents_assets, all_patents_assets, mwtw, Traina_markup,it_producing, software_capital_ratio_average, airline, beverage, telecom, GlobalCompanyKey, high_it_use, six_digit_NAICS, six_digit_ratio, SalesTurnoverNet)]), weights = six_digit_ratio * SalesTurnoverNet)
# mod11 =  lm(mwtw ~ software_patents_assets, data = as.data.frame(normalized), weights = six_digit_ratio * SalesTurnoverNet)
# mod12 =  lm(mwtw ~ all_patents_assets, data = as.data.frame(normalized), weights = six_digit_ratio * SalesTurnoverNet)
# mod13 =  lm(mwtw ~ software_patents_assets + all_patents_assets + it_producing + software_capital_ratio_average + airline + beverage + telecom, data = as.data.frame(normalized), weights = six_digit_ratio * SalesTurnoverNet)
# mod13_ =  lm(mwtw ~ software_patents_assets + all_patents_assets + high_it_use + software_capital_ratio_average + airline + beverage + telecom, data = as.data.frame(normalized), weights = six_digit_ratio * SalesTurnoverNet)
# mod14 =  lm(mwtw ~ software_patents_assets + all_patents_assets + it_producing + software_capital_ratio_average + airline + beverage + telecom +
#              all_patents_assets * software_capital_ratio_average + software_patents_assets * software_capital_ratio_average,
#            data = as.data.frame(normalized), weights = six_digit_ratio * SalesTurnoverNet)
# mod15 =  lm(mwtw ~ software_patents_assets + all_patents_assets + it_producing + software_capital_ratio_average + high_it_use + airline + beverage + telecom +
#              all_patents_assets * software_capital_ratio_average + software_patents_assets * software_capital_ratio_average +
#              all_patents_assets * high_it_use + software_patents_assets * high_it_use +
#              all_patents_assets * it_producing + software_patents_assets * it_producing + it_producing * software_capital_ratio_average,
#            data = as.data.frame(normalized), weights = six_digit_ratio * SalesTurnoverNet)
# mod16 =  lm(Traina_markup ~ software_patents_assets, data = as.data.frame(normalized_Traina), weights = six_digit_ratio * SalesTurnoverNet)
# mod17 =  lm(Traina_markup ~ all_patents_assets, data = as.data.frame(normalized_Traina), weights = six_digit_ratio * SalesTurnoverNet)
# mod18 =  lm(Traina_markup ~ software_patents_assets + all_patents_assets + it_producing + software_capital_ratio_average + airline + beverage + telecom, data = as.data.frame(normalized_Traina), weights = six_digit_ratio * SalesTurnoverNet)
# mod18_ =  lm(Traina_markup ~ software_patents_assets + all_patents_assets + high_it_use + software_capital_ratio_average + airline + beverage + telecom, data = as.data.frame(normalized_Traina), weights = six_digit_ratio * SalesTurnoverNet)
# mod19 =  lm(Traina_markup ~ software_patents_assets + all_patents_assets + it_producing + software_capital_ratio_average + airline + beverage + telecom +
#              all_patents_assets * software_capital_ratio_average + software_patents_assets * software_capital_ratio_average,
#            data = as.data.frame(normalized_Traina), weights = six_digit_ratio * SalesTurnoverNet)
# mod20 =  lm(Traina_markup ~ software_patents_assets + all_patents_assets + it_producing + software_capital_ratio_average + high_it_use + airline + beverage + telecom +
#              all_patents_assets * software_capital_ratio_average + software_patents_assets * software_capital_ratio_average +
#              all_patents_assets * high_it_use + software_patents_assets * high_it_use +
#              all_patents_assets * it_producing + software_patents_assets * it_producing + it_producing * software_capital_ratio_average,
#            data = as.data.frame(normalized_Traina), weights = six_digit_ratio * SalesTurnoverNet)

# mods = list(mod1, mod2, mod3, mod3_, mod4, mod5, mod6, mod7, mod8, mod8_, mod9, mod10, mod11, mod12, mod13, mod13_, mod14, mod15, mod16, mod17, mod18, mod18_, mod19, mod20)
# data8089 = data[calendaryear >= 1980 & calendaryear <= 1989 & 
#                   !((true_six_digit_NAICS %/% 1000) %in% c(221, 521, 522, 523, 524, 525)) & mwtw > -1 & mwtw < 2,
#                 .(software_patents_assets, all_patents_assets, mwtw, Traina_markup,it_producing, software_capital_ratio_average, airline, beverage, telecom, GlobalCompanyKey, high_it_use, high_it_use_for_the_year, true_six_digit_NAICS, six_digit_ratio, SalesTurnoverNet, IT_employment, NAICS.Title)]
# data9099 = data[calendaryear >= 1990 & calendaryear <= 1999 & 
#                   !((true_six_digit_NAICS %/% 1000) %in% c(221, 521, 522, 523, 524, 525)) & mwtw > -1 & mwtw < 2,
#                 .(software_patents_assets, all_patents_assets, mwtw, Traina_markup,it_producing, software_capital_ratio_average, airline, beverage, telecom, GlobalCompanyKey, high_it_use, high_it_use_for_the_year, true_six_digit_NAICS, six_digit_ratio, SalesTurnoverNet, IT_employment, NAICS.Title)]
# data0009 = data[calendaryear >= 2000 & calendaryear <= 2009 & 
#                   !((true_six_digit_NAICS %/% 1000) %in% c(221, 521, 522, 523, 524, 525)) & mwtw > -1 & mwtw < 2,
#                 .(software_patents_assets, all_patents_assets, mwtw, Traina_markup,it_producing, software_capital_ratio_average, airline, beverage, telecom, GlobalCompanyKey, high_it_use, high_it_use_for_the_year, true_six_digit_NAICS, six_digit_ratio, SalesTurnoverNet, IT_employment, NAICS.Title)]
# data1019 = data[calendaryear >= 2010 & calendaryear <= 2019 & 
#                   !((true_six_digit_NAICS %/% 1000) %in% c(221, 521, 522, 523, 524, 525)) & mwtw > -1 & mwtw < 2,
#                 .(software_patents_assets, all_patents_assets, mwtw, Traina_markup,it_producing, software_capital_ratio_average, airline, beverage, telecom, GlobalCompanyKey, high_it_use, high_it_use_for_the_year, true_six_digit_NAICS, six_digit_ratio, SalesTurnoverNet, IT_employment, NAICS.Title)]
# 
# mod80_1 =  lm(mwtw ~ software_patents_assets + all_patents_assets + it_producing + high_it_use_for_the_year + airline + beverage + telecom,
#            data = as.data.frame(data8089), weights = six_digit_ratio * SalesTurnoverNet)
# mod80_2 = lm(mwtw ~ software_patents_assets + all_patents_assets + it_producing + software_capital_ratio_average + high_it_use_for_the_year + airline + beverage + telecom +
#             all_patents_assets * software_capital_ratio_average + software_patents_assets * software_capital_ratio_average +
#             all_patents_assets * high_it_use_for_the_year + software_patents_assets * high_it_use_for_the_year +
#             all_patents_assets * it_producing + software_patents_assets * it_producing + it_producing * software_capital_ratio_average,
#           data = as.data.frame(data8089), weights = six_digit_ratio * SalesTurnoverNet)
# mod90_1 =  lm(mwtw ~ software_patents_assets + all_patents_assets + it_producing + high_it_use_for_the_year + airline + beverage + telecom,
#            data = as.data.frame(data9099), weights = six_digit_ratio * SalesTurnoverNet)
# mod90_2 = lm(mwtw ~ software_patents_assets + all_patents_assets + it_producing + software_capital_ratio_average + high_it_use_for_the_year + airline + beverage + telecom +
#             all_patents_assets * software_capital_ratio_average + software_patents_assets * software_capital_ratio_average +
#             all_patents_assets * high_it_use_for_the_year + software_patents_assets * high_it_use_for_the_year +
#             all_patents_assets * it_producing + software_patents_assets * it_producing + it_producing * software_capital_ratio_average,
#           data = as.data.frame(data9099), weights = six_digit_ratio * SalesTurnoverNet)
# 
# mod1 =  lm(mwtw ~ software_patents_assets + all_patents_assets + it_producing + high_it_use_for_the_year + airline + beverage + telecom,
#            data = as.data.frame(data0009), weights = six_digit_ratio * SalesTurnoverNet)
# mod2 = lm(mwtw ~ software_patents_assets + all_patents_assets + it_producing + software_capital_ratio_average + high_it_use_for_the_year + airline + beverage + telecom +
#             all_patents_assets * software_capital_ratio_average + software_patents_assets * software_capital_ratio_average +
#             all_patents_assets * high_it_use_for_the_year + software_patents_assets * high_it_use_for_the_year +
#             all_patents_assets * it_producing + software_patents_assets * it_producing + it_producing * software_capital_ratio_average,
#           data = as.data.frame(data0009), weights = six_digit_ratio * SalesTurnoverNet)
# mod3 =  lm(mwtw ~ software_patents_assets + all_patents_assets + it_producing + high_it_use_for_the_year + airline + beverage + telecom, data = as.data.frame(data0009[!is.na(IT_employment)]), weights = six_digit_ratio * SalesTurnoverNet)
# mod4 =  lm(mwtw ~ IT_employment + software_patents_assets + all_patents_assets + it_producing + high_it_use_for_the_year + airline + beverage + telecom, data = as.data.frame(data0009[!is.na(IT_employment)]), weights = six_digit_ratio * SalesTurnoverNet)
# mod5 =  lm(mwtw ~ software_patents_assets + all_patents_assets + it_producing + high_it_use_for_the_year + airline + beverage + telecom,
#            data = as.data.frame(data1019), weights = six_digit_ratio * SalesTurnoverNet)
# mod6 = lm(mwtw ~ software_patents_assets + all_patents_assets + it_producing + software_capital_ratio_average + high_it_use_for_the_year + airline + beverage + telecom +
#             all_patents_assets * software_capital_ratio_average + software_patents_assets * software_capital_ratio_average +
#             all_patents_assets * high_it_use_for_the_year + software_patents_assets * high_it_use_for_the_year +
#             all_patents_assets * it_producing + software_patents_assets * it_producing + it_producing * software_capital_ratio_average,
#           data = as.data.frame(data1019), weights = six_digit_ratio * SalesTurnoverNet)
# mod7 =  lm(mwtw ~ software_patents_assets + all_patents_assets + it_producing + high_it_use_for_the_year + airline + beverage + telecom, data = as.data.frame(data1019[!is.na(IT_employment)]), weights = six_digit_ratio * SalesTurnoverNet)
# mod8 =  lm(mwtw ~ IT_employment + software_patents_assets + all_patents_assets + it_producing + high_it_use_for_the_year + airline + beverage + telecom, data = as.data.frame(data1019[!is.na(IT_employment)]), weights = six_digit_ratio * SalesTurnoverNet)
# 
# mods = list(mod80_1, mod80_2, mod90_1, mod90_2, mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8)
# years = c('1980-1989', '1980-1989', '1990-1999', '1990-1999',
#           '2000-2009', '2000-2009', '2000-2009', '2000-2009',
#           '2010-2019', '2010-2019', '2010-2019', '2010-2019')



data_for_regressions = data[calendaryear >= 1980 & calendaryear <= 2019 & 
                  !((true_six_digit_NAICS %/% 1000) %in% c(221, 521, 522, 523, 524, 525)) & mwtw > -1 & mwtw < 2,
                .(software_patents_assets, all_patents_assets, mwtw, Traina_markup,it_producing, it_use_for_the_year_continuous, airline, beverage, telecom, GlobalCompanyKey, high_it_use_for_the_year, true_six_digit_NAICS, six_digit_ratio, SalesTurnoverNet, IT_employment, NAICS.Title, `Anticompetitive Practices`, laborRelations)]


mod1 =  lm(mwtw ~ software_patents_assets,
           data = as.data.frame(data_for_regressions), weights = six_digit_ratio * SalesTurnoverNet)
mod2 =  lm(mwtw ~ all_patents_assets,
           data = as.data.frame(data_for_regressions), weights = six_digit_ratio * SalesTurnoverNet)
mod3 =  lm(mwtw ~ all_patents_assets + software_patents_assets,
           data = as.data.frame(data_for_regressions), weights = six_digit_ratio * SalesTurnoverNet)
mod4 =  lm(mwtw ~ all_patents_assets + software_patents_assets + it_use_for_the_year_continuous,
           data = as.data.frame(data_for_regressions), weights = six_digit_ratio * SalesTurnoverNet)
mod5 =  lm(mwtw ~ all_patents_assets + software_patents_assets + it_producing,
           data = as.data.frame(data_for_regressions), weights = six_digit_ratio * SalesTurnoverNet)
mod6 =  lm(mwtw ~ all_patents_assets + software_patents_assets + it_use_for_the_year_continuous + it_producing,
           data = as.data.frame(data_for_regressions), weights = six_digit_ratio * SalesTurnoverNet)
mod7 =  lm(mwtw ~ all_patents_assets + software_patents_assets + it_use_for_the_year_continuous + it_producing + it_producing * it_use_for_the_year_continuous,
           data = as.data.frame(data_for_regressions), weights = six_digit_ratio * SalesTurnoverNet)
mod8 =  lm(mwtw ~ all_patents_assets + software_patents_assets + high_it_use_for_the_year + it_producing,
           data = as.data.frame(data_for_regressions), weights = six_digit_ratio * SalesTurnoverNet)
mod9 =  lm(mwtw ~ software_patents_assets + all_patents_assets + it_producing + high_it_use_for_the_year + airline + beverage + telecom,
           data = as.data.frame(data_for_regressions), weights = six_digit_ratio * SalesTurnoverNet)
mod10 =  lm(mwtw ~ software_patents_assets + all_patents_assets + it_use_for_the_year_continuous + it_producing + it_producing * it_use_for_the_year_continuous + airline + beverage + telecom,
           data = as.data.frame(data_for_regressions), weights = six_digit_ratio * SalesTurnoverNet)


mod11 = lm(mwtw ~ software_patents_assets + all_patents_assets + it_producing + it_use_for_the_year_continuous + airline + beverage + telecom +
            all_patents_assets * it_use_for_the_year_continuous + software_patents_assets * it_use_for_the_year_continuous +
            all_patents_assets * it_producing + software_patents_assets * it_producing + it_producing * it_use_for_the_year_continuous,
          data = as.data.frame(data_for_regressions), weights = six_digit_ratio * SalesTurnoverNet)

mod12 = lm(mwtw ~ software_patents_assets + all_patents_assets + it_producing + high_it_use_for_the_year + airline + beverage + telecom +
            all_patents_assets * high_it_use_for_the_year + software_patents_assets * high_it_use_for_the_year +
            all_patents_assets * it_producing + software_patents_assets * it_producing,
          data = as.data.frame(data_for_regressions), weights = six_digit_ratio * SalesTurnoverNet)

mod13 =  lm(mwtw ~ IT_employment, data = as.data.frame(data_for_regressions[!is.na(IT_employment)]), weights = six_digit_ratio * SalesTurnoverNet)
mod14 =  lm(mwtw ~ software_patents_assets + all_patents_assets + it_producing + high_it_use_for_the_year + airline + beverage + telecom, data = as.data.frame(data_for_regressions[!is.na(IT_employment)]), weights = six_digit_ratio * SalesTurnoverNet)
mod15 =  lm(mwtw ~ IT_employment + software_patents_assets + all_patents_assets + it_producing + high_it_use_for_the_year + airline + beverage + telecom, data = as.data.frame(data_for_regressions[!is.na(IT_employment)]), weights = six_digit_ratio * SalesTurnoverNet)


mod16 =  lm(mwtw ~ `Anticompetitive Practices`, data = as.data.frame(data_for_regressions[!is.na(`Anticompetitive Practices`) & !is.na(laborRelations)]), weights = six_digit_ratio * SalesTurnoverNet)
mod17 =  lm(mwtw ~ laborRelations, data = as.data.frame(data_for_regressions[!is.na(`Anticompetitive Practices`) & !is.na(laborRelations)]), weights = six_digit_ratio * SalesTurnoverNet)
mod13 =  lm(mwtw ~ laborStrength + laborConcern, data = as.data.frame(data_for_regressions[!is.na(`Anticompetitive Practices`) & !is.na(laborRelations)]), weights = six_digit_ratio * SalesTurnoverNet)
mod14 =  lm(mwtw ~ `Anticompetitive Practices` + laborStrength + laborConcern + software_patents_assets + all_patents_assets + it_producing + high_it_use_for_the_year + airline + beverage + telecom, data = as.data.frame(data_for_regressions[!is.na(`Anticompetitive Practices`) & !is.na(laborRelations)]), weights = six_digit_ratio * SalesTurnoverNet)
mod18 =  lm(mwtw ~ `Anticompetitive Practices` + laborRelations, data = as.data.frame(data_for_regressions[!is.na(`Anticompetitive Practices`) & !is.na(laborRelations)]), weights = six_digit_ratio * SalesTurnoverNet)
mod19 =  lm(mwtw ~ software_patents_assets + all_patents_assets + it_producing + high_it_use_for_the_year + airline + beverage + telecom, data = as.data.frame(data_for_regressions[!is.na(`Anticompetitive Practices`) & !is.na(laborRelations)]), weights = six_digit_ratio * SalesTurnoverNet)
mod20 =  lm(mwtw ~ `Anticompetitive Practices` + laborRelations + software_patents_assets + all_patents_assets + it_producing + high_it_use_for_the_year + airline + beverage + telecom, data = as.data.frame(data_for_regressions[!is.na(`Anticompetitive Practices`) & !is.na(laborRelations)]), weights = six_digit_ratio * SalesTurnoverNet)


mods = list(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10, mod11, mod12, mod13, mod14, mod15, mod16, mod17, mod18, mod19, mod20)






options(scipen = 9)
regression_outputs = foreach(i = 1:length(mods), .combine = rbind_and_fill)%do%{
  summed = summary(mods[[i]], cluster = c('NAICS.Title'))
  summary_dt = data.table(coef(summed))
  firm_cluster = data.table(coef(summary(mods[[i]], cluster = c('GlobalCompanyKey'))))
  industry_cluster = data.table(coef(summary(mods[[i]], cluster = c('true_six_digit_NAICS'))))
  varnames = row.names(summed$coefficients)
  summary_dt[,Estimate := format(round(Estimate,4),nsmall = 4)]
  summary_dt[,`Std. Error` := paste0('(',format(round(`Std. Error`,4),nsmall=4),')')]
  if(varnames[2] %in% c("software_patents_assets", "all_patents_assets")) summary_dt[2, `:=`(`Std. Error` = firm_cluster[2, paste0('(',format(round(`Std. Error`,4),nsmall=4),')')], `Pr(>|t|)` = firm_cluster[2, `Pr(>|t|)`])]
  if(varnames[2] %in% c("Anticompetitive Practices", "laborRelations")) summary_dt[2, `:=`(`Std. Error` = firm_cluster[2, paste0('(',format(round(`Std. Error`,4),nsmall=4),')')], `Pr(>|t|)` = firm_cluster[2, `Pr(>|t|)`])]
    if(varnames[2] %in% c("laborStrength", "laborConcern")) summary_dt[2, `:=`(`Std. Error` = firm_cluster[2, paste0('(',format(round(`Std. Error`,4),nsmall=4),')')], `Pr(>|t|)` = firm_cluster[2, `Pr(>|t|)`])]
  if(varnames[2] %in% c("IT_employment")) summary_dt[2, `:=`(`Std. Error` = industry_cluster[2, paste0('(',format(round(`Std. Error`,4),nsmall=4),')')], `Pr(>|t|)` = industry_cluster[2, `Pr(>|t|)`])]
  if(varnames[3] %in% c("software_patents_assets", "all_patents_assets")) summary_dt[3, `:=`(`Std. Error` = firm_cluster[3, paste0('(',format(round(`Std. Error`,4),nsmall=4),')')], `Pr(>|t|)` = firm_cluster[3, `Pr(>|t|)`])]
  if(varnames[3] %in% c("Anticompetitive Practices", "laborRelations")) summary_dt[3, `:=`(`Std. Error` = firm_cluster[2, paste0('(',format(round(`Std. Error`,4),nsmall=4),')')], `Pr(>|t|)` = firm_cluster[2, `Pr(>|t|)`])]
    if(varnames[3] %in% c("laborStrength", "laborConcern")) summary_dt[3, `:=`(`Std. Error` = firm_cluster[2, paste0('(',format(round(`Std. Error`,4),nsmall=4),')')], `Pr(>|t|)` = firm_cluster[2, `Pr(>|t|)`])]
    if(varnames[4] %in% c("laborStrength", "laborConcern")) summary_dt[4, `:=`(`Std. Error` = firm_cluster[2, paste0('(',format(round(`Std. Error`,4),nsmall=4),')')], `Pr(>|t|)` = firm_cluster[2, `Pr(>|t|)`])]
  if(varnames[4] %in% c("software_patents_assets", "all_patents_assets")) summary_dt[4, `:=`(`Std. Error` = firm_cluster[4, paste0('(',format(round(`Std. Error`,4),nsmall=4),')')], `Pr(>|t|)` = firm_cluster[3, `Pr(>|t|)`])]
  if(varnames[5] %in% c("software_patents_assets", "all_patents_assets")) summary_dt[5, `:=`(`Std. Error` = firm_cluster[4, paste0('(',format(round(`Std. Error`,4),nsmall=4),')')], `Pr(>|t|)` = firm_cluster[3, `Pr(>|t|)`])]
  if(varnames[6] %in% c("software_patents_assets", "all_patents_assets")) summary_dt[6, `:=`(`Std. Error` = firm_cluster[4, paste0('(',format(round(`Std. Error`,4),nsmall=4),')')], `Pr(>|t|)` = firm_cluster[3, `Pr(>|t|)`])]
  summary_dt[, Estimate := paste0(Estimate, gsub('\\.','+',stars.pval(`Pr(>|t|)`)))]
  summary_dt[,`t value` := NULL]
  summary_dt[,`Pr(>|t|)` := NULL]
  # transposed = data.table(dv = c(as.character(mods[[i]]$call[[2]][[2]]), ''), t(summary_dt))
  transposed = data.table(dv = c('Monopoly Wealth/TW', ''), t(summary_dt))
  setnames(transposed,c('Dependent Variable',varnames))
  # transposed[1, type := if(i>10) 'normalized' else 'percentages']
  if(sum(grepl('IT_employment', varnames)) > 0 | i == 14) transposed[1, Subset := 'Has IT Employment'] else transposed[1, Subset := '']
  # transposed[1, `Years Included` := years[i]]
  transposed[1, `R Squared` := summed$adj.r.squared]
  setcolorder(transposed, c('Dependent Variable', 'Subset', # 'type', 'Years Included', 
                            'R Squared', names(transposed)[!names(transposed) %in% c('Dependent Variable', 'Subset', # 'type', 'Years Included',
                                                                                     'R Squared')]))
}
# write.xlsx(regression_outputs,
#            'patent_regression_ouptuts.xlsx')






data[, my_three_digit_NAICS := six_digit_NAICS %/% 1000]
# aggs = data[!my_three_digit_NAICS %in% c(221, 521, 522, 523, 524, 525) & !is.na(my_three_digit_NAICS) & !is.na(monopolywealth) &!is.na(totalwealth),
#      .(three_digit_MW = sum(monopolywealth * six_digit_ratio),
#        three_digit_TW = sum(totalwealth * six_digit_ratio)),
#      .(my_three_digit_NAICS, calendaryear)][three_digit_TW > 0]
# aggs[, three_digit_size := three_digit_TW/sum(three_digit_TW), calendaryear]
# aggs[, three_digit_mwtw_ratio := three_digit_MW/three_digit_TW]
# 
# over_time_decomposition = foreach(yr = 1986:2019, .combine = rbind_and_fill)%do%{
# size85_share85 = aggs[calendaryear == 1985, sum(three_digit_size * three_digit_mwtw_ratio)]
# size85_share86 = aggs[calendaryear == 1985, .(my_three_digit_NAICS, three_digit_size, three_digit_mwtw_ratio)
#                     ][aggs[calendaryear == yr, .(my_three_digit_NAICS, three_digit_mwtw_ratio)],
#                       on = 'my_three_digit_NAICS',
#                       three_digit_mwtw_ratio := i.three_digit_mwtw_ratio
#                     ][, sum(three_digit_size * three_digit_mwtw_ratio)]
# size86_share85 = aggs[calendaryear == 1985, .(my_three_digit_NAICS, three_digit_size, three_digit_mwtw_ratio)
#                     ][aggs[calendaryear == yr, .(my_three_digit_NAICS, three_digit_size)],
#                       on = 'my_three_digit_NAICS',
#                       three_digit_size := i.three_digit_size
#                     ][, sum(three_digit_size * three_digit_mwtw_ratio)]
# size86_share86 = aggs[calendaryear == yr, sum(three_digit_size * three_digit_mwtw_ratio)]
# data.table(year = yr, initial = size85_share85, change_share = size85_share86, change_size = size86_share85,
#            linear =  size85_share86 + size86_share85 - size85_share85, final = size86_share86)
# }
# 
# over_time_decomposition_long = rbind(over_time_decomposition[,.(year, V1 = change_share, V2 = 'change_share')],over_time_decomposition[,.(year, V1 = change_size, V2 = 'change_size')],over_time_decomposition[,.(year, V1 = linear, V2 = 'linear')],over_time_decomposition[,.(year, V1 = final, V2 = 'final')])
# ggplot(over_time_decomposition, aes(x = year, y = V1, group = V2)) + geom_line()
# 
# 
# 




firms = data[!my_three_digit_NAICS %in% c(221, 521, 522, 523, 524, 525) & !is.na(my_three_digit_NAICS) & !is.na(monopolywealth) &!is.na(totalwealth),
            .(firm_MW = sum(monopolywealth * six_digit_ratio),
              firm_TW = sum(totalwealth * six_digit_ratio),
              firm_sales = sum(SalesTurnoverNet * six_digit_ratio)),
            .(GlobalCompanyKey, calendaryear)][firm_TW > 0]
firms[, size := firm_sales/sum(firm_sales), calendaryear]
firms[, mwtw_ratio := firm_MW/firm_TW]

over_time_decomposition = foreach(yr = 1986:2019, .combine = rbind_and_fill)%do%{
  firms_stay_the_same = merge(firms[calendaryear == yr-1], firms[calendaryear == yr], all.x = F, all.y = F, by = 'GlobalCompanyKey', suffixes = c('_old', '_new'))
  initial = firms_stay_the_same[, sum(firm_sales_old/sum(firm_sales_old) * mwtw_ratio_old)]
  change_mw = firms_stay_the_same[, sum(firm_sales_old/sum(firm_sales_old) * mwtw_ratio_new)]
  change_market_shares = firms_stay_the_same[, sum(firm_sales_new/sum(firm_sales_new) * mwtw_ratio_old)]
  final = firms_stay_the_same[, sum(firm_sales_new/sum(firm_sales_new) * mwtw_ratio_new)]
  actual_mw_old = firms[calendaryear == yr-1, sum(size * mwtw_ratio)]
  actual_mw_new = firms[calendaryear == yr, sum(size * mwtw_ratio)]
  data.table(year = yr, initial = initial, change_mw = change_mw, change_market_shares = change_market_shares,
             linear =  change_mw + change_market_shares - initial, final = final,
             entry_exit = (actual_mw_new - actual_mw_old) - (final - initial)
             )[, `:=`(reallocation = change_market_shares - initial,
                      within_firm_rise = change_mw - initial,
                      cross_term = final - linear)][]
}

over_time_decomposition_long = rbind(over_time_decomposition[,.(year, V1 = reallocation, V2 = 'reallocation')],
                                     over_time_decomposition[,.(year, V1 = within_firm_rise, V2 = 'within_firm_rise')],
                                     over_time_decomposition[,.(year, V1 = cross_term, V2 = 'cross_term')],
                                     over_time_decomposition[,.(year, V1 = entry_exit, V2 = 'entry_exit')])
over_time_decomposition_long[, cumulative := cumsum(V1), V2]
ggplot(over_time_decomposition_long, aes(x = year, y = cumulative, color = V2)) + geom_line()





firms_and_industries = data[!my_three_digit_NAICS %in% c(221, 521, 522, 523, 524, 525) & !is.na(my_three_digit_NAICS) & !is.na(monopolywealth) &!is.na(totalwealth),
             .(firm_MW = sum(monopolywealth * six_digit_ratio),
               firm_TW = sum(totalwealth * six_digit_ratio),
               firm_sales = sum(SalesTurnoverNet * six_digit_ratio),
               DLE_markup = first(DLE_markup)),
             .(GlobalCompanyKey, calendaryear, my_three_digit_NAICS)][firm_TW > 0]
firms_and_industries[, size := firm_sales/sum(firm_sales), calendaryear]
firms_and_industries[, mwtw_ratio := firm_MW/firm_TW]

over_time_decomposition = foreach(yr = 1986:2019, .combine = rbind_and_fill)%do%{
  firms_stay_the_same = merge(firms_and_industries[calendaryear == yr-1], firms_and_industries[calendaryear == yr], all.x = F, all.y = F, by = c('GlobalCompanyKey', 'my_three_digit_NAICS'), suffixes = c('_old', '_new'))[, issue := sum(firm_sales_old) == 0 | sum(firm_sales_new) == 0, my_three_digit_NAICS][issue == F]
  firms_stay_the_same[, firm_share_of_industry_old := firm_sales_old/sum(firm_sales_old), my_three_digit_NAICS]
  firms_stay_the_same[, firm_share_of_industry_new := firm_sales_new/sum(firm_sales_new), my_three_digit_NAICS]
  firms_stay_the_same[, industry_share_of_economy_old := sum(firm_sales_old)/sum(firms_stay_the_same$firm_sales_old), my_three_digit_NAICS]
  firms_stay_the_same[, industry_share_of_economy_new := sum(firm_sales_new)/sum(firms_stay_the_same$firm_sales_new), my_three_digit_NAICS]
  initial = firms_stay_the_same[, sum(firm_share_of_industry_old * industry_share_of_economy_old * mwtw_ratio_old)]
  change_firm_shares_of_industries = firms_stay_the_same[, sum(firm_share_of_industry_new * industry_share_of_economy_old * mwtw_ratio_old)]
  change_industry_shares_of_economy = firms_stay_the_same[, sum(firm_share_of_industry_old * industry_share_of_economy_new * mwtw_ratio_old)]
  change_both_shares_full_reallocation = firms_stay_the_same[, sum(firm_share_of_industry_new * industry_share_of_economy_new * mwtw_ratio_old)]
  change_mw = firms_stay_the_same[, sum(firm_share_of_industry_old * industry_share_of_economy_old * mwtw_ratio_new)]
  final = firms_stay_the_same[, sum(firm_share_of_industry_new * industry_share_of_economy_new * mwtw_ratio_new)]
  actual_mw_old = firms_and_industries[calendaryear == yr-1, sum(size * mwtw_ratio)]
  actual_mw_new = firms_and_industries[calendaryear == yr, sum(size * mwtw_ratio)]
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

over_time_decomposition_long = rbind(over_time_decomposition[,.(year, V1 = reallocation, V2 = 'reallocation')],
                                     over_time_decomposition[,.(year, V1 = within_firm_rise, V2 = 'within_firm_rise')],
                                     over_time_decomposition[,.(year, V1 = cross_term, V2 = 'cross_term')],
                                     over_time_decomposition[,.(year, V1 = entry_exit, V2 = 'entry_exit')])
over_time_decomposition_long[, cumulative := cumsum(V1), V2]
ggplot(over_time_decomposition_long, aes(x = year, y = cumulative, color = V2)) + geom_line()
over_time_decomposition_cumulative = over_time_decomposition[, lapply(.SD,cumsum),
                                                             .SDcols = c('reallocation', 'firm_reallocation',
                                                                         'industry_reallocation', 'cross_term_firm_industry_reallocation',
                                                                         'within_firm_rise', 'cross_term', 'entry_exit'
                                                                         )][, year := over_time_decomposition$year]
write.xlsx(over_time_decomposition_cumulative, 'MWTW_decomposition.xlsx')

over_time_decomposition_long2 = rbind(over_time_decomposition[,.(year, V1 = firm_reallocation, V2 = 'firm_reallocation')],
                                      over_time_decomposition[,.(year, V1 = industry_reallocation, V2 = 'industry_reallocation')],
                                      over_time_decomposition[,.(year, V1 = cross_term_firm_industry_reallocation, V2 = 'cross_term_firm_industry_reallocation')],
                                     over_time_decomposition[,.(year, V1 = within_firm_rise, V2 = 'within_firm_rise')],
                                     over_time_decomposition[,.(year, V1 = cross_term, V2 = 'cross_term')],
                                     over_time_decomposition[,.(year, V1 = entry_exit, V2 = 'entry_exit')])
over_time_decomposition_long2[, cumulative := cumsum(V1), V2]
ggplot(over_time_decomposition_long2, aes(x = year, y = cumulative, color = V2)) + geom_line(size = 1, aes(linetype = V2))




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

over_time_decomposition = foreach(yr = 1986:2019, .combine = rbind_and_fill)%do%{
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

over_time_decomposition_long = rbind(over_time_decomposition[,.(year, V1 = reallocation, V2 = 'reallocation')],
                                     over_time_decomposition[,.(year, V1 = within_firm_rise, V2 = 'within_firm_rise')],
                                     over_time_decomposition[,.(year, V1 = cross_term, V2 = 'cross_term')],
                                     over_time_decomposition[,.(year, V1 = entry_exit, V2 = 'entry_exit')])
over_time_decomposition_long[, cumulative := cumsum(V1), V2]
ggplot(over_time_decomposition_long, aes(x = year, y = cumulative, color = V2)) + geom_line()


over_time_decomposition_long2 = rbind(over_time_decomposition[,.(year, V1 = firm_reallocation, V2 = 'firm_reallocation')],
                                      over_time_decomposition[,.(year, V1 = industry_reallocation, V2 = 'industry_reallocation')],
                                      over_time_decomposition[,.(year, V1 = cross_term_firm_industry_reallocation, V2 = 'cross_term_firm_industry_reallocation')],
                                      over_time_decomposition[,.(year, V1 = within_firm_rise, V2 = 'within_firm_rise')],
                                      over_time_decomposition[,.(year, V1 = cross_term, V2 = 'cross_term')],
                                      over_time_decomposition[,.(year, V1 = entry_exit, V2 = 'entry_exit')])
over_time_decomposition_long2[, cumulative := cumsum(V1), V2]
ggplot(over_time_decomposition_long2, aes(x = year, y = cumulative, color = V2)) + geom_line(size = 1, aes(linetype = V2))



# clusters=makeCluster(7)
# registerDoSNOW(clusters)

# regs = foreach(spec = iter(specifications, by = 'row'))%do%{
#   if(spec$dv == 'DLE_markup' | spec$dv == 'Traina_markup') dvbounds = '0, 5'
#   if(spec$dv == 'DLE_profit') dvbounds = '-1, 2'
#   if(spec$dv == 'mwtw') dvbounds = '-1, 2'
#   if(spec$dv == 'mwv') dvbounds = '-6, 6'
#   reg = eval(parse(text = paste0(
#     'lm(',spec$dv,'~',spec$iv,',
#     data = data[is.finite(',spec$dv,')&is.finite(',spec$iv,') & data.table::between(', spec$dv, ', ',dvbounds,')],
#     weights = six_digit_ratio)')))
#   normalized = eval(parse(text = paste0(
#     'data[is.finite(',spec$dv,')&is.finite(',spec$iv,') & data.table::between(', spec$dv, ', ',dvbounds,'),
#          .(', spec$dv, ' = z_score(', spec$dv, ', weights = six_digit_ratio),
#            ', spec$iv, ' = z_score(', spec$iv, ', weights = six_digit_ratio),
#            six_digit_ratio, sd(Traina_markup), wtd.var(Traina_markup))]')))
#   # normalized_reg = lm(eval(parse(text = (paste(spec$dv,spec$iv,sep = '~')))),
#   #                     data = normalized,
#   #                     weights = six_digit_ratio)
#   normalized_reg = eval(parse(text = paste0(
#     'lm(',spec$dv,'~',spec$iv,',
#     data = normalized,
#     weights = six_digit_ratio)')))
#   list(reg,normalized_reg)
# }

# # stopCluster(clusters)


# regression_ouptuts = foreach(i = 1:length(regs), .combine = rbind)%do%{
#   reg = regs[[i]][[1]]
#   data.table(dv = as.character(reg$call[[2]][[2]]),
#              iv = as.character(reg$call[[2]][[3]]),
#              intercept = reg$coefficients['(Intercept)'],
#              coef = reg$coefficients[2],
#              se = coef(summary(reg))[2,2],
#              p_val = coef(summary(reg))[2,4])
# }
# normalized_regression_ouptuts = foreach(i = 1:length(regs), .combine = rbind)%do%{
#   reg = regs[[i]][[2]]
#   data.table(dv = as.character(reg$call[[2]][[2]]),
#              iv = as.character(reg$call[[2]][[3]]),
#              intercept = reg$coefficients['(Intercept)'],
#              coef = reg$coefficients[2],
#              se = coef(summary(reg))[2,2],
#              p_val = coef(summary(reg))[2,4])
# }
# write.xlsx(list(regression_ouptuts, normalized_regression_ouptuts),
#            'regression_ouptuts.xlsx')

