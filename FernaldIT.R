getCharCols = function(x) {
  jkl = readLines(x,n = 2)[2]
  cols = strsplit(jkl,',')[[1]]
  grep('"',cols)
}

rbind_and_fill = function(...) rbind(...,fill=T)

fread_and_getCharCols = function(x) {
  fread(x, colClasses = list(character = getCharCols(x)))
}

withSixDigit = fread_and_getCharCols('IntermediateFiles/withMarkups.csv')

# withSixDigit[,monopolywealth:=monopolywealth*six_digit_ratio]
# withSixDigit[,totalwealth:=totalwealth*six_digit_ratio]
# withSixDigit[,marketvalue:=MktVal*six_digit_ratio]
withSixDigit[, marketvalue := MktVal]

it_producing_industries = c(5415,511,516,334)

it_intensive_industries = c(22,42,481,483,486,
                 512,515,517,518,519,
                 5412,5413,5414,5416,5417,5418,5419,
                 55,561,621)

non_financial_industries = c(311,312,313,314,315,316,
                  322,323,324,325,326,321,327,
                  331,332,333,334,335,336,337,339,
                  22,42,44,45,481,482,483,484,485,486,487,488,492,493,
                  511,516,512,515,517,518,519,5411,5415,
                  5412,5413,5414,5416,5417,5418,5419,
                  55,561,562,61,621,622,623,624,711,712,713,721,722,81)

itertable = data.table(var_name = c('it_producing','it_intensive','non_financial'),
                       var_industries = list(it_producing_industries,it_intensive_industries,non_financial_industries))
throwaway = foreach(row=iter(itertable,by = 'row'))%do%{
  industries = row$var_industries[[1]]
  withSixDigit[,(row$var_name):=(true_six_digit_NAICS%/%100)%in%industries
                              |(true_six_digit_NAICS%/%1000)%in%industries
                              |(true_six_digit_NAICS%/%10000)%in%industries]
  NULL
}

# withSixDigit[,it_intensive:=(true_four_digit_NAICS%/%100)%in%it_intensive_list
#                             |(true_four_digit_NAICS%/%1000)%in%it_intensive_list
#                             |(true_four_digit_NAICS%/%10000)%in%it_intensive_list]
# 
# withSixDigit[,non_financial:=(true_four_digit_NAICS%/%100)%in%non_financial_list
#               |(true_four_digit_NAICS%/%1000)%in%non_financial_list
#               |(true_four_digit_NAICS%/%10000)%in%non_financial_list]

# it_producing_data = withSixDigit[non_financial==T,.(monopolywealth = sum(monopolywealth)),.(itprod,calendaryear)]
# it_intensive_data = withSixDigit[non_financial==T&itprod==F,.(monopolywealth = sum(monopolywealth)),.(it_intensive,calendaryear)]
# setkey(it_producing_data,calendaryear)
# setkey(it_intensive_data,calendaryear)
# withSixDigit[,itprod:=it_producing]
# it_producing_data = withSixDigit[non_financial==T&itprod==T&!is.na(monopolywealth)&!is.na(totalwealth),
#                                   .(monopolywealth = sum(monopolywealth),totalwealth = sum(totalwealth)),calendaryear]
# it_intensive_data = withSixDigit[non_financial==T&itprod==F&it_intensive==T&!is.na(monopolywealth)&!is.na(totalwealth),
#                                   .(monopolywealth = sum(monopolywealth),totalwealth = sum(totalwealth)),calendaryear]
# non_it_data = withSixDigit[non_financial==T&itprod==F&it_intensive==F&!is.na(monopolywealth)&!is.na(totalwealth),
#                             .(monopolywealth = sum(monopolywealth),totalwealth = sum(totalwealth)),calendaryear]
# setkey(it_producing_data,calendaryear)
# setkey(it_intensive_data,calendaryear)
# setkey(non_it_data,calendaryear)
# it_producing_data[it_intensive_data,`:=`(it_intensive_monopolywealth=i.monopolywealth,it_intensive_totalwealth=i.totalwealth)]
# it_producing_data[non_it_data,`:=`(non_it_monopolywealth=i.monopolywealth,non_it_totalwealth=i.totalwealth)]
withSixDigit[, firmtype := factor(ifelse(it_producing == T, 'it_producing',
                                       ifelse(it_intensive == T,'it_intensive',
                                              'non_it')),
                                levels = c('it_producing','it_intensive','non_it'))]
aggregates_by_it_use = withSixDigit[non_financial == T & !is.na(monopolywealth),
                    .(monopolywealth = sum(monopolywealth), totalwealth = sum(totalwealth), marketvalue = sum(marketvalue)),
                    .(calendaryear,firmtype)]
aggregates_wide = dcast(aggregates_by_it_use,calendaryear~firmtype,value.var = c('monopolywealth','totalwealth','marketvalue'))

write.xlsx(aggregates_wide,'SpreadsheetOutputs/MonopolyWealthByIT.xlsx', startCol = 2, startRow = 2)


withSixDigit[, three_digit_HHI := sum((SalesTurnoverNet/sum(SalesTurnoverNet))^2),.(three_digit_NAICS,calendaryear)]
#create imputation datasets with different NAICS codes and average the resulting HHIs. Or try to solve for the average analytically.
#But don't treat all the imputed firms as small firms in a variety of industries


BEA_capital_assets = readRDS('IntermediateFiles/BEA Capital Assets by Industry.rds')

withSixDigit[, true_six_digit_NAICS2 := true_six_digit_NAICS]
setkey(withSixDigit, true_six_digit_NAICS, true_six_digit_NAICS2)
setkey(BEA_capital_assets, NAICSmin, NAICSmax)
howsthis2 = foverlaps(withSixDigit[true_six_digit_NAICS < 999900], BEA_capital_assets)
howsthis2[, c('NAICSmin', 'NAICSmax') := NULL]
#516110 becomes 519130
data1987 = howsthis2[calendaryear == 1987 & !is.na(monopolywealth)& !is.na(totalwealth)& !is.na(marketvalue) & !is.na(NAICS.Title),
          .(mw = sum(monopolywealth*six_digit_ratio), tw = sum(totalwealth*six_digit_ratio), mktval = sum(marketvalue*six_digit_ratio)),NAICS.Title]
data2018 = howsthis2[calendaryear == 2018 & !is.na(monopolywealth)& !is.na(totalwealth)& !is.na(marketvalue) & !is.na(NAICS.Title),
          .(mw = sum(monopolywealth*six_digit_ratio), tw = sum(totalwealth*six_digit_ratio), mktval = sum(marketvalue*six_digit_ratio)),NAICS.Title]
setkey(data1987,NAICS.Title)
setkey(data2018,NAICS.Title)
data2018[,deltaMW:=data2018$mw/data2018$tw - data1987$mw/data1987$tw]
data2018[,MWTW2018 := data2018$mw/data2018$tw]
regression_data = data2018[BEA_capital_assets, on = 'NAICS.Title'] #[,monopoly_wealth_over_total_wealth := mw/tw]
summary(lm(MWTW2018 ~ computer_capital_ratio_1987 + communication_capital_ratio_1987 + other_capital_ratio_1987 + software_capital_ratio_1987 + rd_capital_ratio_1987,data = regression_data))
summary(lm(MWTW2018 ~ it_capital_1987,data = regression_data))

input_output_IT = fread_and_getCharCols('IntermediateFiles/input_output_IT.csv')
setkey(howsthis2,true_six_digit_NAICS,true_six_digit_NAICS2)
setkey(input_output_IT,NAICSmin,NAICSmax)
howsthis3 = foverlaps(howsthis2, input_output_IT)
howsthis3[,c('NAICSmin','NAICSmax') := NULL]

IT_employment = fread_and_getCharCols('IntermediateFiles/IT_employment.csv')
IT_employment[,NAICSmin:=as.numeric(str_pad(naics, width=6, side='right', pad='0'))]
IT_employment[,NAICSmax:=as.numeric(str_pad(naics, width=6, side='right', pad='9'))]
IT_employment[, temp_unique_NAICSmin := (YEAR - 1900) * 1000000 + NAICSmin
            ][, temp_unique_NAICSmax := (YEAR - 1900) * 1000000 + NAICSmax
            ]
setkey(IT_employment, temp_unique_NAICSmin, temp_unique_NAICSmax)
howsthis3[, temp_unique_NAICS := (calendaryear - 1900) * 1000000 + true_six_digit_NAICS]
howsthis3[, temp_unique_NAICS2 := temp_unique_NAICS]
setkey(howsthis3, temp_unique_NAICS, temp_unique_NAICS2)
howsthis4 = foverlaps(howsthis3, IT_employment)

firm_software_patenting = fread_and_getCharCols('IntermediateFiles/firm_software_patenting.csv')
howsthis4[firm_software_patenting,
          on = c(calendaryear = 'appyear', GlobalCompanyKey = 'gvkey'),
          `:=`(software_patents = i.patents, software_patents_rolling_5 = i.patents_rolling_5)]

fwrite(howsthis4,'IntermediateFiles/foranalysis.csv')

rm(howsthis2, howsthis3)