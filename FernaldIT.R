library(data.table)
library(openxlsx)
library(readxl)
library(foreach)
library(iterators)
library(stringr)
library(readxl)

getCharCols = function(x) {
  jkl = readLines(x,n = 2)[2]
  cols = strsplit(jkl,',')[[1]]
  grep('"',cols)
}

rbind_and_fill = function(...) rbind(...,fill=T)

fread_and_getCharCols = function(x) {
  fread(x, colClasses = list(character = getCharCols(x)))
}

setwd("C:/Users/Nathan/Downloads/Compustat")
withSixDigit = fread_and_getCharCols('withMarkups.csv')

# withSixDigit[,monopolywealth:=monopolywealth*six_digit_ratio]
# withSixDigit[,totalwealth:=totalwealth*six_digit_ratio]
# withSixDigit[,marketvalue:=MktVal*six_digit_ratio]
withSixDigit[,marketvalue := MktVal]

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
withSixDigit[,firmtype:=factor(ifelse(it_producing == T, 'it_producing',
                                       ifelse(it_intensive == T,'it_intensive',
                                              'non_it')),
                                levels = c('it_producing','it_intensive','non_it'))]
aggregates_by_it_use = withSixDigit[non_financial==T&!is.na(monopolywealth),
                    .(monopolywealth = sum(monopolywealth),totalwealth = sum(totalwealth),marketvalue = sum(marketvalue)),
                    .(calendaryear,firmtype)]
aggregates_wide = dcast(aggregates_by_it_use,calendaryear~firmtype,value.var = c('monopolywealth','totalwealth','marketvalue'))

fwrite(aggregates_wide,'MonopolyWealthByIT.csv')

NAICS_codes_to_names = data.table(read_excel('2017_NAICS_Codes.xlsx'))
suppressWarnings(NAICS_codes_to_names[,Code := as.numeric(str_pad(Code, width=6, side='right', pad='0'))])
withSixDigit[,three_digit_HHI := sum((SalesTurnoverNet/sum(SalesTurnoverNet))^2),.(three_digit_NAICS,calendaryear)]
#create imputation datasets with different NAICS codes and average the resulting HHIs. Or try to solve for the average analytically.
#But don't treat all the imputed firms as small firms in a variety of industries
industry_aggregates = withSixDigit[!is.na(monopolywealth)&calendaryear==2019,
                                           .(.N,monopolywealth = sum(monopolywealth*six_digit_ratio),totalwealth = sum(totalwealth*six_digit_ratio),marketvalue = sum(marketvalue*six_digit_ratio)),
                                           .(true_three_digit_NAICS)]
monopolywealth_by_industry = industry_aggregates[,mwtw:=monopolywealth/totalwealth
                                               ][,mwv:=monopolywealth/marketvalue
                                               ][NAICS_codes_to_names,on=c(true_three_digit_NAICS='Code')]
setorder(monopolywealth_by_industry,-mwv)
monopolywealth_by_industry = monopolywealth_by_industry[N>=2]
setnames(monopolywealth_by_industry, 'true_three_digit_NAICS', 'NAICS Code')
monopolywealth_by_industry[substr(`NAICS Code`,1,2) == '52', `:=`(totalwealth=NA, mwtw = NA)]
setcolorder(monopolywealth_by_industry, c('Name'))
monopolywealth_by_industry[,`NAICS Code`:=`NAICS Code`/1000]

industry_aggregates_1985 = withSixDigit[!is.na(monopolywealth)&calendaryear==1985,
                                        .(.N,monopolywealth = sum(monopolywealth*six_digit_ratio),totalwealth = sum(totalwealth*six_digit_ratio),marketvalue = sum(marketvalue*six_digit_ratio)),
                                        .(true_three_digit_NAICS)]
monopolywealth_by_industry_1985 = industry_aggregates_1985[,mwtw:=monopolywealth/totalwealth
                                                    ][,mwv:=monopolywealth/marketvalue
                                                    ][NAICS_codes_to_names,on=c(true_three_digit_NAICS='Code')]
monopolywealth_by_industry_1985 = monopolywealth_by_industry_1985[N>=1]
monopolywealth_by_industry_1985[substr(true_three_digit_NAICS,1,2) == '52', `:=`(totalwealth=NA, mwtw = NA)]

monopolywealth_by_industry[monopolywealth_by_industry_1985,on = 'Name', `:=`(mwv_1985 = i.mwv, mwtw_1985 = i.mwtw)]

write.xlsx(monopolywealth_by_industry,
           'monopoly_wealth_by_industry_2019.xlsx')


aggregate_capital = data.table(
  read.xlsx('cap_details.xlsx',sheet = 'DATA')
  )[NAICS == 'FB', NAICS := '111,112'
  ][grep('487', NAICS), NAICS := '487488491492'
  ][grep('[0-9]',NAICS)
  ][Measure=='Productive capital stock (direct aggregate-billions of 2012 dollars)'&
    Asset.Category=='All assets'&
    Duration.Title=='Levels'
  ][Asset.Category=='All assets',Asset.Category:='All_Assets']
intellectual_property_capital = data.table(
  read.xlsx('cap_details_ipp.xlsx',sheet = 'Data')
  )[NAICS == 'FB', NAICS := '111,112'
  ][grep('487', NAICS), NAICS := '487488491492'
  ][grep('[0-9]',NAICS)
  ][Measure=='Productive capital stock (direct aggregate-billions of 2012 dollars)'&
    Asset.Category!='Total'&
    Duration.Title=='Levels']
information_capital = data.table(
  read.xlsx('cap_details_ipe.xlsx',sheet = 'Data')
  )[NAICS == 'FB', NAICS := '111,112'
  ][grep('487', NAICS), NAICS := '487488491492'
  ][grep('[0-9]', NAICS)
  ][Measure=='Productive capital stock (direct aggregate-billions of 2012 dollars)'&
    Asset.Category!='Total'&
    Duration.Title=='Levels']

yearcols_to_numeric = function(x) suppressWarnings(
                                                    x[,
                                                     (names(x)[names(x)%in%as.character(1900:2030)]) := lapply(.SD,as.numeric),
                                                     .SDcols = names(x)[names(x)%in%as.character(1900:2030)]
                                                     ]
                                                   )
yearcols_to_numeric(aggregate_capital)
yearcols_to_numeric(information_capital)
yearcols_to_numeric(intellectual_property_capital)

intellectual_property_capital = dcast(intellectual_property_capital,NAICS+NAICS.Title+Measure+Duration.Title~Asset.Category,value.var = grep('^[0-9]{4}$',names(intellectual_property_capital), value = T))
information_capital = dcast(information_capital,NAICS+NAICS.Title+Measure+Duration.Title~Asset.Category,value.var = grep('^[0-9]{4}$',names(information_capital), value = T))
aggregate_capital = dcast(aggregate_capital,NAICS+NAICS.Title+Measure+Duration.Title~Asset.Category,value.var = grep('^[0-9]{4}$',names(aggregate_capital), value = T))
setnames(aggregate_capital,sub('(?<=[0-9]{4})','_capital',names(aggregate_capital),perl=T))
capital_data = aggregate_capital[information_capital,on='NAICS'][intellectual_property_capital,on='NAICS']

aggregate_capital[grep('^[0-9]{5,}$',NAICS),NAICS := prettyNum(NAICS, big.mark = ',')] #insert commas into the NAICS codes of 5-digits or more (which are actually three digit codes squished together)


aggregate_capital = aggregate_capital[aggregate_capital[,.(split=trimws(unlist(strsplit(NAICS,',')))),by=NAICS],on='NAICS']
aggregate_capital[,c('NAICSmin','NAICSmax'):=tstrsplit(split,'-')
                ][is.na(NAICSmax),NAICSmax:=NAICSmin]
aggregate_capital[,NAICSmin:=as.numeric(str_pad(NAICSmin, width=6, side='right', pad='0'))]
aggregate_capital[,NAICSmax:=as.numeric(str_pad(NAICSmax, width=6, side='right', pad='9'))]
BEA_capital_assets = aggregate_capital[intellectual_property_capital,on=c('NAICS.Title','Measure','Duration.Title')
                     ][,i.NAICS:=NULL
                     ][information_capital,on=c('NAICS.Title','Measure','Duration.Title')
                     ][,i.NAICS:=NULL
                     ]
setnames(BEA_capital_assets,'NAICS','BEA_NAICS_str')
foreach(year = 1987:2018)%do%{
  eval(parse(text = paste0('BEA_capital_assets[,it_capital_',year,' := sum(`',year,'_Software`, `',year,'_Computers`, `',year,'_Communication`, `',year,'_Other`, na.rm = T), by = 1:nrow(BEA_capital_assets)]')))
  eval(parse(text = paste0('BEA_capital_assets[,it_capital_ratio_',year,' := it_capital_',year,' / `',year,'_capital_All_Assets`]')))
  eval(parse(text = paste0('BEA_capital_assets[,computer_capital_ratio_',year,' := `',year,'_Computers` / `',year,'_capital_All_Assets`]')))
  eval(parse(text = paste0('BEA_capital_assets[,communication_capital_ratio_',year,' := `',year,'_Communication` / `',year,'_capital_All_Assets`]')))
  eval(parse(text = paste0('BEA_capital_assets[,other_capital_ratio_',year,' := `',year,'_Other` / `',year,'_capital_All_Assets`]')))
  eval(parse(text = paste0('BEA_capital_assets[,software_capital_ratio_',year,' := `',year,'_Software` / `',year,'_capital_All_Assets`]')))
  eval(parse(text = paste0('BEA_capital_assets[,artistic_capital_ratio_',year,' := `',year,'_Artistic originals` / `',year,'_capital_All_Assets`]')))
  eval(parse(text = paste0('BEA_capital_assets[,rd_capital_ratio_',year,' := `',year,'_Research and development` / `',year,'_capital_All_Assets`]')))
  eval(parse(text = paste0('BEA_capital_assets[,ip_capital_',year,' := sum(`',year,'_Software`, `',year,'_Artistic originals`, `',year,'_Research and development`, na.rm = T), by = 1:nrow(BEA_capital_assets)]')))
  eval(parse(text = paste0('BEA_capital_assets[,ip_capital_ratio_',year,' := ip_capital_',year,' / `',year,'_capital_All_Assets`]')))
  NULL
}
BEA_capital_assets[,average_it_capital_ratio := rowMeans(.SD,na.rm = T),.SDcols = grep('it_capital_ratio',names(BEA_capital_assets),value = T)]
BEA_capital_assets[,average_ip_capital_ratio := rowMeans(.SD,na.rm = T),.SDcols = grep('ip_capital_ratio',names(BEA_capital_assets),value = T)]
BEA_capital_assets[,average_computer_capital_ratio := rowMeans(.SD,na.rm = T),.SDcols = grep('computer_capital_ratio',names(BEA_capital_assets),value = T)]
withSixDigit[,true_six_digit_NAICS2:=true_six_digit_NAICS]
setkey(withSixDigit,true_six_digit_NAICS,true_six_digit_NAICS2)
setkey(BEA_capital_assets,NAICSmin,NAICSmax)
howsthis2 = foverlaps(withSixDigit[true_six_digit_NAICS < 999900], BEA_capital_assets)
howsthis2[,c('NAICSmin','NAICSmax') := NULL]
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

input_output_IT = fread_and_getCharCols('input_output_IT.csv')
setkey(howsthis2,true_six_digit_NAICS,true_six_digit_NAICS2)
setkey(input_output_IT,NAICSmin,NAICSmax)
howsthis3 = foverlaps(howsthis2, input_output_IT)
howsthis3[,c('NAICSmin','NAICSmax') := NULL]

IT_employment = fread_and_getCharCols('IT_employment.csv')
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

firm_software_patenting = fread_and_getCharCols('firm_software_patenting.csv')
howsthis4[firm_software_patenting,
          on = c(calendaryear = 'appyear', GlobalCompanyKey = 'gvkey'),
          `:=`(software_patents = i.patents, software_patents_rolling_5 = i.patents_rolling_5)]

fwrite(howsthis4,'foranalysis.csv')