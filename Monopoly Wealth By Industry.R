library(data.table)
library(openxlsx)
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
withSixDigit = fread_and_getCharCols('IntermediateFiles/withMarkups.csv')

withSixDigit[, marketvalue := MktVal]


NAICS_codes_to_names = data.table(read_excel('Data/2017_NAICS_Codes.xlsx'))
suppressWarnings(NAICS_codes_to_names[, Code := as.numeric(str_pad(Code, width=6, side='right', pad='0'))])

industry_aggregates = withSixDigit[!is.na(monopolywealth)&calendaryear==2019,
                                   .(.N,monopolywealth = sum(monopolywealth*six_digit_ratio),totalwealth = sum(totalwealth*six_digit_ratio),marketvalue = sum(marketvalue*six_digit_ratio)),
                                   .(true_three_digit_NAICS)]
monopolywealth_by_industry = industry_aggregates[,mwtw:=monopolywealth/totalwealth
                                                 ][,mwv:=monopolywealth/marketvalue
                                                   ][NAICS_codes_to_names,on=c(true_three_digit_NAICS='Code')]
setorder(monopolywealth_by_industry,-mwv)
monopolywealth_by_industry = monopolywealth_by_industry[N >= 2]
setnames(monopolywealth_by_industry, 'true_three_digit_NAICS', 'NAICS Code')
monopolywealth_by_industry[substr(`NAICS Code`,1,2) == '52', `:=`(totalwealth = NA, mwtw = NA)]
setcolorder(monopolywealth_by_industry, c('Name'))
monopolywealth_by_industry[,`NAICS Code`:=`NAICS Code`/1000]

industry_aggregates_1985 = withSixDigit[!is.na(monopolywealth) & calendaryear == 1985,
                                        .(.N,monopolywealth = sum(monopolywealth*six_digit_ratio),totalwealth = sum(totalwealth*six_digit_ratio),marketvalue = sum(marketvalue*six_digit_ratio)),
                                        .(true_three_digit_NAICS)]
monopolywealth_by_industry_1985 = industry_aggregates_1985[,mwtw:=monopolywealth/totalwealth
                                                         ][,mwv:=monopolywealth/marketvalue
                                                         ][NAICS_codes_to_names,on=c(true_three_digit_NAICS='Code')]
monopolywealth_by_industry_1985 = monopolywealth_by_industry_1985[N>=1]
monopolywealth_by_industry_1985[substr(true_three_digit_NAICS,1,2) == '52', `:=`(totalwealth = NA, mwtw = NA)]

monopolywealth_by_industry[monopolywealth_by_industry_1985, on = 'Name', `:=`(mwv_1985 = i.mwv, mwtw_1985 = i.mwtw)]

write.xlsx(monopolywealth_by_industry,
           'SpreadsheetOutputs/monopoly_wealth_by_industry_2019.xlsx')
