#TFP to Psi
setwd('C:/Users/Nathan/Downloads/Compustat')

library(data.table)
library(foreach)
library(openxlsx)
library(ggplot2)
library(Hmisc)

rbind_and_fill = function(...) rbind(...,fill=T)

getCharCols = function(x) {
  second_line = readLines(x,n = 2)[2]
  cols = strsplit(second_line, ',')[[1]]
  grep('"',cols)
}

fread_and_getCharCols = function(x) {
  fread(x, colClasses = list(character = getCharCols(x)))
}


withThreeDigit = fread_and_getCharCols('IntermediateFiles/withThreeDigit.csv')
# withThreeDigit[, value_added := oibdp + emp*wage]


tfp_data = fread('TFPData_updated_2019.csv')[, TFP_level := exp(TFP)]
# tfp_data = fread('TFPData_updated_ImrohorogluTuzel (1).csv')[, TFP_level := exp(TFP)]
asdf = unique(withThreeDigit, by = c('GlobalCompanyKey', 'calendaryear'))[, .(conm, DataYearFiscal, calendaryear, SalesTurnoverNet, GlobalCompanyKey = as.integer(GlobalCompanyKey))][tfp_data, on = c(DataYearFiscal = 'fyear', GlobalCompanyKey = 'gvkey')]
aggregate_tfp = setkey(unique(asdf[, .(average_TFP = wtd.mean(TFP_level, weights = SalesTurnoverNet)), calendaryear]), calendaryear)[]
aggregate_tfp[calendaryear >= 1980, linear :=
                predict(lm(log(average_TFP) ~ calendaryear, data = aggregate_tfp[calendaryear >= 1980 & calendaryear < 2019]))]
#some patchy reporting in 2019, I think it's a fiscal year thing
aggregate_tfp[, little_zeta := exp(log(average_TFP) - linear)]
panel = asdf[calendaryear >= 1980, n_obs := .N, GlobalCompanyKey][n_obs == 40]
# panel[, average_TFP := wtd.mean(TFP_level, weights = SalesTurnoverNet), calendaryear]
panel[aggregate_tfp, on = 'calendaryear', average_TFP := i.average_TFP]
panel[, psi := TFP_level/average_TFP]
