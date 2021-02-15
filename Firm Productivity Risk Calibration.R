#Getting calibrated values for "psi" using firm TFP risk
input_data = c(withThreeDigit = 'IntermediateFiles/withThreeDigit.csv', 
               TFPData = 'Data/TFPData_updated_2019.csv')

withThreeDigit = fread_and_getCharCols(input_data['withThreeDigit'])
# withThreeDigit[, value_added := oibdp + emp*wage]


tfp_data = fread(input_data['TFPData'])[, TFP_level := exp(TFP)]
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
