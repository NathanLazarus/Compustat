input_data = c(withMarkups = 'IntermediateFiles/withMarkups.feather', 
               firmSoftwarePatenting = 'IntermediateFiles/firm_software_patenting.feather', 
               ITemployment = 'IntermediateFiles/IT_employment.feather', 
               inputOutputIT = 'IntermediateFiles/input_output_IT.feather', 
               BEACapitalAssets = 'IntermediateFiles/BEA Capital Assets by Industry.feather', 
               FernaldITCodes = 'IntermediateFiles/Fernald Categorical IT Intensity Codes.feather', 
               KLDData = 'IntermediateFiles/KLD Data Clean.feather')

output_files = c(foranalysis = 'IntermediateFiles/foranalysis.feather')

withMarkups = read_feather_dt(input_data['withMarkups'])

withMarkups[, marketvalue := MktVal]

withMarkups[, three_digit_HHI := sum((SalesTurnoverNet/sum(SalesTurnoverNet))^2), .(three_digit_NAICS, calendaryear)]
#create imputation datasets with different NAICS codes and average the resulting HHIs. Or try to solve for the average analytically.
#But don't treat all the imputed firms as small firms in a variety of industries


BEA_capital_assets = read_feather_dt(input_data['BEACapitalAssets'])

withMarkups[, true_six_digit_NAICS2 := true_six_digit_NAICS]
setkey(withMarkups, true_six_digit_NAICS, true_six_digit_NAICS2)
setkey(BEA_capital_assets, NAICSmin, NAICSmax)
howsthis2 = foverlaps(withMarkups[true_six_digit_NAICS < 999900], BEA_capital_assets)
howsthis2[, c('NAICSmin', 'NAICSmax') := NULL]


FernaldIT = read_feather_dt(input_data['FernaldITCodes'])
howsthis2[, IT_type := foverlaps(howsthis2, FernaldIT)[, IT_type]]


input_output_IT = read_feather_dt(input_data['inputOutputIT'])

setkey(howsthis2, true_six_digit_NAICS, true_six_digit_NAICS2)
setkey(input_output_IT, NAICSmin, NAICSmax)
howsthis3 = foverlaps(howsthis2, input_output_IT)
howsthis3[, c('NAICSmin', 'NAICSmax') := NULL]

IT_employment = read_feather_dt(input_data['ITemployment'])

IT_employment[, temp_unique_NAICSmin := (YEAR - 1900) * 1000000 + NAICSmin
            ][, temp_unique_NAICSmax := (YEAR - 1900) * 1000000 + NAICSmax
            ]
setkey(IT_employment, temp_unique_NAICSmin, temp_unique_NAICSmax)
howsthis3[, temp_unique_NAICS := (calendaryear - 1900) * 1000000 + true_six_digit_NAICS]
howsthis3[, temp_unique_NAICS2 := temp_unique_NAICS]
setkey(howsthis3, temp_unique_NAICS, temp_unique_NAICS2)
howsthis4 = foverlaps(howsthis3, IT_employment)

firm_software_patenting = read_feather_dt(input_data['firmSoftwarePatenting'])

howsthis4[firm_software_patenting, 
          on = c(calendaryear = 'appyear', GlobalCompanyKey = 'gvkey'), 
          `:=`(software_patents = i.patents, software_patents_rolling_5 = i.patents_rolling_5)]


# SDC_MA_data = read_feather_dt('SDC_data.feather')
howsthis4[, cusip6 := substr(cusip, 1, 6)]
setkey(howsthis4, cusip6)
# setkey(SDC_MA_data, `Acquiror Ultimate Parent CUSIP`)
# merged = SDC_MA_data[howsthis4]
howsthis4[, software_patents := NULL]
patent_data = read_feather_dt('patent_data_for_analysis.feather')
howsthis4[patent_data, `:=`(all_patents = all_patents, software_patents = software_patents), 
     on = c(cusip6 = 'owner', calendaryear = 'possession_year')]
howsthis4[is.na(software_patents), `:=`(software_patents = 0, all_patents = 0)]

KLD_data_clean = read_feather_dt(input_data['KLDData'])

howsthis4[KLD_data_clean, on = c('cusip6', calendaryear = 'year'), `:=`(
  laborStrength = i.laborStrength, laborConcern = i.laborConcern, 
  laborRelations = i.laborRelations, `Anticompetitive Practices` = `i.Anticompetitive Practices`
)]

tfp_data = fread('TFPData_updated_2019.csv')
howsthis4[tfp_data, on = c(DataYearFiscal = 'fyear', GlobalCompanyKey = 'gvkey'), tfp := i.TFP]


write_feather(howsthis4, output_files['foranalysis'])

rm(howsthis2, howsthis3)
