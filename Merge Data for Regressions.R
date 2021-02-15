input_data = c(withMarkups = 'IntermediateFiles/withMarkups.csv',
               firmSoftwarePatenting = 'IntermediateFiles/firm_software_patenting.csv',
               ITemployment = 'IntermediateFiles/IT_employment.csv',
               inputOutputIT = 'IntermediateFiles/input_output_IT.csv',
               BEACapitalAssets = 'IntermediateFiles/BEA Capital Assets by Industry.rds',
               FernaldITCodes = 'IntermediateFiles/Fernald Categorical IT Intensity Codes.rds')
output_files = c(foranalysis = 'IntermediateFiles/foranalysis.csv',
                 monopolyWealthByITSpreadsheet = 'SpreadsheetOutputs/MonopolyWealthByIT.xlsx')

withMarkups = fread_and_getCharCols(input_data['withMarkups'])

withMarkups[, marketvalue := MktVal]

withMarkups[, three_digit_HHI := sum((SalesTurnoverNet/sum(SalesTurnoverNet))^2), .(three_digit_NAICS,calendaryear)]
#create imputation datasets with different NAICS codes and average the resulting HHIs. Or try to solve for the average analytically.
#But don't treat all the imputed firms as small firms in a variety of industries


BEA_capital_assets = readRDS(input_data['BEACapitalAssets'])

withMarkups[, true_six_digit_NAICS2 := true_six_digit_NAICS]
setkey(withMarkups, true_six_digit_NAICS, true_six_digit_NAICS2)
setkey(BEA_capital_assets, NAICSmin, NAICSmax)
howsthis2 = foverlaps(withMarkups[true_six_digit_NAICS < 999900], BEA_capital_assets)
howsthis2[, c('NAICSmin', 'NAICSmax') := NULL]


FernaldIT = readRDS(input_data['FernaldITCodes'])
howsthis2[, IT_type := foverlaps(howsthis2, FernaldIT)[, IT_type]]


input_output_IT = fread_and_getCharCols(input_data['inputOutputIT'])

setkey(howsthis2,true_six_digit_NAICS,true_six_digit_NAICS2)
setkey(input_output_IT,NAICSmin,NAICSmax)
howsthis3 = foverlaps(howsthis2, input_output_IT)
howsthis3[, c('NAICSmin','NAICSmax') := NULL]

IT_employment = fread_and_getCharCols(input_data['ITemployment'])

IT_employment[, temp_unique_NAICSmin := (YEAR - 1900) * 1000000 + NAICSmin
            ][, temp_unique_NAICSmax := (YEAR - 1900) * 1000000 + NAICSmax
            ]
setkey(IT_employment, temp_unique_NAICSmin, temp_unique_NAICSmax)
howsthis3[, temp_unique_NAICS := (calendaryear - 1900) * 1000000 + true_six_digit_NAICS]
howsthis3[, temp_unique_NAICS2 := temp_unique_NAICS]
setkey(howsthis3, temp_unique_NAICS, temp_unique_NAICS2)
howsthis4 = foverlaps(howsthis3, IT_employment)

firm_software_patenting = fread_and_getCharCols(input_data['firmSoftwarePatenting'])

howsthis4[firm_software_patenting,
          on = c(calendaryear = 'appyear', GlobalCompanyKey = 'gvkey'),
          `:=`(software_patents = i.patents, software_patents_rolling_5 = i.patents_rolling_5)]


# SDC_MA_data = readRDS('SDC_data.rds')
howsthis4[, cusip6 := substr(cusip, 1, 6)]
setkey(howsthis4, cusip6)
# setkey(SDC_MA_data, `Acquiror Ultimate Parent CUSIP`)
# merged = SDC_MA_data[howsthis4]
howsthis4[, software_patents := NULL]
patent_data = readRDS('patent_data_for_analysis.RDS')
howsthis4[patent_data, `:=`(all_patents = all_patents, software_patents = software_patents),
     on = c(cusip6 = 'owner', calendaryear = 'possession_year')]
howsthis4[is.na(software_patents), `:=`(software_patents = 0, all_patents = 0)]

tfp_data = fread('TFPData_updated_2019.csv')
howsthis4[tfp_data, on = c(DataYearFiscal = 'fyear', GlobalCompanyKey = 'gvkey'), tfp := i.TFP]


fwrite(howsthis4, output_files['foranalysis'])

rm(howsthis2, howsthis3)