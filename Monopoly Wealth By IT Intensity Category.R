input_data = c(withMarkups = 'IntermediateFiles/withMarkups.feather',
               FernaldITCodes = 'IntermediateFiles/Fernald Categorical IT Intensity Codes.feather')

output_files = c(monopolyWealthByITSpreadsheet = 'SpreadsheetOutputs/MonopolyWealthByIT.xlsx')

withMarkups = read_feather_dt(input_data['withMarkups'])
FernaldIT = read_feather_dt(input_data['FernaldITCodes'])

withMarkups[, true_six_digit_NAICS2 := true_six_digit_NAICS]
setkey(withMarkups, true_six_digit_NAICS, true_six_digit_NAICS2)
setkey(FernaldIT, NAICSmin, NAICSmax)
withMarkups[, IT_type := foverlaps(withMarkups, FernaldIT)[, IT_type]]
aggregates_by_it_use = withMarkups[!is.na(monopolywealth) & !is.na(IT_type),
                                   .(monopolywealth = sum(monopolywealth * six_digit_ratio),
                                     totalwealth = sum(totalwealth * six_digit_ratio),
                                     marketvalue = sum(MktVal * six_digit_ratio)),
                                   .(calendaryear, IT_type)]
aggregates_wide = dcast(aggregates_by_it_use, 
                        calendaryear ~ IT_type,
                        value.var = c('monopolywealth', 'totalwealth', 'marketvalue'))

write.xlsx(aggregates_wide[between(calendaryear, 1950, 2019, incbounds = T)
                         ][, .SD, .SDcols = !c('calendaryear')],
           output_files['monopolyWealthByITSpreadsheet'],
           startCol = 12,
           startRow = 2)
