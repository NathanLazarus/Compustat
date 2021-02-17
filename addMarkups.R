input_data = c(withSixDigit = 'IntermediateFiles/withSixDigit.feather', 
               FREDData = 'Data/FRED Data (Inflation and Interest Rates).feather')

output_files = c(withMarkups = 'IntermediateFiles/withMarkups.feather')


withSixDigit = read_feather_dt(input_data['withSixDigit'])

capitalcost = readRDS(input_data['FREDData'])
capitalcost[, real_interest := FedFundsRate - cpi_inflation]
capitalcost[, real_interest_ppi := FedFundsRate - ppi_inflation]
capitalcost[, usercost := (12 + real_interest) / 100]
capitalcost[, usercost_ppi := (12 + real_interest_ppi) / 100]

production_function_coefs = setnames(data.table(read_dta('Data/theta_W_s_window.dta')), 
                                     c('ind2d', 'theta_WI1_ct'), c('true_two_digit_NAICS', 'theta_v'))
withSixDigit[capitalcost, on = c(calendaryear = 'year'), usercost := i.usercost]
withSixDigit[, kexp := PropertyPlantandEquipmentTotalGross * usercost]
withSixDigit[, DLE_markup := theta_v * SalesTurnoverNet / CostofGoodsSold]
theta_vx = 0.95
withSixDigit[, Traina_markup := theta_vx * SalesTurnoverNet / (CostofGoodsSold + SellingGeneralandAdministrativeExpense)]
#is kexp in profits?
withSixDigit[, DLE_profit := (SalesTurnoverNet - CostofGoodsSold -
                                 SellingGeneralandAdministrativeExpense) / SalesTurnoverNet]

write_feather(withSixDigit, output_files['withMarkups'])
