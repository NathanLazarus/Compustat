getCharCols = function(x) {
  second_line = readLines(x,n = 2)[2]
  cols = strsplit(second_line, ',')[[1]]
  grep('"',cols)
}

fread_and_getCharCols = function(x) {
  fread(x, colClasses = list(character = getCharCols(x)))
}

withSixDigit = fread_and_getCharCols('IntermediateFiles/withSixDigit.csv')

capitalcost = readRDS('Data/FRED Data (Inflation and Interest Rates).rds')
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
withSixDigit[, DLE_profit := (SalesTurnoverNet - CostofGoodsSold -
                                 SellingGeneralandAdministrativeExpense) / SalesTurnoverNet]

fwrite(withSixDigit, 'IntermediateFiles/withMarkups.csv', quote = T)