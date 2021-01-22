setwd("C:/Users/Nathan/Downloads/Compustat")

getCharCols = function(x) {
  jkl = readLines(x,n = 2)[2]
  cols = strsplit(jkl,',')[[1]]
  grep('"',cols)
}

fread_and_getCharCols = function(x) {
  fread(x, colClasses = list(character = getCharCols(x)))
}

withSixDigit = fread_and_getCharCols('withSixDigit.csv')

ppi = fread('ppi.csv')
ppi[,ppi_inflation := PPI/shift(PPI) - 1]
capitalcost = fread('FedFundsAndInflation.csv')
capitalcost[, year := year(Date)]
capitalcost[, real_interest := FedFundsRate - Inflation]
capitalcost[ppi, on = 'Date', ppi := i.ppi_inflation]
capitalcost[, real_interest_ppi := FedFundsRate - (100 * ppi)]
capitalcost[, usercost := (12 + real_interest) / 100]
capitalcost[, usercost_ppi := (12 + real_interest_ppi) / 100]

withSixDigit[capitalcost, on = c(calendaryear = 'year'), usercost := i.usercost]
withSixDigit[, kexp := PropertyPlantandEquipmentTotalGross * usercost]
withSixDigit[, DLE_markup := SalesTurnoverNet / (CostofGoodsSold + kexp)]
withSixDigit[, Traina_markup := SalesTurnoverNet / (CostofGoodsSold + SellingGeneralandAdministrativeExpense + kexp)]
withSixDigit[, DLE_profit := (SalesTurnoverNet - CostofGoodsSold -
                                 SellingGeneralandAdministrativeExpense - kexp) / SalesTurnoverNet]

fwrite(withSixDigit, 'withMarkups.csv', quote = T)