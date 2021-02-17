# Calculate Market Value ------------------

dtcut[conm == 'DELHAIZE AMERICA INC' & calendaryear == 2001 & CommonSharesOutstanding == 91125.785, 
      CommonSharesOutstanding := dtcut[conm == 'DELHAIZE AMERICA INC' & calendaryear == 2000]$CommonSharesOutstanding]

dtcut[, MktVal := MarketValueTotalFiscal]
dtcut[is.na(MktVal), MktVal := PriceCloseAnnualFiscal * CommonSharesOutstanding]
dtcut[is.na(MktVal), MktVal := PriceCloseAnnualCalendar * CommonSharesOutstanding]
dtcut[is.na(PreferredPreferenceStockCapitalTotal) & !is.na(PreferredPreferenceStockRedeemable), 
      PreferredPreferenceStockCapitalTotal := PreferredPreferenceStockRedeemable]

dtcut[, preferred := pmax(PreferredPreferenceStockCapitalTotal, PreferredStockLiquidatingValue, PreferredStockRedemptionValue, PreferredStockConvertible, na.rm = T)]
dtcut[!is.na(preferred), MktVal := MktVal + preferred]
dtcut = dtcut[MktVal != 0 | is.na(MktVal)] #drop about 100 firms with 0 common shares outstanding, mostly firms in the process of dissolving
