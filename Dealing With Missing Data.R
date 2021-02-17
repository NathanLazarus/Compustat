dtcut[, AssetsOther := AssetsOther - na0(DeferredCharges) - na0(PrepaidExpenses)]
dtcut[, intangibleratio := IntangibleAssetsTotal/AssetsTotal]
setkey(dtcut, GlobalCompanyKey, calendaryear)

# dtcut[, intangiblesadded:= +is.na(IntangibleAssetsTotal)]
dtcut_no_NA_intangibles = dtcut[!is.na(IntangibleAssetsTotal)]
setkey(dtcut_no_NA_intangibles, GlobalCompanyKey, calendaryear)
dtcut[, intangibleratio := dtcut_no_NA_intangibles[dtcut, intangibleratio, roll = 'nearest']
    ][is.na(IntangibleAssetsTotal), `:=`(IntangibleAssetsTotal = pmin(intangibleratio*AssetsTotal, na0(Goodwill), pmax(na0(AssetsOther), 0)), 
                                        AssetsOther = na0(AssetsOther) - na0(pmin(intangibleratio*AssetsTotal, na0(Goodwill), pmax(na0(AssetsOther), 0))))]

dtcut[, twodigitsic := as.character(floor(SIC/100))]

intangiblemod = lm(intangibleratio~factor(calendaryear)+twodigitsic, data = dtcut)
dtcut[, predictedintangibleratio := pmax(predict(intangiblemod, dtcut), 0)
    ][is.na(IntangibleAssetsTotal), `:=`(IntangibleAssetsTotal = pmin(predictedintangibleratio*AssetsTotal, na0(Goodwill), pmax(na0(AssetsOther), 0)), 
                                        AssetsOther = na0(AssetsOther) - na0(pmin(predictedintangibleratio*AssetsTotal, na0(Goodwill), pmax(na0(AssetsOther), 0))))]


