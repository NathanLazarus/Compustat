input_data = c(dtcut = 'IntermediateFiles/With Stochastic Crosswalk for Defunct NAICS Codes.feather', 
               Z1CurrentAndHistoricalPrices = 'Data/Z1 Assets at Current and Historical Prices.csv')

output_files = c(dtcutForSpreadsheets = 'IntermediateFiles/dtcut_for_spreadsheets.feather')

temporary_files_to_send_to_python =
  c(realEstateEquipmentToWeight = 'IntermediateFiles/real_estate_equipment_to_weight.csv', 
    assetPctsandZ1Weights = 'IntermediateFiles/asset_pcts_and_Z1_weights.csv')

python_output_files =
  c(optimalRealEstateEquipmentWeights = 'IntermediateFiles/Optimal_real_estate_equipment_Weights.csv', 
    optimalZ1weights = 'IntermediateFiles/OptimalZ1Weights.csv')

python_scripts = c(realEstateEquipmentWeights = 'real_estate_equipment_weights.py', 
                   GetOptimalZ1Weights = 'GetOptimalZ1Weights.py')

with_stochastic_crosswalk = read_feather_dt(input_data['dtcut'])
Z1 = fread(input_data['Z1CurrentAndHistoricalPrices'])

with_stochastic_crosswalk = with_stochastic_crosswalk[!is.na(SIC)]
# with_stochastic_crosswalk[is.na(NAICS), is_a_utility := roundDown(SIC, 100) == 4900]
# with_stochastic_crosswalk[!is.na(NAICS), is_a_utility := roundDown(SIC, 100) == 4900 | roundDown(NAICS, 10000) == 221000]
with_stochastic_crosswalk[, is_a_utility := roundDown(SIC, 100) == 4900]
with_stochastic_crosswalk[with_stochastic_crosswalk[is_a_utility == T], on = c('GlobalCompanyKey', 'DataYearFiscal'), is_a_utility := i.is_a_utility]

# dtcut = unique(with_stochastic_crosswalk, by = c('GlobalCompanyKey', 'DataYearFiscal'))
dtcut = with_stochastic_crosswalk[, is_most_likely_NAICS := frank(-ratio, ties.method =  'random'), .(GlobalCompanyKey, DataYearFiscal)
                                ][is_most_likely_NAICS == 1]




max_year = 2019

# Make it so there is one unique observation for every calendar year ----------------------
#
#firms choose the date at which their fiscal year ends, and they file their annual report, and unfortunately they sometimes change these dates
#two things can happen: they can change from reporting in, say, November to reporting in, say, March, in which case there's a missing calendar year
#(November 2006 for FY 2006 and March 2008 for FY 2007 lead to no observations in 2007).
#Or, if they go from March to November, they have two reports in one calendar year
dtcut[, haspreviousfiscalyear := (DataYearFiscal - 1) %in% DataYearFiscal, GlobalCompanyKey]
dtcut[, hascalendaryear := DataYearFiscal %in% calendaryear, GlobalCompanyKey]
dtcut[, missing := haspreviousfiscalyear & !hascalendaryear]
missings = dtcut[missing == T]
missings[, calendaryear := DataYearFiscal]
dtcut = rbind(dtcut, missings)

setkey(dtcut, GlobalCompanyKey, calendaryear)
dtcut[, n_obs_calendaryear := .N, .(calendaryear, GlobalCompanyKey)]
dtcut[n_obs_calendaryear > 1, `:=`(first_obs = datadate == min(datadate),
                                   last_obs = datadate == max(datadate)), .(GlobalCompanyKey, calendaryear)]

most_recent_available_data_for_companies_that_appear_twice_in_a_calendar_year =
   merge_and_reconcile(dtcut[last_obs == T], dtcut[first_obs == T],
                    join_cols = c('GlobalCompanyKey', 'calendaryear'))
# it's possible this could create a weird balance sheet, like if sales in December are 5 and COGS are NA,
# and sales in March are 2000 and COGS are 1500, then we'd see sales = 5 and COGS = 1500 after merge_and_reconcile

dtcut = rbind(dtcut[n_obs_calendaryear == 1],
              most_recent_available_data_for_companies_that_appear_twice_in_a_calendar_year)

dtcut[, c('haspreviousfiscalyear', 'hascalendaryear', 'missing', 'n_obs_calendaryear') := NULL]

dtcut = dtcut[calendaryear <= max_year]



dtcut[, AssetsOther := AssetsOther - na0(DeferredCharges) - na0(PrepaidExpenses)]
dtcut[, intangibleratio := IntangibleAssetsTotal/AssetsTotal]
dtcut[, intangibleratio := pmin(pmax(intangibleratio, 0), 1)]
setkey(dtcut, GlobalCompanyKey, calendaryear)

dtcut[, intangiblesadded := +is.na(IntangibleAssetsTotal)]
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






Z1[, calendaryear := as.integer(substring(Year, 1, 4))][, Year := NULL]
setnames(Z1, gsub("[^[:alnum:]]", "", names(Z1)))
Z1[, desired_real_estate_share := RealEstateHistorical/(RealEstateHistorical + EquipmentHistorical)]
Z1[, Equipmentadjustment := EquipmentCurrent/EquipmentHistorical]
Z1[, IPadjustment := IPCurrent/IPHistorical]
Z1[, Inventoriesadjustment := InventoriesCurrent/InventoriesHistorical]
Z1[, RealEstateadjustment := RealEstateCurrent/RealEstateHistorical]
Z1[, AllAssetsadjustment := AllAssetsCurrent/AllAssetsHistorical]
dtcut[Z1, on = 'calendaryear', `:=`(desired_real_estate_share = i.desired_real_estate_share, 
                                Equipmentadjustment = i.Equipmentadjustment, 
                                IPadjustment = i.IPadjustment, 
                                Inventoriesadjustment = i.Inventoriesadjustment, 
                                RealEstateadjustment = i.RealEstateadjustment, 
                                AllAssetsadjustment = i.AllAssetsadjustment)]

# c2log = function(x) {a = 0.001
# suppressWarnings(ifelse(x>a, log(x), log(a) + (1/a) * (x - a) - (1/(2 * a)) * (x - a)^2))
# }
# mult_dist = function(x, y) abs(c2log(x) - c2log(y))

dtcut[, financial := +(SIC >= 6000 & SIC < 6500)]
PPEcategories = c('MachineryandEquipment', 'NaturalResources', 'Other', 'Buildings', 
                  'ConstructioninProgress', 'LandandImprovements', 'Leases')
depreciationconstants = foreach(i = 1:length(PPEcategories), .combine = rbind) %do% {
  depreciation = dtcut[!is.na(eval(parse(text = paste0('PropertyPlantandEquipment', PPEcategories[i], 'Net')))) &
                         !is.na(eval(parse(text = paste0('PropertyPlantandEquipment', PPEcategories[i], 'atCost')))), 
                       sum(eval(parse(text = paste0('PropertyPlantandEquipment', PPEcategories[i], 'Net')))) /
                         sum(eval(parse(text = paste0('PropertyPlantandEquipment', PPEcategories[i], 'atCost'))))]
  #this is the important side effect
  dtcut[, PPEcategories[i] := ifelse(!is.na(eval(parse(text = paste0('PropertyPlantandEquipment', PPEcategories[i], 'Net')))), 
                                  eval(parse(text = paste0('PropertyPlantandEquipment', PPEcategories[i], 'Net'))), 
                                  depreciation * eval(parse(text = paste0('PropertyPlantandEquipment', PPEcategories[i], 'atCost'))))]
  data.table(PPEcategory = PPEcategories[i], depreciationconstant = depreciation)
}
dtcut[, equipment := sum(MachineryandEquipment, 
                      NaturalResources, 
                      ConstructioninProgress, na.rm = T), by = 1:nrow(dtcut)]
dtcut[, realestate := sum(Buildings, 
                       LandandImprovements, 
                       Leases, na.rm = T), by = 1:nrow(dtcut)]
dtcut[, PPEuncategorized := PropertyPlantandEquipmentTotalNet - na0(equipment) - na0(realestate)]
dtcut[is.na(PropertyPlantandEquipmentTotalNet), PPEuncategorized := Other]
dtcut[PPEuncategorized < 0, realestate_share := na0(realestate)/(na0(realestate) + na0(equipment))
    ][PPEuncategorized < 0, realestate := na0(realestate) + na0(PPEuncategorized * realestate_share)
    ][PPEuncategorized < 0, equipment := na0(equipment) + na0(PPEuncategorized * (1 - realestate_share))
    ][PPEuncategorized < 0, PPEuncategorized := 0
    ]

# dtcut[, equipmentpct := equipment/(equipment + realestate + PPEuncategorized)]
# dtcut[, realestatepct := realestate/(equipment + realestate + PPEuncategorized)]
# dtcut[, mostly_accounted_for := equipmentpct + realestatepct > 0.85]
# dtcut[, calendaryear2 := calendaryear]
# dtcut_mostly_accounted_for = dtcut[mostly_accounted_for == T]
# setkey(dtcut_mostly_accounted_for, GlobalCompanyKey, calendaryear)
# setkey(dtcut, GlobalCompanyKey, calendaryear)
# re_merge = dtcut_mostly_accounted_for[dtcut, , roll = 'nearest']
# dtcut[, c('mergedre', 'mergedeq', 'mergeyear', 'added')  :=  re_merge[, .(equipmentpct, realestatepct, calendaryear2, calendaryear2 != i.calendaryear2)]]


#can we guess at a firm's equipment to real estate ratio using sectoral data?
setkey(dtcut, calendaryear, twodigitsic)
industry_aggregate_real_estate_share = dtcut[, .(sector_re_share = (sum(realestate, na.rm = T)) / (sum(realestate, equipment, na.rm = T))), twodigitsic]
industry_year_PPE_aggregates = dtcut[, .(realestate_categorized = sum(realestate, na.rm = T), 
                                        equipment_categorized = sum(equipment, na.rm = T), 
                                        uncategorized = sum(PPEuncategorized, na.rm = T), 
                                        desired_real_estate_share = median(desired_real_estate_share)), 
                                     .(calendaryear, twodigitsic)]
industry_year_PPE_aggregates[industry_aggregate_real_estate_share, on = 'twodigitsic', sector_re_share := i.sector_re_share
                           ][, total := sum(realestate_categorized, equipment_categorized, uncategorized, na.rm = T), by = 1:nrow(industry_year_PPE_aggregates)
                           ][, min_re_share := realestate_categorized/total]
setkey(industry_year_PPE_aggregates, calendaryear, twodigitsic)
fwrite(industry_year_PPE_aggregates[, .(realestate_categorized, uncategorized, total, desired_real_estate_share, sector_re_share, calendaryear, twodigitsic)], 
       temporary_files_to_send_to_python['realEstateEquipmentToWeight'], col.names = T)

shell(python_scripts['realEstateEquipmentWeights'])

optimal_real_estate_weights = fread(python_output_files['optimalRealEstateEquipmentWeights'])
optimal_real_estate_weights[, twodigitsic := as.character(twodigitsic)]
setkey(optimal_real_estate_weights, calendaryear, twodigitsic)
setkey(dtcut, calendaryear, twodigitsic)
dtcut[optimal_real_estate_weights, uncategorized_real_estate_share := i.uncategorized_real_estate_share]

dtcut[, realestate := na0(realestate) + na0(PPEuncategorized * uncategorized_real_estate_share)
      ][, equipment := na0(equipment) + na0(PPEuncategorized * (1 - uncategorized_real_estate_share))]


dtcut[IntangibleAssetsTotal == 0 & Goodwill > 0, `:=`(IntangibleAssetsTotal = Goodwill, AssetsOther = AssetsOther - Goodwill)]
dtcut[, goodwillpct := Goodwill/IntangibleAssetsTotal]
# dtcut[, sum(Goodwill, na.rm = T)/sum(IntangibleAssetsTotal * !is.na(Goodwill), na.rm = T), .(calendaryear, twodigitsic)]
dtcut[, stryear_1988_or_later := factor(pmax(calendaryear, 1988))]
dtcut[, twodigitsic_after_1988 := ifelse(twodigitsic == '90', '65', ifelse(twodigitsic == '84', '79', twodigitsic))]
goodwillmod = lm(goodwillpct~stryear_1988_or_later + twodigitsic_after_1988, data = dtcut)
dtcut[, goodwill_pct_year_sector := pmax(predict(goodwillmod, dtcut), 0)]

setkey(dtcut, GlobalCompanyKey, calendaryear)
dtcut[, calendaryear2 := calendaryear]
dtcut_no_NA_goodwillpct = dtcut[!is.na(goodwillpct)]
setkey(dtcut_no_NA_goodwillpct, GlobalCompanyKey, calendaryear)
merged = dtcut_no_NA_goodwillpct[dtcut, , roll = 'nearest']
merged[abs(calendaryear2 - i.calendaryear2) > 20, goodwillpct := NA_real_]
dtcut[, goodwillpct_firm := merged[, goodwillpct]]
dtcut[is.na(Goodwill) & !is.na(goodwillpct_firm) & !is.na(IntangibleAssetsTotal), 
      `:=`(Goodwill = IntangibleAssetsTotal * goodwillpct_firm, goodwilladded = 1)
      ][is.na(Goodwill) & is.na(goodwillpct_firm) & !is.na(IntangibleAssetsTotal), 
        `:=`(Goodwill = IntangibleAssetsTotal * goodwill_pct_year_sector, goodwilladded = 2)]

dtcut[, IntellectualProperty := IntangibleAssetsTotal - na0(Goodwill)]


# merged[, keyyear := abs(calendaryear2 - i.calendaryear2)>0.5&abs(calendaryear2 - i.calendaryear2)<1.5]
#decade max?

# merged[keyyear == T, if_missing_goodwill_in_intangibles := goodwillpct * IntangibleAssetsTotal]
# merged[keyyear == T, if_missing_goodwill_in_AssetsOther := goodwillpct * (IntangibleAssetsTotal/(1 - goodwillpct))]
# dtcut[, goodwillpct := dtcut_no_NA_goodwillpct[dtcut, goodwillpct, roll = 'nearest']]
# dtcut[is.na(Goodwill)&!is.na(goodwillpct)&!is.na(IntangibleAssetsTotal)&(AssetsOther>IntangibleAssetsTotal * goodwillpct), 
#       Goodwill := IntangibleAssetsTotal * goodwillpct
#       ][is.na(Goodwill)&!is.na(goodwillpct)&!is.na(IntangibleAssetsTotal)&(AssetsOther>IntangibleAssetsTotal * goodwillpct), 
#         AssetsOther := AssetsOther - IntangibleAssetsTotal * goodwillpct]

# possiblecases: goodwill NA, and goodwill is in Intangibles, goodwill NA and goodwill in AssetsOther
# goodwill == intangibles and intellectual property is in AssetsOther
# if intangibles are NA: fill them and use goodwill pct, pref at the firm level, but if not, then at the sector/year level (watch out for completely missing sector - years)

# dtcut[, intellectualpropertypct := IntellectualProperty/IntangibleAssetsTotal]
# dtcut_no_NA_intellectualproperty = dtcut[!is.na(IntellectualProperty)&IntellectualProperty != 0]
# setkey(dtcut_no_NA_intellectualproperty, GlobalCompanyKey, calendaryear)
# dtcut[, intellectualpropertypct := dtcut_no_NA_intellectualproperty[dtcut, intellectualpropertypct, roll = 'nearest']]
# dtcut[is.na(IntellectualProperty)&!is.na(intellectualpropertypct)&!is.na(IntangibleAssetsTotal)&(AssetsOther>IntangibleAssetsTotal * intellectualpropertypct), 
#       IntellectualProperty2 := IntangibleAssetsTotal * intellectualpropertypct
#       ][is.na(IntellectualProperty)&!is.na(intellectualpropertypct)&!is.na(IntangibleAssetsTotal)&(AssetsOther>IntangibleAssetsTotal * intellectualpropertypct), 
#         AssetsOther := AssetsOther - IntangibleAssetsTotal * intellectualpropertypct]
# 
dtcut[is.na(CashandShortTermInvestments) & (!is.na(Cash) | !is.na(ShortTermInvestmentsTotal)), 
      CashandShortTermInvestments := na0(Cash) + na0(ShortTermInvestmentsTotal)]
dtcut[is.na(ReceivablesTotal) & (!is.na(ReceivablesTrade) | !is.na(ReceivablesCurrentOther)), 
      ReceivablesTotal := na0(ReceivablesTrade) + na0(ReceivablesCurrentOther)]
dtcut[is.na(InventoriesTotal), Inventories_component_sum := na0(InventoriesRawMaterials) + 
        na0(InventoriesOther) + na0(InventoriesWorkInProcess) + na0(InventoriesFinishedGoods) + 
        na0(InventoryStockOther) + na0(InventoryStockRealEstateHeldforDevelopment) + na0(InventoryStockRealEstateHeldforSale) + na0(InventoryStockRealEstateUnderDevelopment)]
dtcut[is.na(InventoriesTotal) & Inventories_component_sum != 0, InventoriesTotal := Inventories_component_sum]

dtcut[, CurrentAssets_categorized := na0(InventoriesTotal) + na0(ReceivablesTotal) + 
        na0(CashandShortTermInvestments) + na0(CurrentAssetsOtherTotal)]
dtcut[, CurrentAssets_uncategorized := CurrentAssetsTotal - CurrentAssets_categorized]

dtcut[, c('oldInventoriesTotal', 'oldReceivablesTotal', 'oldCashandShortTermInvestments', 'oldCurrentAssetsOtherTotal') := .(InventoriesTotal, ReceivablesTotal, CashandShortTermInvestments, CurrentAssetsOtherTotal)]

dtcut[, current_assets_good := (CurrentAssets_uncategorized/CurrentAssetsTotal) < 0.001]
dtcut[, ca_inventories_pct := na0(InventoriesTotal)/CurrentAssets_categorized
    ][, ca_receivables_pct := na0(ReceivablesTotal)/CurrentAssets_categorized
    ][, ca_cash_pct := na0(CashandShortTermInvestments)/CurrentAssets_categorized
    ][, ca_other_pct := na0(CurrentAssetsOtherTotal)/CurrentAssets_categorized]
setkey(dtcut, GlobalCompanyKey, calendaryear)
dtcut_current_assets_are_good = dtcut[current_assets_good == T]
dtcut_current_assets_are_good[, ca_merge_success := 1]
setkey(dtcut_current_assets_are_good, GlobalCompanyKey, calendaryear)
ca_merge = dtcut_current_assets_are_good[dtcut, , roll = 'nearest']
dtcut[, c('ca_inventories_pct_firm', 'ca_receivables_pct_firm', 'ca_cash_pct_firm', 'ca_other_pct_firm', 'ca_merge_success') :=
        ca_merge[, .(ca_inventories_pct, ca_receivables_pct, ca_cash_pct, ca_other_pct, ca_merge_success)]]
dtcut[, total_current_assets_year_sector := sum(InventoriesTotal, ReceivablesTotal, CashandShortTermInvestments, CurrentAssetsOtherTotal, na.rm = T), .(calendaryear, twodigitsic)]
stopifnot(nrow(dtcut[total_current_assets_year_sector <= 0]) == 0)
dtcut[, ca_inventories_pct_year_sector := sum(InventoriesTotal, na.rm = T) / total_current_assets_year_sector, .(calendaryear, twodigitsic)
    ][, ca_receivables_pct_year_sector := sum(ReceivablesTotal, na.rm = T) / total_current_assets_year_sector, .(calendaryear, twodigitsic)
    ][, ca_cash_pct_year_sector := sum(CashandShortTermInvestments, na.rm = T) / total_current_assets_year_sector, .(calendaryear, twodigitsic)
    ][, ca_other_pct_year_sector := sum(CurrentAssetsOtherTotal, na.rm = T) / total_current_assets_year_sector, .(calendaryear, twodigitsic)]

dtcut[ca_merge_success == 1, c('ca_other_pct', 'ca_inventories_pct', 'ca_receivables_pct', 'ca_cash_pct') := 
        .(ca_other_pct_firm, ca_inventories_pct_firm, ca_receivables_pct_firm, ca_cash_pct_firm)]

dtcut[conm == 'U S CHINA MINING GROUP INC' & (calendaryear == 2012 | calendaryear == 2013), c('ca_other_pct', 'ca_inventories_pct', 'ca_receivables_pct', 'ca_cash_pct') := .(0, 0, 1, 0)]

dtcut[is.na(ca_merge_success), c('ca_other_pct', 'ca_inventories_pct', 'ca_receivables_pct', 'ca_cash_pct') :=
        .(ca_other_pct_year_sector, ca_inventories_pct_year_sector, ca_receivables_pct_year_sector, ca_cash_pct_year_sector)]
dtcut[, CurrentAssetsOtherTotal := na0(CurrentAssetsOtherTotal) + na0(ca_other_pct * CurrentAssets_uncategorized)
    ][, InventoriesTotal := na0(InventoriesTotal) + na0(ca_inventories_pct * CurrentAssets_uncategorized)
    ][, ReceivablesTotal := na0(ReceivablesTotal) + na0(ca_receivables_pct * CurrentAssets_uncategorized)
    ][, CashandShortTermInvestments := na0(CashandShortTermInvestments) + na0(ca_cash_pct * CurrentAssets_uncategorized)]
# dtcut[, CurrentAssets_categorized_check := na0(InventoriesTotal) + na0(ReceivablesTotal) + 
#         na0(CashandShortTermInvestments) + na0(CurrentAssetsOtherTotal)]
# dtcut[, CurrentAssets_uncategorized_check := CurrentAssetsTotal - CurrentAssets_categorized_check]


assetclasscols =  c('AssetsOther', 'DeferredCharges', 'PrepaidExpenses', 
                    'InventoriesTotal', 'ReceivablesTotal', 'CashandShortTermInvestments', 'CurrentAssetsOtherTotal', 
                    'realestate', 'equipment', 
                    'InvestmentandAdvancesEquity', 'InvestmentandAdvancesOther', 
                    'IntellectualProperty', 'Goodwill')
tangible_assetclasscols =  c('AssetsOther', 'DeferredCharges', 'PrepaidExpenses', 
                             'InventoriesTotal', 'ReceivablesTotal', 'CashandShortTermInvestments', 'CurrentAssetsOtherTotal', 
                             'realestate', 'equipment', 
                             'InvestmentandAdvancesEquity', 'InvestmentandAdvancesOther')


eval(parse(text = paste0('dtcut[, assets_accounted_for := sum(', paste0(assetclasscols, collapse = ','), ', na.rm = T), by = 1:nrow(dtcut)]')))


dtcut[, residual := AssetsTotal - assets_accounted_for]

dtcut[, asset_categories_add_up := abs(residual / AssetsTotal) < 0.01]

dtcut[, total_categorized_assets := eval(parse(text = paste0('na0(', assetclasscols, ')', collapse = '+')))]
dtcut[, total_categorized_tangible_assets := eval(parse(text = paste0('na0(', tangible_assetclasscols, ')', collapse = '+')))]


throwaway = foreach(asset = assetclasscols) %do% {
  dtcut[, paste0('residual_', asset, '_pct_firm') := eval(parse(text = paste0('na0(', asset, ')/total_categorized_assets')))]
  if (asset %in% tangible_assetclasscols) dtcut[, paste0('residual_', asset, '_tangible_pct_firm') := eval(parse(text = paste0('na0(', asset, ')/total_categorized_tangible_assets')))]
  NULL
}

dtcut_asset_categories_add_up = dtcut[asset_categories_add_up == T]
dtcut_asset_categories_add_up[, residual_merge_success := 1]
setkey(dtcut_asset_categories_add_up, GlobalCompanyKey, calendaryear)
setkey(dtcut, GlobalCompanyKey, calendaryear)
residual_merge = dtcut_asset_categories_add_up[dtcut, roll = 'nearest']
setkey(residual_merge, GlobalCompanyKey, calendaryear)
dtcut[, residual_merge_success := residual_merge[, residual_merge_success]]

dtcut[, total_categorized_assets_year_sector := eval(parse(text = paste0('sum(', paste(assetclasscols, collapse = ','), ', na.rm = T)'))), .(calendaryear, twodigitsic)]
dtcut[, total_categorized_tangible_assets_year_sector := eval(parse(text = paste0('sum(', paste(tangible_assetclasscols, collapse = ','), ', na.rm = T)'))), .(calendaryear, twodigitsic)]

stopifnot(nrow(dtcut[total_categorized_assets_year_sector <= 0]) == 0)
stopifnot(nrow(dtcut[total_categorized_tangible_assets_year_sector <= 0]) == 0)
dtcut[total_categorized_assets_year_sector <= 0, total_categorized_assets_year_sector := 0.1]
dtcut[total_categorized_tangible_assets_year_sector <= 0, total_categorized_tangible_assets_year_sector := 0.1]

throwaway = foreach(asset = assetclasscols) %do% {
  dtcut[, paste0('residual_', asset, '_pct_year_sector') := eval(parse(text = paste0('sum(', asset, ', na.rm = T)/total_categorized_assets_year_sector'))), .(calendaryear, twodigitsic)]
  dtcut[, paste0('residual_', asset, '_pct_firm') := residual_merge[, paste0('residual_', asset, '_pct_firm'), with = F][[1]]]
  dtcut[residual_merge_success == 1, paste0('residual_', asset, '_pct') := eval(parse(text = paste0('residual_', asset, '_pct_firm')))]
  dtcut[is.na(residual_merge_success), paste0('residual_', asset, '_pct') := eval(parse(text = paste0('residual_', asset, '_pct_year_sector')))]
  
  if (asset %in% tangible_assetclasscols) {
    dtcut[, paste0('residual_', asset, '_tangible_pct_year_sector') := eval(parse(text = paste0('sum(', asset, ', na.rm = T)/total_categorized_tangible_assets_year_sector'))), .(calendaryear, twodigitsic)]
    dtcut[, paste0('residual_', asset, '_tangible_pct_firm') := residual_merge[, paste0('residual_', asset, '_tangible_pct_firm'), with = F]]
    dtcut[residual_merge_success == 1, paste0('residual_', asset, '_tangible_pct') := eval(parse(text = paste0('residual_', asset, '_tangible_pct_firm')))]
    dtcut[is.na(residual_merge_success), paste0('residual_', asset, '_tangible_pct') := eval(parse(text = paste0('residual_', asset, '_tangible_pct_year_sector')))]
  }
  
  dtcut[intangiblesadded == 0, paste0(asset) := eval(parse(text = paste0('na0(', asset, ') +', paste0('residual_', asset, '_pct'), '*residual')))]
  if (asset %in% tangible_assetclasscols) dtcut[intangiblesadded == 1, paste0(asset) := eval(parse(text = paste0('na0(', asset, ') +', paste0('residual_', asset, '_tangible_pct'), '*residual')))]
  NULL
}

eval(parse(text = paste0('dtcut[, pseudoassets_check := sum(', paste0(assetclasscols, collapse = ','), ', na.rm = T), by = 1:nrow(dtcut)]')))

dtcut[, residual_check := AssetsTotal - pseudoassets_check]

sums_by_year = dtcut[, lapply(.SD, sum, na.rm = T), .(financial, calendaryear), .SDcols =
                       c(assetclasscols, 'AssetsTotal')]

setkey(sums_by_year, calendaryear, financial)
sums_by_year[Z1, on = 'calendaryear', `:=`(Equipmentadjustment = i.Equipmentadjustment, 
                                       IPadjustment = i.IPadjustment, 
                                       Inventoriesadjustment = i.Inventoriesadjustment, 
                                       RealEstateadjustment = i.RealEstateadjustment, 
                                       AllAssetsadjustment = i.AllAssetsadjustment)]

pctsbyfin = cbind(sums_by_year[, .(financial, calendaryear)], 
                  sums_by_year[, assetclasscols, with = F]/sums_by_year$AssetsTotal, 
                  sums_by_year[, .(Equipmentadjustment, IPadjustment, Inventoriesadjustment, RealEstateadjustment, AllAssetsadjustment)])

eval(parse(text = paste0('pctsbyfin[, accountedfor2 := sum(', paste0(assetclasscols, collapse = ','), ', na.rm = T), by = 1:nrow(pctsbyfin)]')))

pctsbyfin[, dontrevalue := ReceivablesTotal + DeferredCharges + PrepaidExpenses + 
            Goodwill + CashandShortTermInvestments + AssetsOther + 
            InvestmentandAdvancesEquity + InvestmentandAdvancesOther]

pctsbyfin[, adjustment_if_categories_matched_up := InventoriesTotal * Inventoriesadjustment + realestate * RealEstateadjustment + equipment * Equipmentadjustment + IntellectualProperty * IPadjustment + (1 - IntellectualProperty - equipment - realestate - InventoriesTotal)]

fwrite(pctsbyfin[financial == 0, .(InventoriesTotal, realestate, equipment, IntellectualProperty, Inventoriesadjustment, RealEstateadjustment, Equipmentadjustment, IPadjustment, AllAssetsadjustment)], 
       temporary_files_to_send_to_python['assetPctsandZ1Weights'], col.names = F)

shell(python_scripts['GetOptimalZ1Weights'])

optimalweights = fread(python_output_files['optimalZ1weights'])

pctsbyfin = merge(pctsbyfin, optimalweights, by = 'calendaryear')
pctsbyfin[, optimal_adjustment := InventoriesTotal * reweightedInventoriesadjustment + 
            realestate * reweightedRealEstateadjustment + 
            equipment * reweightedEquipmentadjustment + 
            IntellectualProperty * reweightedIntellectualPropertyadjustment + 
            (1 - IntellectualProperty - equipment - realestate - InventoriesTotal)]

setkey(optimalweights, calendaryear)
setkey(dtcut, calendaryear)
dtcut = dtcut[optimalweights, `:=`(reweightedInventoriesadjustment = i.reweightedInventoriesadjustment, 
                                  reweightedRealEstateadjustment = i.reweightedRealEstateadjustment, 
                                  reweightedEquipmentadjustment = i.reweightedEquipmentadjustment, 
                                  reweightedIntellectualPropertyadjustment = i.reweightedIntellectualPropertyadjustment)]

dtcut[, InventoriesTotal := InventoriesTotal * reweightedInventoriesadjustment
    ][, realestate := realestate * reweightedRealEstateadjustment
    ][, equipment := equipment * reweightedEquipmentadjustment
    ][, IntellectualProperty := IntellectualProperty * reweightedIntellectualPropertyadjustment
    ][, Goodwill := Goodwill * reweightedIntellectualPropertyadjustment]

eval(parse(text = paste0('dtcut[, AssetsTotal := sum(', paste0(assetclasscols, collapse = ', '), ', na.rm = T), by = 1:nrow(dtcut)]')))
#without the adjustments, this is exactly equal to AssetsTotal (the difference is less than 1e-10)

dtcut[, IntangibleAssetsTotal := sum(IntellectualProperty, Goodwill, na.rm = T), by = 1:nrow(dtcut)]


# dtcut[, liabilitiesadded := +is.na(LiabilitiesTotal)]
dtcut[, liabilityratio := LiabilitiesTotal/AssetsTotal]
dtcut[, liabilityratio := pmin(pmax(liabilityratio, 0), 10)]
dtcut_no_NA_liabilities = dtcut[!is.na(LiabilitiesTotal)]
setkey(dtcut, GlobalCompanyKey, calendaryear)
setkey(dtcut_no_NA_liabilities, GlobalCompanyKey, calendaryear)
dtcut[, liabilityratio := dtcut_no_NA_liabilities[dtcut, liabilityratio, roll = 'nearest']
    ][is.na(LiabilitiesTotal), LiabilitiesTotal := liabilityratio * AssetsTotal]
liabilitymod = lm(liabilityratio ~ factor(calendaryear) + twodigitsic, data = dtcut)
dtcut[, predictedliabilityratio := pmax(predict(liabilitymod, dtcut), 0)
    ][is.na(LiabilitiesTotal), LiabilitiesTotal := predictedliabilityratio * AssetsTotal]

dtcut[, monopolywealth := MktVal - AssetsTotal + IntangibleAssetsTotal + LiabilitiesTotal
    ][, totalwealth := MktVal + LiabilitiesTotal]

dtcut_without_cols_created_here = dtcut[, !(desired_real_estate_share:liabilityratio)]

write_feather(dtcut_without_cols_created_here, output_files['dtcutForSpreadsheets'])
