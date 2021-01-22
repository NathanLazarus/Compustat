setwd("C:/Users/Nathan/Downloads/Compustat")
library(foreach)
library(iterators)
library(snow)
library(doSNOW)


getCharCols = function(x) {
  jkl = readLines(x,n = 2)[2]
  cols = strsplit(jkl,',')[[1]]
  grep('"',cols)
}

fread_and_getCharCols = function(x) {
  fread(x, colClasses = list(character = getCharCols(x)))
}

dtcut = fread_and_getCharCols('dtcut.csv')
Z1 = fread('Z1_assets.csv')

Z1[,calendaryear := as.integer(substring(Year,1,4))][,Year := NULL]
setnames(Z1,gsub("[^[:alnum:]]","",names(Z1)))
Z1[,desired_real_estate_share := RealEstateHistorical/(RealEstateHistorical+EquipmentHistorical)]
Z1[,Equipmentadjustment := EquipmentCurrent/EquipmentHistorical]
Z1[,IPadjustment := IPCurrent/IPHistorical]
Z1[,Inventoriesadjustment := InventoriesCurrent/InventoriesHistorical]
Z1[,RealEstateadjustment := RealEstateCurrent/RealEstateHistorical]
Z1[,AllAssetsadjustment := AllAssetsCurrent/AllAssetsHistorical]
dtcut[Z1,on='calendaryear',`:=`(desired_real_estate_share=i.desired_real_estate_share,
                                Equipmentadjustment=i.Equipmentadjustment,
                                IPadjustment=i.IPadjustment,
                                Inventoriesadjustment=i.Inventoriesadjustment,
                                RealEstateadjustment=i.RealEstateadjustment,
                                AllAssetsadjustment=i.AllAssetsadjustment)]

na0 = function(x) ifelse(!is.na(x),x,0)
# c2log = function(x) {a = 0.001
# suppressWarnings(ifelse(x>a,log(x),log(a)+(1/a)*(x-a)-(1/(2*a))*(x-a)^2))
# }
# mult_dist = function(x,y) abs(c2log(x)-c2log(y))

dtcut[,financial := +(SIC>=6000&SIC<6500)]
PPEcategories = c('MachineryandEquipment', 'NaturalResources', 'Other', 'Buildings',
                  'ConstructioninProgress', 'LandandImprovements', 'Leases')
depreciationconstants = foreach(i = 1:length(PPEcategories),.combine = rbind)%do%{
  depreciation = dtcut[!is.na(eval(parse(text=paste0('PropertyPlantandEquipment',PPEcategories[i],'Net'))))&
                         !is.na(eval(parse(text=paste0('PropertyPlantandEquipment',PPEcategories[i],'atCost')))),
                       sum(eval(parse(text=paste0('PropertyPlantandEquipment',PPEcategories[i],'Net'))))/
                         sum(eval(parse(text=paste0('PropertyPlantandEquipment',PPEcategories[i],'atCost'))))]
  #this is the important side effect
  dtcut[,PPEcategories[i] := ifelse(!is.na(eval(parse(text=paste0('PropertyPlantandEquipment',PPEcategories[i],'Net')))),
                                  eval(parse(text=paste0('PropertyPlantandEquipment',PPEcategories[i],'Net'))),
                                  depreciation*eval(parse(text=paste0('PropertyPlantandEquipment',PPEcategories[i],'atCost'))))]
  data.table(PPEcategory = PPEcategories[i],depreciationconstant = depreciation)
}
dtcut[,equipment := sum(MachineryandEquipment,
                      NaturalResources,
                      ConstructioninProgress,na.rm = T),by=1:nrow(dtcut)]
dtcut[,realestate := sum(Buildings,
                       LandandImprovements,
                       Leases,na.rm = T),by=1:nrow(dtcut)]
dtcut[,PPEuncategorized := PropertyPlantandEquipmentTotalNet-na0(equipment)-na0(realestate)]
dtcut[is.na(PropertyPlantandEquipmentTotalNet),PPEuncategorized := Other]
dtcut[PPEuncategorized<0,realestate_share := na0(realestate)/(na0(realestate)+na0(equipment))
    ][PPEuncategorized<0,realestate := na0(realestate)+na0(PPEuncategorized*realestate_share)
    ][PPEuncategorized<0,equipment := na0(equipment)+na0(PPEuncategorized*(1-realestate_share))
    ][PPEuncategorized<0,PPEuncategorized := 0
    ]

# dtcut[,equipmentpct := equipment/(equipment+realestate+PPEuncategorized)]
# dtcut[,realestatepct := realestate/(equipment+realestate+PPEuncategorized)]
# dtcut[,mostly_accounted_for := equipmentpct+realestatepct > 0.85]
# dtcut[,calendaryear2 := calendaryear]
# dtcut_mostly_accounted_for = dtcut[mostly_accounted_for==T]
# setkey(dtcut_mostly_accounted_for,GlobalCompanyKey,calendaryear)
# setkey(dtcut,GlobalCompanyKey,calendaryear)
# re_merge = dtcut_mostly_accounted_for[dtcut,,roll='nearest']
# dtcut[,c('mergedre','mergedeq','mergeyear','added')  :=  re_merge[,.(equipmentpct,realestatepct,calendaryear2,calendaryear2!=i.calendaryear2)]]


#can we guess at a firm's equipment to real estate ratio using sectoral data?
setkey(dtcut,calendaryear,twodigitsic)
industry_aggregate_real_estate_share = dtcut[,.(sector_re_share = (sum(realestate,na.rm=T))/(sum(realestate,equipment,na.rm=T))),twodigitsic]
industry_year_PPE_aggregates = dtcut[,.(realestate_categorized = sum(realestate,na.rm=T),
                                        equipment_categorized = sum(equipment,na.rm=T),
                                        uncategorized=sum(PPEuncategorized,na.rm = T),
                                        desired_real_estate_share = median(desired_real_estate_share)),
                                     .(calendaryear,twodigitsic)]
industry_year_PPE_aggregates[industry_aggregate_real_estate_share,on='twodigitsic',sector_re_share := i.sector_re_share
                           ][,total := sum(realestate_categorized,equipment_categorized,uncategorized,na.rm=T),by=1:nrow(industry_year_PPE_aggregates)
                           ][,min_re_share := realestate_categorized/total]
setkey(industry_year_PPE_aggregates,calendaryear,twodigitsic)
fwrite(industry_year_PPE_aggregates[,.(realestate_categorized,uncategorized,total,desired_real_estate_share,sector_re_share,calendaryear,twodigitsic)],
       'real_estate_equipment_to_weight.csv',col.names = T)

shell('real_estate_equipment_weights.py')

optimal_real_estate_weights = fread('Optimal_real_estate_equipment_Weights.csv')
optimal_real_estate_weights[,twodigitsic := as.character(twodigitsic)]
setkey(optimal_real_estate_weights,calendaryear,twodigitsic)
setkey(dtcut,calendaryear,twodigitsic)
dtcut[optimal_real_estate_weights,uncategorized_real_estate_share := i.uncategorized_real_estate_share]

dtcut[,realestate := na0(realestate)+na0(PPEuncategorized*uncategorized_real_estate_share)
      ][,equipment := na0(equipment)+na0(PPEuncategorized*(1-uncategorized_real_estate_share))]


dtcut[IntangibleAssetsTotal==0 & Goodwill>0,`:=`(IntangibleAssetsTotal=Goodwill,AssetsOther=AssetsOther-Goodwill)]
dtcut[,goodwillpct := Goodwill/IntangibleAssetsTotal]
# dtcut[,sum(Goodwill,na.rm = T)/sum(IntangibleAssetsTotal*!is.na(Goodwill),na.rm = T),.(calendaryear,twodigitsic)]
dtcut[,stryear_1988_or_later := factor(pmax(calendaryear,1988))]
dtcut[,twodigitsic_after_1988 := ifelse(twodigitsic=='9','65',ifelse(twodigitsic=='84','79',twodigitsic))]
goodwillmod = lm(goodwillpct~stryear_1988_or_later+twodigitsic_after_1988,data=dtcut)
dtcut[,goodwill_pct_year_sector := pmax(predict(goodwillmod,dtcut),0)]

setkey(dtcut,GlobalCompanyKey,calendaryear)
dtcut[,calendaryear2 := calendaryear]
dtcut_no_NA_goodwillpct = dtcut[!is.na(goodwillpct)]
setkey(dtcut_no_NA_goodwillpct,GlobalCompanyKey,calendaryear)
merged = dtcut_no_NA_goodwillpct[dtcut,,roll='nearest']
merged[abs(calendaryear2-i.calendaryear2)>20,goodwillpct := NA_real_]
dtcut[,goodwillpct_firm := merged[,goodwillpct]]
dtcut[is.na(Goodwill)&!is.na(goodwillpct_firm)&!is.na(IntangibleAssetsTotal),
      `:=`(Goodwill=IntangibleAssetsTotal*goodwillpct_firm,goodwilladded=1)
      ][is.na(Goodwill)&is.na(goodwillpct_firm)&!is.na(IntangibleAssetsTotal),
        `:=`(Goodwill=IntangibleAssetsTotal*goodwill_pct_year_sector,goodwilladded=2)]

dtcut[,IntellectualProperty := IntangibleAssetsTotal-na0(Goodwill)]


# merged[,keyyear := abs(calendaryear2-i.calendaryear2)>0.5&abs(calendaryear2-i.calendaryear2)<1.5]
#decade max?

# merged[keyyear==T,if_missing_goodwill_in_intangibles := goodwillpct*IntangibleAssetsTotal]
# merged[keyyear==T,if_missing_goodwill_in_AssetsOther := goodwillpct*(IntangibleAssetsTotal/(1-goodwillpct))]
# dtcut[,goodwillpct := dtcut_no_NA_goodwillpct[dtcut,goodwillpct,roll='nearest']]
# dtcut[is.na(Goodwill)&!is.na(goodwillpct)&!is.na(IntangibleAssetsTotal)&(AssetsOther>IntangibleAssetsTotal*goodwillpct),
#       Goodwill := IntangibleAssetsTotal*goodwillpct
#       ][is.na(Goodwill)&!is.na(goodwillpct)&!is.na(IntangibleAssetsTotal)&(AssetsOther>IntangibleAssetsTotal*goodwillpct),
#         AssetsOther := AssetsOther-IntangibleAssetsTotal*goodwillpct]

# possiblecases: goodwill NA, and goodwill is in Intangibles, goodwill NA and goodwill in AssetsOther
# goodwill == intangibles and intellectual property is in AssetsOther
# if intangibles are NA: fill them and use goodwill pct, pref at the firm level, but if not, then at the sector/year level (watch out for completely missing sector-years)

# dtcut[,intellectualpropertypct := IntellectualProperty/IntangibleAssetsTotal]
# dtcut_no_NA_intellectualproperty=dtcut[!is.na(IntellectualProperty)&IntellectualProperty!=0]
# setkey(dtcut_no_NA_intellectualproperty,GlobalCompanyKey,calendaryear)
# dtcut[,intellectualpropertypct := dtcut_no_NA_intellectualproperty[dtcut,intellectualpropertypct,roll='nearest']]
# dtcut[is.na(IntellectualProperty)&!is.na(intellectualpropertypct)&!is.na(IntangibleAssetsTotal)&(AssetsOther>IntangibleAssetsTotal*intellectualpropertypct),
#       IntellectualProperty2 := IntangibleAssetsTotal*intellectualpropertypct
#       ][is.na(IntellectualProperty)&!is.na(intellectualpropertypct)&!is.na(IntangibleAssetsTotal)&(AssetsOther>IntangibleAssetsTotal*intellectualpropertypct),
#         AssetsOther := AssetsOther-IntangibleAssetsTotal*intellectualpropertypct]
# 
dtcut[is.na(CashandShortTermInvestments)&(!is.na(Cash)|!is.na(ShortTermInvestmentsTotal)),
      CashandShortTermInvestments := na0(Cash)+na0(ShortTermInvestmentsTotal)]
dtcut[is.na(ReceivablesTotal)&(!is.na(ReceivablesTrade)|!is.na(ReceivablesCurrentOther)),
      ReceivablesTotal := na0(ReceivablesTrade)+na0(ReceivablesCurrentOther)]
dtcut[is.na(InventoriesTotal),Inventories_component_sum := na0(InventoriesRawMaterials)+
        na0(InventoriesOther)+na0(InventoriesWorkInProcess)+na0(InventoriesFinishedGoods)+
        na0(InventoryStockOther)+na0(InventoryStockRealEstateHeldforDevelopment)+na0(InventoryStockRealEstateHeldforSale)+na0(InventoryStockRealEstateUnderDevelopment)]
dtcut[is.na(InventoriesTotal)&Inventories_component_sum!=0,InventoriesTotal := Inventories_component_sum]

dtcut[,CurrentAssets_categorized := na0(InventoriesTotal)+na0(ReceivablesTotal)+
        na0(CashandShortTermInvestments)+na0(CurrentAssetsOtherTotal)]
dtcut[,CurrentAssets_uncategorized := CurrentAssetsTotal-CurrentAssets_categorized]

dtcut[,c('oldInventoriesTotal','oldReceivablesTotal','oldCashandShortTermInvestments','oldCurrentAssetsOtherTotal') := .(InventoriesTotal,ReceivablesTotal,CashandShortTermInvestments,CurrentAssetsOtherTotal)]

dtcut[,current_assets_good := (CurrentAssets_uncategorized/CurrentAssetsTotal)<0.001]
dtcut[,ca_inventories_pct := na0(InventoriesTotal)/CurrentAssets_categorized
    ][,ca_receivables_pct := na0(ReceivablesTotal)/CurrentAssets_categorized
    ][,ca_cash_pct := na0(CashandShortTermInvestments)/CurrentAssets_categorized
    ][,ca_other_pct := na0(CurrentAssetsOtherTotal)/CurrentAssets_categorized]
setkey(dtcut,GlobalCompanyKey,calendaryear)
dtcut_current_assets_are_good = dtcut[current_assets_good==T]
dtcut_current_assets_are_good[,ca_merge_success := 1]
setkey(dtcut_current_assets_are_good,GlobalCompanyKey,calendaryear)
ca_merge = dtcut_current_assets_are_good[dtcut,,roll='nearest']
dtcut[,c('ca_inventories_pct_firm','ca_receivables_pct_firm','ca_cash_pct_firm','ca_other_pct_firm','ca_merge_success') :=
        ca_merge[,.(ca_inventories_pct,ca_receivables_pct,ca_cash_pct,ca_other_pct,ca_merge_success)]]
dtcut[,total_current_assets_year_sector := sum(InventoriesTotal,ReceivablesTotal,CashandShortTermInvestments,CurrentAssetsOtherTotal,na.rm = T),.(calendaryear,twodigitsic)]
stopifnot(nrow(dtcut[total_current_assets_year_sector <= 0]) == 0)
dtcut[,ca_inventories_pct_year_sector := sum(InventoriesTotal,na.rm = T)/total_current_assets_year_sector,.(calendaryear,twodigitsic)
    ][,ca_receivables_pct_year_sector := sum(ReceivablesTotal,na.rm = T)/total_current_assets_year_sector,.(calendaryear,twodigitsic)
    ][,ca_cash_pct_year_sector := sum(CashandShortTermInvestments,na.rm = T)/total_current_assets_year_sector,.(calendaryear,twodigitsic)
    ][,ca_other_pct_year_sector := sum(CurrentAssetsOtherTotal,na.rm = T)/total_current_assets_year_sector,.(calendaryear,twodigitsic)]

dtcut[ca_merge_success==1,c('ca_other_pct','ca_inventories_pct','ca_receivables_pct','ca_cash_pct'):=
        .(ca_other_pct_firm,ca_inventories_pct_firm,ca_receivables_pct_firm,ca_cash_pct_firm)]

dtcut[conm=='U S CHINA MINING GROUP INC'&(calendaryear==2012|calendaryear==2013),c('ca_other_pct','ca_inventories_pct','ca_receivables_pct','ca_cash_pct'):=.(0,0,1,0)]

dtcut[is.na(ca_merge_success),c('ca_other_pct','ca_inventories_pct','ca_receivables_pct','ca_cash_pct') :=
        .(ca_other_pct_year_sector,ca_inventories_pct_year_sector,ca_receivables_pct_year_sector,ca_cash_pct_year_sector)]
dtcut[,CurrentAssetsOtherTotal:=na0(CurrentAssetsOtherTotal)+na0(ca_other_pct*CurrentAssets_uncategorized)
    ][,InventoriesTotal:=na0(InventoriesTotal)+na0(ca_inventories_pct*CurrentAssets_uncategorized)
    ][,ReceivablesTotal:=na0(ReceivablesTotal)+na0(ca_receivables_pct*CurrentAssets_uncategorized)
    ][,CashandShortTermInvestments:=na0(CashandShortTermInvestments)+na0(ca_cash_pct*CurrentAssets_uncategorized)]
# dtcut[,CurrentAssets_categorized_check:=na0(InventoriesTotal)+na0(ReceivablesTotal)+
#         na0(CashandShortTermInvestments)+na0(CurrentAssetsOtherTotal)]
# dtcut[,CurrentAssets_uncategorized_check:=CurrentAssetsTotal-CurrentAssets_categorized_check]


assetclasscols =  c('AssetsOther','DeferredCharges','PrepaidExpenses',
                    'InventoriesTotal','ReceivablesTotal','CashandShortTermInvestments','CurrentAssetsOtherTotal',
                    'realestate','equipment',
                    'InvestmentandAdvancesEquity','InvestmentandAdvancesOther',
                    'IntellectualProperty','Goodwill')
tangible_assetclasscols =  c('AssetsOther','DeferredCharges','PrepaidExpenses',
                             'InventoriesTotal','ReceivablesTotal','CashandShortTermInvestments','CurrentAssetsOtherTotal',
                             'realestate','equipment',
                             'InvestmentandAdvancesEquity','InvestmentandAdvancesOther')


eval(parse(text=paste0('dtcut[,assets_accounted_for:=sum(',paste0(assetclasscols,collapse=','),',na.rm = T),by=1:nrow(dtcut)]')))


dtcut[,residual:=AssetsTotal - assets_accounted_for]

dtcut[,asset_categories_add_up := abs(residual/AssetsTotal) < 0.01]

dtcut[,total_categorized_assets := eval(parse(text=paste0('na0(',assetclasscols,')',collapse = '+')))]
dtcut[,total_categorized_tangible_assets := eval(parse(text=paste0('na0(',tangible_assetclasscols,')',collapse = '+')))]


throwaway = foreach(asset=assetclasscols)%do%{
  dtcut[,paste0('residual_',asset,'_pct_firm'):=eval(parse(text=paste0('na0(',asset,')/total_categorized_assets')))]
  if(asset%in%tangible_assetclasscols) dtcut[,paste0('residual_',asset,'_tangible_pct_firm'):=eval(parse(text=paste0('na0(',asset,')/total_categorized_tangible_assets')))]
  NULL
}

dtcut_asset_categories_add_up = dtcut[asset_categories_add_up==T]
dtcut_asset_categories_add_up[,residual_merge_success:=1]
setkey(dtcut_asset_categories_add_up,GlobalCompanyKey,calendaryear)
residual_merge = dtcut_asset_categories_add_up[dtcut,,roll='nearest']
dtcut[,residual_merge_success := residual_merge[,residual_merge_success]]

dtcut[,total_categorized_assets_year_sector := eval(parse(text=paste0('sum(',paste(assetclasscols,collapse=','),',na.rm = T)'))),.(calendaryear,twodigitsic)]
dtcut[,total_categorized_tangible_assets_year_sector := eval(parse(text=paste0('sum(',paste(tangible_assetclasscols,collapse=','),',na.rm = T)'))),.(calendaryear,twodigitsic)]
stopifnot(nrow(dtcut[total_categorized_assets_year_sector <= 0]) == 0)
stopifnot(nrow(dtcut[total_categorized_tangible_assets_year_sector <= 0]) == 0)

throwaway = foreach(asset=assetclasscols)%do%{
  dtcut[,paste0('residual_',asset,'_pct_year_sector') := eval(parse(text=paste0('sum(',asset,',na.rm = T)/total_categorized_assets_year_sector'))),.(calendaryear,twodigitsic)]
  dtcut[,paste0('residual_',asset,'_pct_firm') := residual_merge[,paste0('residual_',asset,'_pct_firm'), with = F]]
  dtcut[residual_merge_success==1,paste0('residual_',asset,'_pct') := eval(parse(text=paste0('residual_',asset,'_pct_firm')))]
  dtcut[is.na(residual_merge_success),paste0('residual_',asset,'_pct') := eval(parse(text=paste0('residual_',asset,'_pct_year_sector')))]
  
  if(asset%in%tangible_assetclasscols) {
    dtcut[,paste0('residual_',asset,'_tangible_pct_year_sector') := eval(parse(text=paste0('sum(',asset,',na.rm = T)/total_categorized_tangible_assets_year_sector'))),.(calendaryear,twodigitsic)]
    dtcut[,paste0('residual_',asset,'_tangible_pct_firm') := residual_merge[,paste0('residual_',asset,'_tangible_pct_firm'), with = F]]
    dtcut[residual_merge_success==1,paste0('residual_',asset,'_tangible_pct') := eval(parse(text=paste0('residual_',asset,'_tangible_pct_firm')))]
    dtcut[is.na(residual_merge_success),paste0('residual_',asset,'_tangible_pct') := eval(parse(text=paste0('residual_',asset,'_tangible_pct_year_sector')))]
  }
  
  dtcut[intangiblesadded==0,paste0(asset) := eval(parse(text=paste0('na0(',asset,') +',paste0('residual_',asset,'_pct'),'*residual')))]
  if(asset%in%tangible_assetclasscols) dtcut[intangiblesadded==1,paste0(asset) := eval(parse(text=paste0('na0(',asset,') +',paste0('residual_',asset,'_tangible_pct'),'*residual')))]
  NULL
}

eval(parse(text=paste0('dtcut[,pseudoassets_check := sum(',paste0(assetclasscols,collapse=','),',na.rm = T),by=1:nrow(dtcut)]')))

dtcut[,residual_check := AssetsTotal - pseudoassets_check]

sums_by_year = dtcut[,lapply(.SD,sum,na.rm = T),.(financial,calendaryear),.SDcols=
                       c(assetclasscols,'AssetsTotal')]

setkey(sums_by_year,calendaryear,financial)
sums_by_year[Z1,on='calendaryear',`:=`(Equipmentadjustment=i.Equipmentadjustment,
                                       IPadjustment=i.IPadjustment,
                                       Inventoriesadjustment=i.Inventoriesadjustment,
                                       RealEstateadjustment=i.RealEstateadjustment,
                                       AllAssetsadjustment=i.AllAssetsadjustment)]

pctsbyfin = cbind(sums_by_year[,.(financial,calendaryear)],
                  sums_by_year[,assetclasscols,with = F]/sums_by_year$AssetsTotal,
                  sums_by_year[,.(Equipmentadjustment,IPadjustment,Inventoriesadjustment,RealEstateadjustment,AllAssetsadjustment)])

eval(parse(text=paste0('pctsbyfin[,accountedfor2 := sum(',paste0(assetclasscols,collapse=','),',na.rm = T),by=1:nrow(pctsbyfin)]')))

pctsbyfin[,dontrevalue := ReceivablesTotal+DeferredCharges+PrepaidExpenses+
            Goodwill+CashandShortTermInvestments+AssetsOther+
            InvestmentandAdvancesEquity+InvestmentandAdvancesOther]

pctsbyfin[,adjustment_if_categories_matched_up := InventoriesTotal*Inventoriesadjustment+realestate*RealEstateadjustment+equipment*Equipmentadjustment+IntellectualProperty*IPadjustment+(1-IntellectualProperty-equipment-realestate-InventoriesTotal)]

fwrite(pctsbyfin[financial==0,.(InventoriesTotal,realestate,equipment,IntellectualProperty,Inventoriesadjustment,RealEstateadjustment,Equipmentadjustment,IPadjustment,AllAssetsadjustment)],
       'asset_pcts_and_Z1_weights.csv',col.names = F)

shell('OptimalZ1Weights.py')

optimalweights = fread('OptimalZ1Weights.csv')

pctsbyfin = merge(pctsbyfin,optimalweights,by='calendaryear')
pctsbyfin[,optimal_adjustment := InventoriesTotal*reweightedInventoriesadjustment+
            realestate*reweightedRealEstateadjustment+
            equipment*reweightedEquipmentadjustment+
            IntellectualProperty*reweightedIntellectualPropertyadjustment+
            (1-IntellectualProperty-equipment-realestate-InventoriesTotal)]

setkey(optimalweights,calendaryear)
setkey(dtcut,calendaryear)
dtcut = dtcut[optimalweights,`:=`(reweightedInventoriesadjustment = i.reweightedInventoriesadjustment,
                                  reweightedRealEstateadjustment = i.reweightedRealEstateadjustment,
                                  reweightedEquipmentadjustment = i.reweightedEquipmentadjustment,
                                  reweightedIntellectualPropertyadjustment = i.reweightedIntellectualPropertyadjustment)]

dtcut[,InventoriesTotal := InventoriesTotal*reweightedInventoriesadjustment
    ][,realestate := realestate*reweightedRealEstateadjustment
    ][,equipment := equipment*reweightedEquipmentadjustment
    ][,IntellectualProperty := IntellectualProperty*reweightedIntellectualPropertyadjustment
    ][,Goodwill := Goodwill*reweightedIntellectualPropertyadjustment]

eval(parse(text=paste0('dtcut[,AssetsTotal := sum(',paste0(assetclasscols,collapse=','),',na.rm = T),by=1:nrow(dtcut)]')))
#without the adjustments, this is exactly equal to AssetsTotal (the difference is less than 1e-10)

dtcut[,IntangibleAssetsTotal := sum(IntellectualProperty,Goodwill,na.rm = T),by=1:nrow(dtcut)]


dtcut[,liabilitiesadded := +is.na(LiabilitiesTotal)]
dtcut[,liabilityratio := LiabilitiesTotal/AssetsTotal]
dtcut_no_NA_liabilities = dtcut[!is.na(LiabilitiesTotal)]
setkey(dtcut,GlobalCompanyKey,calendaryear)
setkey(dtcut_no_NA_liabilities,GlobalCompanyKey,calendaryear)
dtcut[,liabilityratio := dtcut_no_NA_liabilities[dtcut,liabilityratio,roll='nearest']
    ][is.na(LiabilitiesTotal),LiabilitiesTotal := liabilityratio*AssetsTotal]
liabilitymod = lm(liabilityratio~factor(calendaryear)+twodigitsic,data=dtcut)
dtcut[,predictedliabilityratio := pmax(predict(liabilitymod,dtcut),0)
    ][is.na(LiabilitiesTotal),LiabilitiesTotal := predictedliabilityratio*AssetsTotal]

dtcut[,monopolywealth := MktVal-AssetsTotal+IntangibleAssetsTotal+LiabilitiesTotal
    ][,totalwealth := MktVal+LiabilitiesTotal
    ][,mwtw := monopolywealth/totalwealth
    ][,mwv := monopolywealth/MktVal]


fwrite(dtcut, 'dtcut_for_spreadsheets.csv', quote = T)

implied_r = 0.04
tax_rate = 0.5

companies_for_profits_tax_table = dtcut[grepl(paste0('COCA-COLA|PROCTER|AMERICAN\\ EXPRESS CO|ADOBE|APPLE INC|',
                   'MICROSOFT|SALESFORCE|GENERAL MOTORS CO|AMERICAN ELECTRIC POWER|DOW INC|',
                   'FORD MOTOR CO|CHEVRON CORP|JOHNSON & JOHNSON|INTL BUSINESS MACHINES CORP|^GENERAL ELECTRIC CO'),
            conm, ignore.case = T) & calendaryear == 2019,
      .(conm, DataYearFiscal, consol, GlobalCompanyKey, SalesTurnoverNet, SellingGeneralandAdministrativeExpense,
        CostofGoodsSold, LiabilitiesTotal, AssetsTotal, IntangibleAssetsTotal, MktVal,
        OperatingIncomeBeforeDepreciation, DepreciationandAmortization, CapitalExpenditures,
        ResearchandDevelopmentExpense, Employees, PretaxIncome, PretaxIncomeDomestic, PretaxIncomeForeign,
        InterestandRelatedExpenseTotal, IncomeTaxesFederal, IncomeTaxesForeign, IncomeTaxesTotal,
        EarningsBeforeInterest, EarningsBeforeInterestandTaxes)
      ][, profits := PretaxIncome - 
          implied_r * (AssetsTotal - IntangibleAssetsTotal) + implied_r * LiabilitiesTotal
         ][, profits_3_pct_r := PretaxIncome - 
             0.03 * (AssetsTotal - IntangibleAssetsTotal) + 0.03 * LiabilitiesTotal
           ][is.na(PretaxIncomeForeign), PretaxIncomeForeign := PretaxIncome - PretaxIncomeDomestic
         ][, .(`Company Name` = conm, `Revenue` = round(SalesTurnoverNet),
              `Accounting Profits` = round(PretaxIncome), `Profits due to Market Power, 3% Interest Rate` = round(profits_3_pct_r),
              `Profits due to Market Power, 4% Interest Rate` = round(profits), `Profits Tax Liability` = round(pmax(tax_rate * profits, 0)),
              Assets = AssetsTotal, Intangibles = IntangibleAssetsTotal, Liabilities = LiabilitiesTotal,
              `R&D Expenses` = ResearchandDevelopmentExpense, `Reported U.S. Accounting Profits` = PretaxIncomeDomestic, `Reported non-U.S. Accounting Profits` = PretaxIncomeForeign,
              `Current U.S. Corporate Tax Liability` = IncomeTaxesFederal, `Foreign Corporate Tax Liability` = IncomeTaxesForeign)
         ]

write.xlsx(setorder(companies_for_profits_tax_table, -`Profits Tax`)[], 'profits_tax_estimates.xlsx')


dtcut[grepl(paste0('COCA-COLA|PROCTER|AMERICAN\\ EXPRESS CO|ADOBE|APPLE INC|',
                   'MICROSOFT|SALESFORCE|GENERAL MOTORS CO|AMERICAN ELECTRIC POWER|DOW INC|',
                   'FORD MOTOR CO|CHEVRON CORP|JOHNSON & JOHNSON|INTL BUSINESS MACHINES CORP|^GENERAL ELECTRIC CO'),
            conm, ignore.case = T) & calendaryear == 2019,
      .(conm, DataYearFiscal, consol, GlobalCompanyKey, SalesTurnoverNet, SellingGeneralandAdministrativeExpense,
        CostofGoodsSold, LiabilitiesTotal, AssetsTotal, IntangibleAssetsTotal, MktVal,
        OperatingIncomeBeforeDepreciation, DepreciationandAmortization, CapitalExpenditures,
        ResearchandDevelopmentExpense, Employees, PretaxIncome, PretaxIncomeDomestic, PretaxIncomeForeign,
        InterestandRelatedExpenseTotal, IncomeTaxesFederal, IncomeTaxesForeign, IncomeTaxesTotal, IncomeTaxesOther,
        EarningsBeforeInterest, EarningsBeforeInterestandTaxes)
      ][, .(conm, IncomeTaxesFederal, IncomeTaxesForeign, IncomeTaxesOther, IncomeTaxesTotal)]