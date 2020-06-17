# run real_estate_equipment_rates.py

library(foreach)
library(iterators)
library(snow)
library(doSNOW)

optimal_real_estate_weights = fread('Compustat/Optimal_real_estate_equipment_Weights.csv')
optimal_real_estate_weights[,twodigitsic:=as.character(twodigitsic)]
setkey(optimal_real_estate_weights,calendaryear,twodigitsic)
setkey(dtcut,calendaryear,twodigitsic)
dtcut[optimal_real_estate_weights,uncategorized_real_estate_share:=i.uncategorized_real_estate_share]

dtcut[,realestate:=na0(realestate)+na0(PPEuncategorized*uncategorized_real_estate_share)
    ][,equipment:=na0(equipment)+na0(PPEuncategorized*(1-uncategorized_real_estate_share))]


dtcut[IntangibleAssetsTotal==0 & Goodwill>0,`:=`(IntangibleAssetsTotal=Goodwill,AssetsOther=AssetsOther-Goodwill)]
dtcut[,goodwillpct:=Goodwill/IntangibleAssetsTotal]
# dtcut[,sum(Goodwill,na.rm = T)/sum(IntangibleAssetsTotal*!is.na(Goodwill),na.rm = T),.(calendaryear,twodigitsic)]
dtcut[,stryear_1988_or_later := factor(pmax(calendaryear,1988))]
dtcut[,twodigitsic_after_1988 := ifelse(twodigitsic=='9','65',ifelse(twodigitsic=='84','79',twodigitsic))]
goodwillmod = lm(goodwillpct~stryear_1988_or_later+twodigitsic_after_1988,data=dtcut)
dtcut[,goodwill_pct_year_sector := pmax(predict(goodwillmod,dtcut),0)]

setkey(dtcut,GlobalCompanyKey,calendaryear)
dtcut[,calendaryear2:=calendaryear]
dtcut_no_NA_goodwillpct = dtcut[!is.na(goodwillpct)]
setkey(dtcut_no_NA_goodwillpct,GlobalCompanyKey,calendaryear)
merged = dtcut_no_NA_goodwillpct[dtcut,,roll='nearest']
merged[abs(calendaryear2-i.calendaryear2)>20,goodwillpct:=NA_real_]
dtcut[,goodwillpct_firm:=merged[,goodwillpct]]
dtcut[is.na(Goodwill)&!is.na(goodwillpct_firm)&!is.na(IntangibleAssetsTotal),
      `:=`(Goodwill=IntangibleAssetsTotal*goodwillpct_firm,goodwilladded=1)
      ][is.na(Goodwill)&is.na(goodwillpct_firm)&!is.na(IntangibleAssetsTotal),
        `:=`(Goodwill=IntangibleAssetsTotal*goodwill_pct_year_sector,goodwilladded=2)]

dtcut[,IntellectualProperty:=IntangibleAssetsTotal-na0(Goodwill)]


# merged[,keyyear:=abs(calendaryear2-i.calendaryear2)>0.5&abs(calendaryear2-i.calendaryear2)<1.5]
#decade max?

# merged[keyyear==T,if_missing_goodwill_in_intangibles:=goodwillpct*IntangibleAssetsTotal]
# merged[keyyear==T,if_missing_goodwill_in_AssetsOther:=goodwillpct*(IntangibleAssetsTotal/(1-goodwillpct))]
# dtcut[,goodwillpct:=dtcut_no_NA_goodwillpct[dtcut,goodwillpct,roll='nearest']]
# dtcut[is.na(Goodwill)&!is.na(goodwillpct)&!is.na(IntangibleAssetsTotal)&(AssetsOther>IntangibleAssetsTotal*goodwillpct),
#       Goodwill:=IntangibleAssetsTotal*goodwillpct
#       ][is.na(Goodwill)&!is.na(goodwillpct)&!is.na(IntangibleAssetsTotal)&(AssetsOther>IntangibleAssetsTotal*goodwillpct),
#         AssetsOther:=AssetsOther-IntangibleAssetsTotal*goodwillpct]

# possiblecases: goodwill NA, and goodwill is in Intangibles, goodwill NA and goodwill in AssetsOther
# goodwill == intangibles and intellectual property is in AssetsOther
# if intangibles are NA: fill them and use goodwill pct, pref at the firm level, but if not, then at the sector/year level (watch out for completely missing sector-years)

# dtcut[,intellectualpropertypct:=IntellectualProperty/IntangibleAssetsTotal]
# dtcut_no_NA_intellectualproperty=dtcut[!is.na(IntellectualProperty)&IntellectualProperty!=0]
# setkey(dtcut_no_NA_intellectualproperty,GlobalCompanyKey,calendaryear)
# dtcut[,intellectualpropertypct:=dtcut_no_NA_intellectualproperty[dtcut,intellectualpropertypct,roll='nearest']]
# dtcut[is.na(IntellectualProperty)&!is.na(intellectualpropertypct)&!is.na(IntangibleAssetsTotal)&(AssetsOther>IntangibleAssetsTotal*intellectualpropertypct),
#       IntellectualProperty2:=IntangibleAssetsTotal*intellectualpropertypct
#       ][is.na(IntellectualProperty)&!is.na(intellectualpropertypct)&!is.na(IntangibleAssetsTotal)&(AssetsOther>IntangibleAssetsTotal*intellectualpropertypct),
#         AssetsOther:=AssetsOther-IntangibleAssetsTotal*intellectualpropertypct]
# 
dtcut[is.na(CashandShortTermInvestments)&(!is.na(Cash)|!is.na(ShortTermInvestmentsTotal)),
      CashandShortTermInvestments:=na0(Cash)+na0(ShortTermInvestmentsTotal)]
dtcut[is.na(ReceivablesTotal)&(!is.na(ReceivablesTrade)|!is.na(ReceivablesCurrentOther)),
      ReceivablesTotal:=na0(ReceivablesTrade)+na0(ReceivablesCurrentOther)]
dtcut[is.na(InventoriesTotal),Inventories_component_sum :=na0(InventoriesRawMaterials)+
        na0(InventoriesOther)+na0(InventoriesWorkInProcess)+na0(InventoriesFinishedGoods)+
        na0(InventoryStockOther)+na0(InventoryStockRealEstateHeldforDevelopment)+na0(InventoryStockRealEstateHeldforSale)+na0(InventoryStockRealEstateUnderDevelopment)]
dtcut[is.na(InventoriesTotal)&Inventories_component_sum!=0,InventoriesTotal:=Inventories_component_sum]

dtcut[,CurrentAssets_categorized:=na0(InventoriesTotal)+na0(ReceivablesTotal)+
        na0(CashandShortTermInvestments)+na0(CurrentAssetsOtherTotal)]
dtcut[,CurrentAssets_uncategorized:=CurrentAssetsTotal-CurrentAssets_categorized]

dtcut[,c('oldInventoriesTotal','oldReceivablesTotal','oldCashandShortTermInvestments','oldCurrentAssetsOtherTotal'):=.(InventoriesTotal,ReceivablesTotal,CashandShortTermInvestments,CurrentAssetsOtherTotal)]

dtcut[,current_assets_good:=(CurrentAssets_uncategorized/CurrentAssetsTotal)<0.001]
dtcut[,ca_inventories_pct:=na0(InventoriesTotal)/CurrentAssets_categorized
      ][,ca_receivables_pct:=na0(ReceivablesTotal)/CurrentAssets_categorized
        ][,ca_cash_pct:=na0(CashandShortTermInvestments)/CurrentAssets_categorized
          ][,ca_other_pct:=na0(CurrentAssetsOtherTotal)/CurrentAssets_categorized]
setkey(dtcut,GlobalCompanyKey,calendaryear)
dtcut_current_assets_are_good = dtcut[current_assets_good==T]
dtcut_current_assets_are_good[,ca_merge_success:=1]
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
  dtcut[is.na(residual_merge_success),paste0('residual_',asset,'_pct'):= eval(parse(text=paste0('residual_',asset,'_pct_year_sector')))]
  
  if(asset%in%tangible_assetclasscols) {
    dtcut[,paste0('residual_',asset,'_tangible_pct_year_sector') := eval(parse(text=paste0('sum(',asset,',na.rm = T)/total_categorized_tangible_assets_year_sector'))),.(calendaryear,twodigitsic)]
    dtcut[,paste0('residual_',asset,'_tangible_pct_firm') := residual_merge[,paste0('residual_',asset,'_tangible_pct_firm'), with = F]]
    dtcut[residual_merge_success==1,paste0('residual_',asset,'_tangible_pct') := eval(parse(text=paste0('residual_',asset,'_tangible_pct_firm')))]
    dtcut[is.na(residual_merge_success),paste0('residual_',asset,'_tangible_pct'):= eval(parse(text=paste0('residual_',asset,'_tangible_pct_year_sector')))]
  }
  
  dtcut[intangiblesadded==0,paste0(asset) := eval(parse(text=paste0('na0(',asset,') +',paste0('residual_',asset,'_pct'),'*residual')))]
  if(asset%in%tangible_assetclasscols) dtcut[intangiblesadded==1,paste0(asset) := eval(parse(text=paste0('na0(',asset,') +',paste0('residual_',asset,'_tangible_pct'),'*residual')))]
  NULL
}

eval(parse(text=paste0('dtcut[,pseudoassets_check:=sum(',paste0(assetclasscols,collapse=','),',na.rm = T),by=1:nrow(dtcut)]')))

dtcut[,residual_check:=AssetsTotal - pseudoassets_check]

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

eval(parse(text=paste0('pctsbyfin[,accountedfor2:=sum(',paste0(assetclasscols,collapse=','),',na.rm = T),by=1:nrow(pctsbyfin)]')))

pctsbyfin[,dontrevalue:=ReceivablesTotal+DeferredCharges+PrepaidExpenses+
            Goodwill+CashandShortTermInvestments+AssetsOther+
            InvestmentandAdvancesEquity+InvestmentandAdvancesOther]

pctsbyfin[,adjustment_if_categories_matched_up:=InventoriesTotal*Inventoriesadjustment+realestate*RealEstateadjustment+equipment*Equipmentadjustment+IntellectualProperty*IPadjustment+(1-IntellectualProperty-equipment-realestate-InventoriesTotal)]

fwrite(pctsbyfin[financial==0,.(InventoriesTotal,realestate,equipment,IntellectualProperty,Inventoriesadjustment,RealEstateadjustment,Equipmentadjustment,IPadjustment,AllAssetsadjustment)],
       'Compustat/asset_pcts_and_Z1_weights.csv',col.names = F)

