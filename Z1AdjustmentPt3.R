optimalweights = fread('Compustat/OptimalZ1Weights.csv')

pctsbyfin = merge(pctsbyfin,optimalweights,by='calendaryear')
pctsbyfin[,optimal_adjustment:=InventoriesTotal*reweightedInventoriesadjustment+
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

dtcut[,InventoriesTotal:=InventoriesTotal*reweightedInventoriesadjustment
    ][,realestate:=realestate*reweightedRealEstateadjustment
    ][,equipment:=equipment*reweightedEquipmentadjustment
    ][,IntellectualProperty:=IntellectualProperty*reweightedIntellectualPropertyadjustment
    ][,Goodwill:=Goodwill*reweightedIntellectualPropertyadjustment]

eval(parse(text=paste0('dtcut[,AssetsTotal:=sum(',paste0(assetclasscols,collapse=','),',na.rm = T),by=1:nrow(dtcut)]')))
#without the adjustments, this is exactly equal to AssetsTotal (the difference is less than 1e-10)

dtcut[,IntangibleAssetsTotal:=sum(IntellectualProperty,Goodwill,na.rm = T),by=1:nrow(dtcut)]


dtcut[,liabilitiesadded:= +is.na(LiabilitiesTotal)]
dtcut[,liabilityratio:=LiabilitiesTotal/AssetsTotal]
dtcut_no_NA_liabilities = dtcut[!is.na(LiabilitiesTotal)]
setkey(dtcut,GlobalCompanyKey,calendaryear)
setkey(dtcut_no_NA_liabilities,GlobalCompanyKey,calendaryear)
dtcut[,liabilityratio:=dtcut_no_NA_liabilities[dtcut,liabilityratio,roll='nearest']
    ][is.na(LiabilitiesTotal),LiabilitiesTotal:=liabilityratio*AssetsTotal]
liabilitymod = lm(liabilityratio~factor(calendaryear)+twodigitsic,data=dtcut)
dtcut[,predictedliabilityratio:=pmax(predict(liabilitymod,dtcut),0)
    ][is.na(LiabilitiesTotal),LiabilitiesTotal:=predictedliabilityratio*AssetsTotal]

dtcut[,monopolywealth:=MktVal-AssetsTotal+IntangibleAssetsTotal+LiabilitiesTotal
    ][,totalwealth:=MktVal+LiabilitiesTotal
    ][,mwtw:=monopolywealth/totalwealth
    ][,mwv:=monopolywealth/MktVal]


saveRDS(dtcut,'Compustat/dtcut_for_spreadsheets.rds')

