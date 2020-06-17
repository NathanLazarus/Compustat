setwd("C:/Users/Nathan/Downloads")
library(foreach)
library(iterators)
library(snow)
library(doSNOW)

dtcut = readRDS('Compustat/dtcut.rds')
Z1 = fread('Z1_assets.csv')

Z1[,calendaryear:=as.integer(substring(Year,1,4))][,Year:=NULL]
setnames(Z1,gsub("[^[:alnum:]]","",names(Z1)))
Z1[,desired_real_estate_share:=RealEstateHistorical/(RealEstateHistorical+EquipmentHistorical)]
Z1[,Equipmentadjustment:=EquipmentCurrent/EquipmentHistorical]
Z1[,IPadjustment:=IPCurrent/IPHistorical]
Z1[,Inventoriesadjustment:=InventoriesCurrent/InventoriesHistorical]
Z1[,RealEstateadjustment:=RealEstateCurrent/RealEstateHistorical]
Z1[,AllAssetsadjustment:=AllAssetsCurrent/AllAssetsHistorical]
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

dtcut[,financial:=+(SIC>=6000&SIC<6500)]
PPEcategories = c('MachineryandEquipment', 'NaturalResources', 'Other', 'Buildings',
                  'ConstructioninProgress', 'LandandImprovements', 'Leases')
throwaway = foreach(i = 1:length(PPEcategories))%do%{
  depreciation = dtcut[!is.na(eval(parse(text=paste0('PropertyPlantandEquipment',PPEcategories[i],'Net'))))&
                         !is.na(eval(parse(text=paste0('PropertyPlantandEquipment',PPEcategories[i],'atCost')))),
                       sum(eval(parse(text=paste0('PropertyPlantandEquipment',PPEcategories[i],'Net'))))/
                         sum(eval(parse(text=paste0('PropertyPlantandEquipment',PPEcategories[i],'atCost'))))]
  dtcut[,PPEcategories[i]:=ifelse(!is.na(eval(parse(text=paste0('PropertyPlantandEquipment',PPEcategories[i],'Net')))),
                                  eval(parse(text=paste0('PropertyPlantandEquipment',PPEcategories[i],'Net'))),
                                  depreciation*eval(parse(text=paste0('PropertyPlantandEquipment',PPEcategories[i],'atCost'))))]
  NULL
}
dtcut[,equipment:=sum(MachineryandEquipment,
                      NaturalResources,
                      ConstructioninProgress,na.rm = T),by=1:nrow(dtcut)]
dtcut[,realestate:=sum(Buildings,
                       LandandImprovements,
                       Leases,na.rm = T),by=1:nrow(dtcut)]
dtcut[,PPEuncategorized:=PropertyPlantandEquipmentTotalNet-na0(equipment)-na0(realestate)]
dtcut[is.na(PropertyPlantandEquipmentTotalNet),PPEuncategorized:=Other]
dtcut[PPEuncategorized<0,realestate_share:=na0(realestate)/(na0(realestate)+na0(equipment))
    ][PPEuncategorized<0,realestate:=na0(realestate)+na0(PPEuncategorized*realestate_share)
    ][PPEuncategorized<0,equipment:=na0(equipment)+na0(PPEuncategorized*(1-realestate_share))
    ][PPEuncategorized<0,PPEuncategorized:=0
    ]

# dtcut[,equipmentpct:=equipment/(equipment+realestate+PPEuncategorized)]
# dtcut[,realestatepct:=realestate/(equipment+realestate+PPEuncategorized)]
# dtcut[,mostly_accounted_for:=equipmentpct+realestatepct > 0.85]
# dtcut[,calendaryear2:=calendaryear]
# dtcut_mostly_accounted_for = dtcut[mostly_accounted_for==T]
# setkey(dtcut_mostly_accounted_for,GlobalCompanyKey,calendaryear)
# setkey(dtcut,GlobalCompanyKey,calendaryear)
# re_merge = dtcut_mostly_accounted_for[dtcut,,roll='nearest']
# dtcut[,c('mergedre','mergedeq','mergeyear','added') := re_merge[,.(equipmentpct,realestatepct,calendaryear2,calendaryear2!=i.calendaryear2)]]


#can we guess at a firm's equipment to real estate ratio using sectoral data?
setkey(dtcut,calendaryear,twodigitsic)
industry_aggregate_real_estate_share = dtcut[,.(sector_re_share = (sum(realestate,na.rm=T))/(sum(realestate,equipment,na.rm=T))),twodigitsic]
industry_year_PPE_aggregates = dtcut[,.(realestate_categorized = sum(realestate,na.rm=T),
                                        equipment_categorized = sum(equipment,na.rm=T),
                                        uncategorized=sum(PPEuncategorized,na.rm = T),
                                        desired_real_estate_share = median(desired_real_estate_share)),
                                     .(calendaryear,twodigitsic)]
industry_year_PPE_aggregates[industry_aggregate_real_estate_share,on='twodigitsic',sector_re_share:=i.sector_re_share
                           ][,total:=sum(realestate_categorized,equipment_categorized,uncategorized,na.rm=T),by=1:nrow(industry_year_PPE_aggregates)
                           ][,min_re_share:=realestate_categorized/total]
setkey(industry_year_PPE_aggregates,calendaryear,twodigitsic)
fwrite(industry_year_PPE_aggregates[,.(realestate_categorized,uncategorized,total,desired_real_estate_share,sector_re_share,calendaryear,twodigitsic)],
       'Compustat/real_estate_equipment_to_weight.csv',col.names = T)

