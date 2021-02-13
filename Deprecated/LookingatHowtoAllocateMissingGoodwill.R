
# wut =dtcut[,lapply(.SD,sum,na.rm=T),.(calendaryear,financial),
#            .SDcols = c('PPEuncategorized','Buildings','ConstructioninProgress','LandandImprovements','Leases','MachineryandEquipment','NaturalResources','Other','PropertyPlantandEquipmentTotalNet')]
# setkey(wut,calendaryear,financial)
# 
# asdf = setkey(firms_with_asset_breakdowns[,.(round(sum(residual),3),round(sum(residual)/sum(AssetsTotal),3)),.(calendaryear,financial)],calendaryear,financial)

# dukeen = merged[,.(keyyear,conm,IntangibleAssetsTotal,AssetsTotal,AssetsOther,Goodwill,calendaryear2,goodwillpct,i.conm,i.calendaryear2,i.IntangibleAssetsTotal,i.AssetsTotal,i.AssetsOther,i.Goodwill,i.goodwillpct)]
# dukeen[,c('goodwill0_ip','goodwill0_gw','goodwill0_ao'):=.(i.IntangibleAssetsTotal-na0(i.Goodwill),na0(i.Goodwill),i.AssetsOther)]
# dukeen[,goodwillao_ip:=i.IntangibleAssetsTotal-na0(i.Goodwill)
#       ][,goodwillao_gw:=goodwillao_ip*goodwillpct/(1-goodwillpct)
#       ][,goodwillao_ao:=i.AssetsOther+na0(i.Goodwill)-goodwillao_gw]
# dukeen[,c('goodwillintan_ip','goodwillintan_gw','goodwillintan_ao'):=.(i.IntangibleAssetsTotal*(1-goodwillpct),i.IntangibleAssetsTotal*goodwillpct,i.AssetsOther)]
# dukeen[keyyear==T,ao_share_with_goodwill:=AssetsOther/AssetsTotal]
# dukeen[keyyear==T,intan_share_with_goodwill:=IntangibleAssetsTotal/AssetsTotal]
# dukeen[keyyear==T,gw_share_with_goodwill:=Goodwill/AssetsTotal]
# dukeen[keyyear==T,ip_share_with_goodwill:=(IntangibleAssetsTotal-Goodwill)/AssetsTotal]
# 
# dukeen[keyyear==T,goodwill0_ao_share:=goodwill0_ao/AssetsTotal]
# dukeen[keyyear==T,goodwill0_intan_share:=(goodwill0_gw+goodwill0_ip)/AssetsTotal]
# dukeen[keyyear==T,goodwill0_gw_share:=goodwill0_gw/AssetsTotal]
# dukeen[keyyear==T,goodwill0_ip_share:=goodwill0_ip/AssetsTotal]
# 
# dukeen[keyyear==T,goodwillao_ao_share:=goodwillao_ao/AssetsTotal]
# dukeen[keyyear==T,goodwillao_intan_share:=(goodwillao_gw+goodwillao_ip)/AssetsTotal]
# dukeen[keyyear==T,goodwillao_gw_share:=goodwillao_gw/AssetsTotal]
# dukeen[keyyear==T,goodwillao_ip_share:=goodwillao_ip/AssetsTotal]
# 
# dukeen[keyyear==T,goodwillintan_ao_share:=goodwillintan_ao/AssetsTotal]
# dukeen[keyyear==T,goodwillintan_intan_share:=(goodwillintan_gw+goodwillintan_ip)/AssetsTotal]
# dukeen[keyyear==T,goodwillintan_gw_share:=goodwillintan_gw/AssetsTotal]
# dukeen[keyyear==T,goodwillintan_ip_share:=goodwillintan_ip/AssetsTotal]
# 
# dukeen[,goodwill0_implausibility:=mult_dist(goodwill0_ao_share,ao_share_with_goodwill)+
#          mult_dist(goodwill0_intan_share,intan_share_with_goodwill)+
#          mult_dist(goodwill0_gw_share,gw_share_with_goodwill)+
#          mult_dist(goodwill0_ip_share,ip_share_with_goodwill)]
# dukeen[,goodwillao_implausibility:=mult_dist(goodwillao_ao_share,ao_share_with_goodwill)+
#          mult_dist(goodwillao_intan_share,intan_share_with_goodwill)+
#          mult_dist(goodwillao_gw_share,gw_share_with_goodwill)+
#          mult_dist(goodwillao_ip_share,ip_share_with_goodwill)]
# dukeen[,goodwillintan_implausibility:=mult_dist(goodwillintan_ao_share,ao_share_with_goodwill)+
#          mult_dist(goodwillintan_intan_share,intan_share_with_goodwill)+
#          mult_dist(goodwillintan_gw_share,gw_share_with_goodwill)+
#          mult_dist(goodwillintan_ip_share,ip_share_with_goodwill)]
# dukeen[,most_plausible := (goodwillao_implausibility==pmin(goodwill0_implausibility,goodwillao_implausibility,goodwillintan_implausibility))+
#          2*(goodwillintan_implausibility==pmin(goodwill0_implausibility,goodwillao_implausibility,goodwillintan_implausibility))]