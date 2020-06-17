library(openxlsx)

mydata = dt[datadate<as.Date('2016-01-01')
            &indfmt=='INDL'
            &consol=='C'
            &datafmt=='STD']
mycut = dtcut[datadate<as.Date('2016-01-01')]
hisdata = mordecaisrawdata
hiscut = mordecaisdata

mydata[,GlobalCompanyKey:=as.numeric(GlobalCompanyKey)]
mycut[,GlobalCompanyKey:=as.numeric(GlobalCompanyKey)]
hisdata[,GlobalCompanyKey:=`Global Company Key`]
hiscut[,GlobalCompanyKey:=`Global Company Key`]
hisdata[,SIC:=`Standard Industry Classification Code`]
hiscut[,SIC:=`Standard Industry Classification Code`]
hisdata[,loc:=`Current ISO Country Code - Headquarters`]
hiscut[,loc:=`Current ISO Country Code - Headquarters`]
hisdata[,MktVal:=`Market Value - Total - Fiscal`]
hiscut[,MktVal:=`Market Value - Total - Fiscal`]
hisdata[,DataYearFiscal:=`Data Year - Fiscal`]
hiscut[,DataYearFiscal:=`Data Year - Fiscal`]
hisdata[,PriceCloseAnnualCalendar:=`Price Close - Annual - Calendar`]
hiscut[,PriceCloseAnnualCalendar:=`Price Close - Annual - Calendar`]
hisdata[,PriceCloseAnnualFiscal:=`Price Close - Annual - Fiscal`]
hiscut[,PriceCloseAnnualFiscal:=`Price Close - Annual - Fiscal`]
hisdata[,CommonSharesOutstanding:=`Common Shares Outstanding`]
hiscut[,CommonSharesOutstanding:=`Common Shares Outstanding`]
hisdata[,AssetsTotal:=`Assets - Total`]
hiscut[,AssetsTotal:=`Assets - Total`]
hisdata[,exists:=1]

# mycut[hisdata,on='GlobalCompanyKey',`:=`(hisSIC=i.SIC,hisloc=i.loc)]
# hiscut[mydata,on='GlobalCompanyKey',`:=`(mySIC=i.SIC,mycurrentSIC=i.currentsic,myloc=i.loc)]

combo = merge(mycut[,.(GlobalCompanyKey,DataYearFiscal)],hiscut[,.(GlobalCompanyKey,DataYearFiscal)],all=T)
combo = unique(combo)
combo[mydata,on=c('GlobalCompanyKey','DataYearFiscal'),`:=`(mySIC=i.SIC,mycurrentSIC=i.currentsic,mySICH=i.StandardIndustrialClassificationHistorical,
                                                            myloc=i.loc,conm=i.conm,datadate=i.datadate,calendaryear=i.calendaryear,myrawmktval=i.MarketValueTotalFiscal,
                                                            mypriceannual=i.PriceCloseAnnualCalendar,mypricefiscal=PriceCloseAnnualFiscal,mycommonshares=i.CommonSharesOutstanding,
                                                            myassets=i.AssetsTotal)]
combo[mycut,on=c('GlobalCompanyKey','DataYearFiscal'),`:=`(myMktVal=i.MktVal)]
combo[hisdata,on='GlobalCompanyKey',`:=`(hisSIC=i.SIC,hisloc=i.loc)]
combo[hisdata,on=c('GlobalCompanyKey','DataYearFiscal'),`:=`(existsinhisdata=i.exists,hisrawmktval=i.MktVal,
                                                             hispriceannual=i.PriceCloseAnnualCalendar,hispricefiscal=PriceCloseAnnualFiscal,hiscommonshares=i.CommonSharesOutstanding,
                                                             hisassets=i.AssetsTotal)]
combo[hiscut,on=c('GlobalCompanyKey','DataYearFiscal'),`:=`(hisMktVal=i.MktVal)]
vars = c('mySIC','mycurrentSIC','mySICH','hisSIC')
combo[,eval(parse(text=paste0('`:=`(',paste0(vars,'b=+(',vars,'<6000|',vars,'>6499)',collapse=','),')')))]
#different because of the addition of historical sic
# combo[,historical:=+((hisSICb==mycurrentSICb)&(mySICb!=hisSICb))]
combo[,historical:=+((hisSICb!=mySICb)&(!is.na(mySICHb)))]
#different current sic
combo[,recentindustrychange:=+((hisSICb!=mycurrentSICb)&(is.na(mySICHb)))]
#updated loc
combo[,locdiff:=+(myloc!=hisloc)]
combo[,mistakeninclusion:=+(hisloc!='USA')&!is.na(hisMktVal)]
#new filings
combo[,newfilings:=+is.na(existsinhisdata)]
#price disappeared
combo[,pricedisappeared:=is.na(mypriceannual)&is.na(mypricefiscal)&(!is.na(hispriceannual)|!is.na(hispricefiscal))]
combo[,priceappeared:=is.na(hispriceannual)&is.na(hispricefiscal)&(!is.na(mypriceannual)|!is.na(mypricefiscal))]
combo[,mktvalappeared:=is.na(hisrawmktval)&!is.na(myrawmktval)]

# combo[,sharesdisappeared:=is.na(mycommonshares)&!is.na(hiscommonshares)]
# combo[,sharesappeared:=is.na(hiscommonshares)&!is.na(mycommonshares)]
# combo[,mktvaldisappeared:=is.na(myrawmktval)&!is.na(hisrawmktval)]

combo[,icancalculatemktval:=!is.na(myrawmktval)|(!is.na(mycommonshares)&(!is.na(mypriceannual)|!is.na(mypricefiscal)))]
combo[,hecancalculatemktval:=!is.na(hisrawmktval)|(!is.na(hiscommonshares)&(!is.na(hispriceannual)|!is.na(hispricefiscal)))]
combo[,hehassomefirmswith0assets:=myassets==0&hisassets==0]
combo[,ihavepricetohigherprecision:=(mypricefiscal<0.01|is.na(mypricefiscal)&mypriceannual<0.01)
      &(hispricefiscal==0|(is.na(hispricefiscal)&hispriceannual==0))
      &is.na(hisMktVal)]
combo[,theresanerrorinCommonSharesOutstandinghere:=conm=='DELHAIZE AMERICA INC'&calendaryear==2001]
#different market values
combo[,mktvaldiff:=myMktVal-hisMktVal]
combo[,theCompustatmarketvaluedatachanged:=mktvaldiff>1&!is.na(mktvaldiff)]

discrepancies = combo[historical==1|recentindustrychange==1|locdiff==1
                     |mistakeninclusion==1|newfilings==1|pricedisappeared
                     |priceappeared|mktvalappeared|hehassomefirmswith0assets
                     |ihavepricetohigherprecision|theresanerrorinCommonSharesOutstandinghere
                     |theCompustatmarketvaluedatachanged]
discrepancies[historical==1,message:='Explained by using historical instead of present SIC codes'
              ][recentindustrychange==1,message:='No historical SIC code, but the present SIC code changed'
              ][mistakeninclusion==1,message:='Your data includes some firms that you have listed as located outside the US in the years 1950, 1951, 2006, and 2007'
              ][pricedisappeared==T,message:='Your data includes prices for 10 firm-years that are no longer in the database'
              ][priceappeared==T,message:='Compustat has since added prices for this firm to the database'
              ][mktvalappeared==T,message:='Compustat has since added market values for this firm to the database'
              ][hehassomefirmswith0assets==T,message:='You included 7 firms with 0 assets, which you told me to exclude'
              ][ihavepricetohigherprecision==T,message:='You excluded these firms because they have a price of 0 in your data, but I have their prices as being positive but less than 0.01'
              ][theresanerrorinCommonSharesOutstandinghere==T,message:='The data for Delhaize in 2001 suggests it has a market value of >$1 trillion, thanks to a huge rise in shares outstanding. You excluded it entirely, while I used the previous years figure for common shares.'
              ][theCompustatmarketvaluedatachanged==T,message:='The underlying price or market value data in Compustat was changed.'
              ][newfilings==1,message:='Your data is missing some firms in recent years that were delayed filing'
              ][locdiff==1,message:='The location of the firm changed in the last four years (only present firm location data is available)'
              ] #many firms are priceappeared and newfilings, and one firm is locdiff and priceappeared
setkey(discrepancies,message,conm,calendaryear)
discrepanciesfile = discrepancies[,.(GlobalCompanyKey,conm,datadate,calendaryear,DataYearFiscal,myloc,mySIC,myassets,myrawmktval,mycommonshares,mypricefiscal,mypriceannual,myMktVal,hisloc,hisSIC,hisassets,hisrawmktval,hiscommonshares,hispricefiscal,hispriceannual,hisMktVal,message)]
setnames(discrepanciesfile,c('Global Company Key','Company Name','Date','Year','Fiscal Year',
	'My Location','My SIC','My Asset Value','My Market Value Total Variable (unadjusted)','My Common Shares Outstanding','My Price Close - Fiscal','My Price Close - Annual','My Market Value (computed)',
	'Your Location','Your SIC','Your Asset Value','Your Market Value Total Variable (unadjusted)','Your Common Shares Outstanding','Your Price Close - Fiscal','Your Price Close - Annual','Your Market Value (computed)',
	'Explanation of Discrepancy'))

write.xlsx(discrepanciesfile,'Compustat/discrepancies.xlsx')
# combo[,differentNAs:=(is.na(myMktVal)&!is.na(hisMktVal))|(!is.na(myMktVal)&is.na(hisMktVal))]
# unexplained = rbind(
#   combo[is.na(hisMktVal)&historical==0&recentindustrychange==0&locdiff==0&newfilings==0&mistakeninclusion==0&hehassomefirmswith0assets==F&ihavepricetohigherprecision==F&theresanerrorinCommonSharesOutstandinghere==F],
#   combo[is.na(myMktVal)&historical==0&recentindustrychange==0&locdiff==0&newfilings==0&mistakeninclusion==0&hehassomefirmswith0assets==F])