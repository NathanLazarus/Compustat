library(data.table)

#This leaves financial firms and firms with missing market values in the data

setwd("C:/Users/Nathan/Downloads")
companydata=readRDS('Compustat/companydata.rds')
dt = readRDS('Compustat/fundamentalsannualdata.rds')

na0 = function(x) ifelse(!is.na(x),x,0)

companydata[,sic:=as.numeric(sic)]
companydata[,naics:=as.numeric(naics)]
varnames = fread('Compustat/CompustatVarnames.csv',header=F)
setnames(varnames,c('combined','varname','varfull'))
varnames[,lowervarname := tolower(varname)]
merge = varnames[data.table(lowervarname =tolower(names(dt))),on = 'lowervarname',nomatch = 0]
merge[, UpperCamel := gsub("[^[:alnum:]]","",varfull)]
setnames(dt,merge$lowervarname,merge$UpperCamel)

dt[companydata,on=c(GlobalCompanyKey='gvkey'),`:=`(currentsic=i.sic,currentnaics=i.naics,loc=i.loc)]
dt[,SIC:=StandardIndustrialClassificationHistorical]
dt[is.na(SIC),SIC:=currentsic]
dt[,NAICS:=NorthAmericaIndustrialClassificationSystemHistorical]

dt[,calendaryear:=year(datadate)]

saveRDS(dt,'Compustat/raw_dt.rds')

dtcut = dt[curcd=='USD'&!is.na(curcd)
           &loc=='USA'
           &indfmt=='INDL'
           &consol=='C'
           &datafmt=='STD'
           &AssetsTotal!=0
           &!is.na(SIC)] #the only firms missing SIC codes are firms that have yet to IPO. I don't understand the connection. They have NAICS codes.
dtcut[conm=='DELHAIZE AMERICA INC'&calendaryear==2001&CommonSharesOutstanding==91125.785,
      CommonSharesOutstanding:=dtcut[conm=='DELHAIZE AMERICA INC'&calendaryear==2001]$CommonSharesOutstanding]
dtcut[,MktVal:=MarketValueTotalFiscal]
dtcut[is.na(MktVal),MktVal:=PriceCloseAnnualFiscal*CommonSharesOutstanding]
dtcut[is.na(MktVal),MktVal:=PriceCloseAnnualCalendar*CommonSharesOutstanding]
dtcut[is.na(PreferredPreferenceStockCapitalTotal)&!is.na(PreferredPreferenceStockRedeemable),
      PreferredPreferenceStockCapitalTotal:=PreferredPreferenceStockRedeemable]

dtcut[,preferred:=pmax(PreferredPreferenceStockCapitalTotal,PreferredStockLiquidatingValue,PreferredStockRedemptionValue,PreferredStockConvertible,na.rm = T)]
dtcut[!is.na(preferred),MktVal:=MktVal+preferred]

dtcut[,haspreviousfiscalyear:=(DataYearFiscal-1)%in%DataYearFiscal,GlobalCompanyKey]
dtcut[,hascalendaryear:=DataYearFiscal%in%calendaryear,GlobalCompanyKey]
dtcut[,missing:=haspreviousfiscalyear&!hascalendaryear][,wasmissing:=0]
missings = dtcut[missing==T]
missings[,calendaryear:=DataYearFiscal][,wasmissing:=1]
dtcut = rbind(dtcut,missings)

setkey(dtcut,GlobalCompanyKey,calendaryear)
dtcut[,keep:=datadate==max(datadate),.(calendaryear,GlobalCompanyKey)]
dtcut = dtcut[keep==T]

# historicalcost = fread('C:/Users/Nathan/Downloads/HistoricalCostZ1.csv')
# historicalcostgood = historicalcost[substr(`Series Description`,5,6)=='Q4']
# historicalcostgood[,`Series Description`:=substr(`Series Description`,1,4)]
# historicalcostgood = historicalcostgood[,lapply(.SD,as.numeric)]
# setnames(historicalcostgood,c('year','marketprice','historicalprice'))
# historicalcostgood[,adjustment:=marketprice/historicalprice]
# 
# dtcut[historicalcostgood,on=c(calendaryear='year'),adjustment:=i.adjustment]
# 
# setkey(dtcut,calendaryear)
# dtcut = dtcut[calendaryear%in%historicalcostgood$year]

dtcut = dtcut[calendaryear < 2020]

dtcut[,AssetsOther:=AssetsOther-na0(DeferredCharges)-na0(PrepaidExpenses)]
dtcut[,intangibleratio:=IntangibleAssetsTotal/AssetsTotal]
setkey(dtcut,GlobalCompanyKey,calendaryear)

dtcut[,intangiblesadded:= +is.na(IntangibleAssetsTotal)]
dtcut_no_NA_intangibles = dtcut[!is.na(IntangibleAssetsTotal)]
setkey(dtcut_no_NA_intangibles,GlobalCompanyKey,calendaryear)
dtcut[,intangibleratio:=dtcut_no_NA_intangibles[dtcut,intangibleratio,roll='nearest']
    ][is.na(IntangibleAssetsTotal),`:=`(IntangibleAssetsTotal = pmin(intangibleratio*AssetsTotal,na0(Goodwill),pmax(na0(AssetsOther),0)),
                                        AssetsOther = na0(AssetsOther) - na0(pmin(intangibleratio*AssetsTotal,na0(Goodwill),pmax(na0(AssetsOther),0))))]

dtcut[,twodigitsic:=as.character(floor(SIC/100))]

intangiblemod = lm(intangibleratio~factor(calendaryear)+twodigitsic,data=dtcut)
dtcut[,predictedintangibleratio:=pmax(predict(intangiblemod,dtcut),0)
    ][is.na(IntangibleAssetsTotal),`:=`(IntangibleAssetsTotal = pmin(predictedintangibleratio*AssetsTotal,na0(Goodwill),pmax(na0(AssetsOther),0)),
                                        AssetsOther = na0(AssetsOther) - na0(pmin(predictedintangibleratio*AssetsTotal,na0(Goodwill),pmax(na0(AssetsOther),0))))]



saveRDS(dtcut,'Compustat/dtcut.rds')
