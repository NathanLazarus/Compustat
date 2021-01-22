library(data.table)

#This leaves financial firms and firms with missing market values in the data

setwd("C:/Users/Nathan/Downloads/Compustat")
companydata=readRDS('companydata.rds')
dt = readRDS('fundamentalsannualdata.rds')
# write_dta(companydata[dt[, c('year1', 'year2') := .(min(fyear), max(fyear)), gvkey], on = 'gvkey', `:=`(year1 = i.year1, year2 = i.year2)],
#           'C:/Users/Nathan/Downloads/Programs-20201123T183843Z-001/Programs/compustat_names.dta')
# write_dta(dt, 'C:/Users/Nathan/Downloads/Programs-20201123T183843Z-001/Programs/compustat_data.dta')

na0 = function(x) ifelse(!is.na(x),x,0)

# dt_deloecker_eeckhout = dt[companydata,on='gvkey']
# dt_deloecker_eeckhout[,(grep('i\\.',names(dt_deloecker_eeckhout),value = T)):=NULL]
# library(haven)
# write_dta(dt_deloecker_eeckhout,'C:/Users/Nathan/Downloads/DeloeckerEeckhout/RMP/data/raw_dt.dta')
companydata[,sic:=as.numeric(sic)]
companydata[,naics:=as.numeric(naics)]
varnames = fread('CompustatVarnames.csv',header=F)
setnames(varnames,c('combined','varname','varfull'))
varnames[,lowervarname := tolower(varname)]
merge = varnames[data.table(lowervarname =tolower(names(dt))),on = 'lowervarname',nomatch = 0]
merge[, UpperCamel := gsub("[^[:alnum:]]","",varfull)]
setnames(dt,merge$lowervarname,merge$UpperCamel)

dt[companydata,on=c(GlobalCompanyKey='gvkey'),
   `:=`(currentsic=i.sic,currentnaics=i.naics,loc=i.loc)]
dt[, SIC:=StandardIndustrialClassificationHistorical]
dt[is.na(SIC),SIC:=currentsic]
dt[, NAICS:=NorthAmericaIndustrialClassificationSystemHistorical]
dt[is.na(NAICS),NAICS:=currentnaics]

dt[, calendaryear:=year(datadate)]
dt[, cusip6 := substr(cusip, 1, 6)]

KLDdata = readRDS('KLD_stats_indicators.rds')
#remove rows that are all NA
KLDdata = KLDdata[KLDdata[, lapply(.SD, is.na), .SDcols = 6:ncol(KLDdata)
                        ][, rowSums(.SD)] != (ncol(KLDdata) - 6 + 1)]
KLDdata[, laborConcern := na0(`Union Relations Concerns`) + na0(`Health and Safety Concerns`) + 
          na0(`Labor Management Concerns`) + na0(`Other Employee Relations Concerns`)]
KLDdata[, laborStrength := na0(`Union Relations Strength`) + na0(`Cash Profit Sharing`) + 
          na0(`Employee Involvement`) + na0(`Heatlh and Safety Strength`) + 
          na0(`Employee Relations Strength`) + na0(`Human Capital Development`) + 
          na0(`Labor Management Strength`) + na0(`Other Employee Relations Strength`)]

KLDdata[laborConcern > 1, laborConcern := 1]
KLDdata[laborStrength > 1, laborStrength := 1]
KLDdata[, cusip6 := substr(cusip, 1, 6)]
KLDclean = KLDdata[!is.na(cusip),
                   .(laborConcern = sum(laborConcern),
                     laborStrength = sum(laborStrength),
                     `Anticompetitive Practices` = sum(`Anticompetitive Practices`)),
                   .(cusip6, year)]
KLDclean[laborConcern > 1, laborConcern := 1]
KLDclean[laborStrength > 1, laborStrength := 1]
KLDclean[`Anticompetitive Practices` > 1, `Anticompetitive Practices` := 1]

#rolling 5 year window
KLDstack = rbind(KLDclean,
                 copy(KLDclean)[, year := year + 1],
                 copy(KLDclean)[, year := year + 2],
                 copy(KLDclean)[, year := year - 1],
                 copy(KLDclean)[, year := year - 2],
                 fill = T)
KLDstack[, .(laborConcern = sum(laborConcern),
             laborStrength = sum(laborStrength),
             `Anticompetitive Practices` = sum(`Anticompetitive Practices`)),
         .(cusip6, year)]
KLDstack[laborConcern > 1, laborConcern := 1]
KLDstack[laborStrength > 1, laborStrength := 1]
KLDstack[`Anticompetitive Practices` > 1, `Anticompetitive Practices` := 1]
KLDstack[, laborRelations := laborStrength - laborConcern]

dt[KLDstack, on = c('cusip6', calendaryear = 'year'), `:=`(
  laborStrength = i.laborStrength, laborConcern = i.laborConcern,
  laborRelations = i.laborRelations, `Anticompetitive Practices` = `i.Anticompetitive Practices`
)]

fwrite(dt,'raw_dt.csv')


dtcut = dt[curcd=='USD'&!is.na(curcd)
           &loc=='USA'
           &consol=='C'
           &datafmt=='STD'
           &AssetsTotal!=0
           &!is.na(SIC)] #the only firms missing SIC codes are firms that have yet to IPO. I don't understand the connection. They have NAICS codes.

dtcut = dtcut[dtcut[,.I[sum(indfmt=='INDL')==0|indfmt=='INDL'],.(GlobalCompanyKey,DataYearFiscal)]$V1] #I use indfmt FS and INDL and then drop FS reports when they're duplicated
dtcut[conm=='DELHAIZE AMERICA INC'&calendaryear==2001&CommonSharesOutstanding==91125.785,
      CommonSharesOutstanding:=dtcut[conm=='DELHAIZE AMERICA INC'&calendaryear==2000]$CommonSharesOutstanding]
dtcut[,MktVal:=MarketValueTotalFiscal]
dtcut[is.na(MktVal),MktVal:=PriceCloseAnnualFiscal*CommonSharesOutstanding]
dtcut[is.na(MktVal),MktVal:=PriceCloseAnnualCalendar*CommonSharesOutstanding]
dtcut[is.na(PreferredPreferenceStockCapitalTotal)&!is.na(PreferredPreferenceStockRedeemable),
      PreferredPreferenceStockCapitalTotal:=PreferredPreferenceStockRedeemable]

dtcut[,preferred:=pmax(PreferredPreferenceStockCapitalTotal,PreferredStockLiquidatingValue,PreferredStockRedemptionValue,PreferredStockConvertible,na.rm = T)]
dtcut[!is.na(preferred),MktVal:=MktVal+preferred]
dtcut = dtcut[MktVal != 0 | is.na(MktVal)] #drop about 100 firms with 0 common shares outstanding, mostly firms in the process of dissolving

dtcut[,haspreviousfiscalyear:=(DataYearFiscal-1)%in%DataYearFiscal,GlobalCompanyKey]
dtcut[,hascalendaryear:=DataYearFiscal%in%calendaryear,GlobalCompanyKey]
dtcut[,missing:=haspreviousfiscalyear&!hascalendaryear][,wasmissing:=0]
missings = dtcut[missing==T]
missings[,calendaryear:=DataYearFiscal][,wasmissing:=1]
dtcut = rbind(dtcut,missings)

setkey(dtcut,GlobalCompanyKey,calendaryear)
dtcut[,keep:=datadate==max(datadate),.(calendaryear,GlobalCompanyKey)]
dtcut = dtcut[keep==T]

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



fwrite(dtcut, 'dtcut.csv', quote = T)