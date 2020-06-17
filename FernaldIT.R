library(data.table)
library(openxlsx)
library(foreach)

setwd("C:/Users/Nathan/Downloads")
companydata=readRDS('Compustat/companydata.rds')
dt = readRDS('Compustat/fundamentalsannualdata.rds')
SICtoNAICS_bls = readRDS('Compustat/SIC1987toNAICS2002ratios_bls.rds')
SICtoNAICS_bls[,sic:=as.numeric(sic)]
SICtoNAICS_bls[,naics:=as.numeric(naics)]
SICtoNAICS_bls[,sic_ratio:=sic_ratio/100]

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
dt[is.na(NAICS),NAICS:=currentnaics]

dt[,calendaryear:=year(datadate)]


dtcut = dt[curcd=='USD'&!is.na(curcd)
           &loc=='USA'
           &indfmt=='INDL'
           &consol=='C'
           &datafmt=='STD'
           #&(SIC<6000|SIC>6499)
           &AssetsTotal!=0]


dtcut[conm=='DELHAIZE AMERICA INC'&calendaryear==2001&CommonSharesOutstanding==91125.785,
      CommonSharesOutstanding:=dtcut[conm=='DELHAIZE AMERICA INC'&calendaryear==2001]$CommonSharesOutstanding]
dtcut[,MktVal:=MarketValueTotalFiscal]
dtcut[is.na(MktVal),MktVal:=PriceCloseAnnualFiscal*CommonSharesOutstanding]
dtcut[is.na(MktVal),MktVal:=PriceCloseAnnualCalendar*CommonSharesOutstanding]
dtcut = dtcut[!is.na(MktVal)&MktVal>0]
#dtcut = dtcut[is.na(MktVal)|MktVal<=0]

dtcut[,haspreviousfiscalyear:=(DataYearFiscal-1)%in%DataYearFiscal,GlobalCompanyKey]
dtcut[,hascalendaryear:=DataYearFiscal%in%calendaryear,GlobalCompanyKey]
dtcut[,missing:=haspreviousfiscalyear&!hascalendaryear][,wasmissing:=0]
missings = dtcut[missing==T]
missings[,calendaryear:=DataYearFiscal][,wasmissing:=1]
dtcut = rbind(dtcut,missings)
dtcut[,keep:=datadate==max(datadate),.(calendaryear,GlobalCompanyKey)]
dtcut = dtcut[keep==T]

missingSICvalues = c(100,200,800)
additional_SICs = foreach(val=missingSICvalues,.combine=rbind)%do%{
customcrosswalk = dtcut[SIC==val&!is.na(NAICS),.(numfirms = .N),NAICS]
customcrosswalk[,sic:=val][,sic_ratio:=numfirms/sum(numfirms)]
}
setnames(additional_SICs,'NAICS','naics')
additional_SICs[,numfirms:=NULL]
additional_SICs = rbind(additional_SICs,data.table(sic=9900,naics=999990,sic_ratio=1))
SICtoNAICS = rbind(SICtoNAICS_bls,additional_SICs,fill=T)

# suppressWarnings(dtcut[,sic:=NULL])
missingNAICS = dtcut[is.na(NAICS)]
missingNAICS[SIC%in%SICtoNAICS$sic,sic:=SIC]
missingNAICS[is.na(sic) & (SIC - SIC %% 10) %in% SICtoNAICS$sic, sic := SIC - SIC %% 10]
missingNAICS[is.na(sic) & (SIC - SIC %% 100) %in% SICtoNAICS$sic, sic := SIC - SIC %% 100]
# setkey(dtcut,sic)
# setkey(SICtoNAICS,sic)
# rbind(missingNAICS[SICtoNAICS,eval(.(names...)),by=.EACHI],missingNAICS[sic>9000])
addingNAICS = merge(missingNAICS,SICtoNAICS,by='sic',all.x = T,allow.cartesian=T)
addingNAICS[,NAICSadded:=1]
addingNAICS[,`:=`(`CES SIC Tabulating Code`=NULL,
                  `SIC Industry`=NULL,
                  `CES NAICS Tabulating Code`=NULL,
                  `NAICS Industry`=NULL,
                  `SIC to NAICS Employment Ratio`=NULL)]
setnames(addingNAICS,'naics','imputed_NAICS')
withNAICS = rbind(dtcut,addingNAICS,fill=T)
withNAICS[is.na(NAICS)&is.na(NAICSadded),sic_ratio:=0]
withNAICS[,realfirm:=is.na(NAICSadded)]
withNAICS[!is.na(NAICS)&is.na(NAICSadded),sic_ratio:=1]
withNAICS[!is.na(NAICS),true_NAICS:=NAICS][is.na(NAICS),true_NAICS:=imputed_NAICS]
withNAICS[true_NAICS<100,true_NAICS:=true_NAICS*10000]
withNAICS[true_NAICS<1000,true_NAICS:=true_NAICS*1000]
withNAICS[true_NAICS<10000,true_NAICS:=true_NAICS*100]
withNAICS[true_NAICS<100000,true_NAICS:=true_NAICS*10]

withNAICS[,two_digit_NAICS:=true_NAICS-true_NAICS%%10000]
withNAICS[true_NAICS%%10000!=0,three_digit_NAICS:=true_NAICS-true_NAICS%%1000]
missing_three_digit_NAICS = withNAICS[true_NAICS%%10000==0]


two_digit_to_three = withNAICS[!is.na(three_digit_NAICS)&realfirm==T,
                               .(numfirms = .N,two_digit_NAICS=median(two_digit_NAICS)),
                               by = three_digit_NAICS]
two_digit_to_three[,two_to_three_ratio:=numfirms/sum(numfirms),two_digit_NAICS]
setnames(two_digit_to_three,'three_digit_NAICS','imputed_three_digit_NAICS')
two_digit_to_three[,numfirms:=NULL]
addingThreeDigit = merge(missing_three_digit_NAICS,two_digit_to_three,by='two_digit_NAICS',all.x = T,allow.cartesian=T)
addingThreeDigit[,ThreeDigitadded:=1]
addingThreeDigit[,three_digit_ratio := two_to_three_ratio*sic_ratio]
withThreeDigit = rbind(withNAICS,addingThreeDigit,fill=T)
withThreeDigit[is.na(three_digit_NAICS)&is.na(ThreeDigitadded),three_digit_ratio:=0]
withThreeDigit[,realfirm:=is.na(NAICSadded)&is.na(ThreeDigitadded)]
withThreeDigit[!is.na(three_digit_NAICS)&is.na(ThreeDigitadded),three_digit_ratio:=1]
withThreeDigit[!is.na(three_digit_NAICS),true_three_digit_NAICS:=three_digit_NAICS][is.na(three_digit_NAICS),true_three_digit_NAICS:=imputed_three_digit_NAICS]

withThreeDigit[true_NAICS%%1000!=0,four_digit_NAICS:=true_NAICS-true_NAICS%%100]
missing_four_digit_NAICS = withThreeDigit[true_NAICS%%1000==0]


three_digit_to_four = withThreeDigit[!is.na(four_digit_NAICS)&realfirm==T,
                               .(numfirms = .N,true_three_digit_NAICS=median(true_three_digit_NAICS)),
                               by = four_digit_NAICS]
three_digit_to_four[,three_to_four_ratio:=numfirms/sum(numfirms),true_three_digit_NAICS]
setnames(three_digit_to_four,'four_digit_NAICS','imputed_four_digit_NAICS')
addingFourDigit = merge(missing_four_digit_NAICS,three_digit_to_four,by='true_three_digit_NAICS',all.x = T,allow.cartesian=T)
addingFourDigit[,FourDigitadded:=1]
addingFourDigit[,four_digit_ratio := three_to_four_ratio*three_digit_ratio]
withFourDigit = rbind(withThreeDigit,addingFourDigit,fill=T)

withFourDigit = withFourDigit[!is.na(four_digit_NAICS)|!is.na(imputed_four_digit_NAICS)]

withFourDigit[is.na(four_digit_NAICS)&is.na(FourDigitadded),four_digit_ratio:=0]
withFourDigit[,realfirm:=is.na(NAICSadded)&is.na(ThreeDigitadded)&is.na(FourDigitadded)]
withFourDigit[!is.na(four_digit_NAICS)&is.na(FourDigitadded),four_digit_ratio:=1]
withFourDigit[!is.na(four_digit_NAICS),true_four_digit_NAICS:=four_digit_NAICS][is.na(four_digit_NAICS),true_four_digit_NAICS:=imputed_four_digit_NAICS]
withFourDigit[,monopolywealth:=(MktVal-AssetsTotal)*four_digit_ratio]
withFourDigit[,totalwealth:=MktVal*four_digit_ratio]


withFourDigit[,itprod:=true_four_digit_NAICS%/%100==5415
                      |true_four_digit_NAICS%/%1000==511
                      |true_four_digit_NAICS%/%1000==516
                      |true_four_digit_NAICS%/%1000==334]

it_intensive_list = c(22,42,481,483,486,
                 512,515,517,518,519,
                 5412,5413,5414,5416,5417,5418,5419,
                 55,561,621)

non_financial_list = c(311,312,313,314,315,316,
                  322,323,324,325,326,321,327,
                  331,332,333,334,335,336,337,339,
                  22,42,44,45,481,482,483,484,485,486,487,488,492,493,
                  511,516,512,515,517,518,519,5411,5415,
                  5412,5413,5414,5416,5417,5418,5419,
                  55,561,562,61,621,622,623,624,711,712,713,721,722,81)

withFourDigit[,it_intensive:=(true_four_digit_NAICS%/%100)%in%it_intensive_list
                            |(true_four_digit_NAICS%/%1000)%in%it_intensive_list
                            |(true_four_digit_NAICS%/%10000)%in%it_intensive_list]

withFourDigit[,non_financial:=(true_four_digit_NAICS%/%100)%in%non_financial_list
              |(true_four_digit_NAICS%/%1000)%in%non_financial_list
              |(true_four_digit_NAICS%/%10000)%in%non_financial_list]

it_producing_data = withFourDigit[non_financial==T,.(monopolywealth = sum(monopolywealth)),.(itprod,calendaryear)]
it_intensive_data = withFourDigit[non_financial==T&itprod==F,.(monopolywealth = sum(monopolywealth)),.(it_intensive,calendaryear)]
setkey(it_producing_data,calendaryear)
setkey(it_intensive_data,calendaryear)

it_producing_data = withFourDigit[non_financial==T&itprod==T,.(monopolywealth = sum(monopolywealth),totalwealth = sum(totalwealth)),calendaryear]
it_intensive_data = withFourDigit[non_financial==T&itprod==F&it_intensive==T,.(monopolywealth = sum(monopolywealth),totalwealth = sum(totalwealth)),calendaryear]
non_it_data = withFourDigit[non_financial==T&itprod==F&it_intensive==F,.(monopolywealth = sum(monopolywealth),totalwealth = sum(totalwealth)),calendaryear]
setkey(it_producing_data,calendaryear)
setkey(it_intensive_data,calendaryear)
setkey(non_it_data,calendaryear)
it_producing_data[it_intensive_data,`:=`(it_intensive_monopolywealth=i.monopolywealth,it_intensive_totalwealth=i.totalwealth)]
it_producing_data[non_it_data,`:=`(non_it_monopolywealth=i.monopolywealth,non_it_totalwealth=i.totalwealth)]
fwrite(it_producing_data,'Compustat/MonopolyWealthByIT.csv')
#516110 becomes 519130