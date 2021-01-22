library(readxl)
setwd('C:/Users/Nathan/Downloads')
asdf = data.table(read_excel('Final File 1990.xlsx',sheet=4))
asdf = asdf[1:nrow(asdf)-1]
asdf[,duplicates:=.N-1,`Global Company Key`]
sum(asdf[duplicates>0]$duplicates)/2
sum(asdf[duplicates>0]$`Market Value - Total - Fiscal`)/2
# look into the summ std companies
rbind_and_fill=function(...) rbind(...,fill=T)
library(foreach)
library(iterators)
library(snow)
library(doSNOW)
pb = txtProgressBar(max = length(1950:2015), style = 3)
progress = function(n) setTxtProgressBar(pb, n)
opts = list(progress = progress)
clusters=makeCluster(7)
registerDoSNOW(clusters)
mordecaisdata = foreach(yr=1950:2015,.combine = rbind_and_fill,.multicombine=T,.options.snow = opts,.packages = 'readxl')%dopar%{
  filename = paste0('Compustat/Final Compustat Edited Files/Final Compustat Edited Files/Final File ',yr,'.xlsx')
  sheetnames = excel_sheets(filename)
  sheetIwant = max(which(sheetnames!='Update2019'))
  jkl = data.table(read_excel(filename,sheet= sheetIwant))
  jkl[,sheetname:=sheetnames[sheetIwant]]
  if(is.na(jkl[nrow(jkl)]$`Global Company Key`)|jkl[nrow(jkl)]$`Assets - Total`>(max(jkl$`Assets - Total`)/2)){
    jkl = jkl[1:(nrow(jkl)-1)]
  }
}

mordecaisrawdata = foreach(yr=1950:2015,.combine = rbind_and_fill,.multicombine=T,.options.snow = opts,.packages = 'readxl')%dopar%{
  filename = paste0('Compustat/Compustat basic files Zipped (1)/Compustat basic files/Final File ',yr,'.xlsx')
  sheetnames = excel_sheets(filename)
  jkl = data.table(read_excel(filename,sheet=1))
  jkl[,sheetname:=sheetnames[1]]
}
stopCluster(clusters)

mordecaisdata[,`Data Date`:=as.Date(as.character(`Data Date`),'%Y%m%d')]
mordecaisdata[,calendaryear:=year(`Data Date`)]
mordecaisdata[,haspreviousfiscalyear:=(`Data Year - Fiscal`-1)%in%`Data Year - Fiscal`,`Global Company Key`]
mordecaisdata[,hascalendaryear:=`Data Year - Fiscal`%in%calendaryear,`Global Company Key`]
mordecaisdata[,missing:=haspreviousfiscalyear&!hascalendaryear][,wasmissing:=0]
missings = mordecaisdata[missing==T]
missings[,calendaryear:=`Data Year - Fiscal`][,wasmissing:=1]
mordecaisdata = rbind(mordecaisdata,missings)
mordecaisdata[,keep:=`Data Date`==max(`Data Date`),.(calendaryear,`Global Company Key`)]
mordecaisdata = mordecaisdata[keep==T]

dtcut[,numericgvkey:=as.numeric(GlobalCompanyKey)]
my2010 = dtcut[,calendaryear:=year(datadate)][calendaryear==2010&datafmt=='STD'&loc=='USA'&!is.na(MktVal)]
mordecais2010 = mordecaisdata[calendaryear==2010]
diffs1 = my2010[!mordecais2010,on=c(numericgvkey = 'Global Company Key')]
diffs2 = mordecais2010[!my2010,on=c(`Global Company Key` = 'numericgvkey')]
diffs2[,myloc:='asdf']
dtcutinclforeign[,numericgvkey:=as.numeric(GlobalCompanyKey)][,calendaryear:=year(datadate)]
diffs2[dtcutinclforeign[calendaryear==2010],on=c(`Global Company Key` = 'numericgvkey'),myloc:=i.loc]
notexplainedbyloc = diffs2[myloc=='asdf'] #these are explained by SIC discrepancies #gotta also compare the market values
mordecaisrawdata[,location:=`Current ISO Country Code - Headquarters`]
diffs1[mordecaisrawdata[calendaryear==2010],on=c(numericgvkey = 'Global Company Key'),hisloc := i.location]
notexplainedbyloc2= diffs1[hisloc=='USA']
mordecaisrawdata[,`Data Date`:=as.Date(as.character(`Data Date`),'%Y%m%d')]
mordecaisrawdata[,calendaryear:=year(`Data Date`)]
mordecaisrawdata[,SIC := `Standard Industry Classification Code`]
diffs1[mordecaisrawdata[calendaryear==2010],on=c(numericgvkey = 'Global Company Key'),hisSIC := i.SIC]