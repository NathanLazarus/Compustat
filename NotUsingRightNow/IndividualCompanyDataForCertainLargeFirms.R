forxl = data.table()
# ,'DUPONT DE NEMOURS INC' 2018
conames = c('UNITED STATES STEEL CORP','MARATHON OIL CORP','GENERAL MOTORS CO',
            'CHEVRON CORP','BERKSHIRE HATHAWAY','GENERAL ELECTRIC CO',
            'NORFOLK SOUTHERN CORP','CATERPILLAR INC','SOUTHWEST AIRLINES',
            'DOW INC','ALPHABET INC',
            'MICROSOFT CORP','HONEYWELL INTERNATIONAL INC','3M CO','PEPSICO INC','AMAZON.COM INC','AMGEN INC','FACEBOOK INC')
for(i in conames){
  forxl = rbind(forxl,dtcut[conm==i&datafmt=='STD'&DataYearFiscal==2019])
}
forxl[,capitalemployed:=AssetsTotal - IntangibleAssetsTotal]
library(openxlsx)
sheet_name = 'unformatted'
tryCatch({wb=loadWorkbook('C:/Users/Nathan/Downloads/Compustat/ForIndividualFirms2019.xlsx')},
         error = function(e) {wb <<- createWorkbook()})
if(!sheet_name%in%wb$sheet_names) addWorksheet(wb, sheet_name)
writeData(wb, sheet_name,forxl[,.(conm,datadate,AssetsTotal,IntangibleAssetsTotal,LiabilitiesTotal,MktVal,capitalemployed,monopolywealth,totalwealth,mwtw,mwv)])
saveWorkbook(wb, 'C:/Users/Nathan/Downloads/Compustat/ForIndividualFirms2019.xlsx',overwrite = T)