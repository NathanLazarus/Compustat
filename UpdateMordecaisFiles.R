library(foreach)
library(iterators)
library(snow)
library(doSNOW)
library(openxlsx)

setwd('C:/Users/Nathan/Downloads')

dt = readRDS('Compustat/raw_dt.rds')

raw = dt[indfmt=='INDL'&consol=='C'&datafmt=='STD']
trimmed = dt[curcd=='USD'&!is.na(curcd)
         &loc=='USA'
         &indfmt=='INDL'
         &consol=='C'
         &datafmt=='STD'
         &AssetsTotal!=0
         &!is.na(SIC)]

final = readRDS('Compustat/dtcut_for_spreadsheets.rds')[,
       .(AssetsTotal,IntangibleAssetsTotal,LiabilitiesTotal,MktVal,monopolywealth,totalwealth,
         MarketValueTotalFiscal,PriceCloseAnnualCalendar,PriceCloseAnnualFiscal,CommonSharesOutstanding,preferred,
         IntellectualProperty,Goodwill,
         realestate,equipment,
         InventoriesTotal,
         reweightedIntellectualPropertyadjustment,reweightedRealEstateadjustment,reweightedEquipmentadjustment,reweightedInventoriesadjustment,
         ReceivablesTotal,CashandShortTermInvestments,CurrentAssetsOtherTotal,
         InvestmentandAdvancesEquity,InvestmentandAdvancesOther,
         AssetsOther,DeferredCharges,PrepaidExpenses,
         calendaryear,wasmissing,intangiblesadded,liabilitiesadded)]
# saveRDS(final,'Compustat/update_Mordecais_files_final_processed.rds')
# final = readRDS('Compustat/update_Mordecais_files_final_processed.rds')


# updateMordecaisFiles = copy(dtcut)
# updateMordecaisFiles[,`:=`(fyr=NULL,currentsic=NULL,haspreviousfiscalyear=NULL,hascalendaryear=NULL,missing=NULL,wasmissing=NULL,keep=NULL,theresanerrorinCommonSharesOutstandinghere=NULL,MarketValueTotalFiscal=NULL,StandardIndustrialClassificationHistorical=NULL)]
keep = c('GlobalCompanyKey','datadate','DataYearFiscal','indfmt','consol','popsrc','datafmt','tic','cusip','conm','curcd','AssetsTotal','CommonSharesOutstanding','Goodwill','IntangibleAssetsTotal','OtherIntangibles','LiabilitiesTotal','PreferredPreferenceStockCapitalTotal','PreferredStockRedemptionValue','PreferredStockLiquidatingValue','PreferredStockConvertible','PreferredPreferenceStockNonredeemable','PreferredPreferenceStockRedeemable','PremiumonPreferredStock','PremiumonPreferenceStock','PreferredStockatCarryingValue','exchg','costat','PriceCloseAnnualCalendar','MarketValueTotalFiscal','PriceCloseAnnualFiscal','InvestmentandAdvancesEquity','InvestmentandAdvancesOther','PropertyPlantandEquipmentTotalNet','CurrentAssetsTotal','AssetsOther','CashandShortTermInvestments','Cash','ShortTermInvestmentsTotal','CurrentAssetsOtherTotal','InventoriesTotal','InventoriesFinishedGoods','InventoriesOther','InventoriesRawMaterials','InventoriesWorkInProcess','InventoryStockOther','InventoryStockRealEstateHeldforDevelopment','InventoryStockRealEstateHeldforSale','InventoryStockRealEstateUnderDevelopment','ReceivablesTotal','ReceivablesTrade','UnbilledReceivables','PropertyPlantandEquipmentMachineryandEquipmentatCost','PropertyPlantandEquipmentNaturalResourcesatCost','PropertyPlantandEquipmentOtheratCost','PropertyPlantandEquipmentMachineryandEquipmentNet','PropertyPlantandEquipmentNaturalResourcesNet','PropertyPlantandEquipmentOtherNet','PropertyPlantandEquipmentBuildingsatCost','PropertyPlantandEquipmentConstructioninProgressatCost','PropertyPlantandEquipmentLeasesatCost','PropertyPlantandEquipmentLandandImprovementsatCost','PropertyPlantandEquipmentBuildingsNet','PropertyPlantandEquipmentConstructioninProgressNet','PropertyPlantandEquipmentLandandImprovementsNet','PropertyPlantandEquipmentLeasesNet','AssetsOtherSundry','DeferredCharges','PrepaidExpenses','loc','SIC','NAICS','calendaryear')
getridof = c('fyr','StandardIndustrialClassificationHistorical','NorthAmericaIndustrialClassificationSystemHistorical','ReceivablesCurrentOther','ReceivablesEstimatedDoubtful','ExchangeAdjustmentsAssets','UnappropriatedNetLoss','TreasuryStockAssets','currentsic','currentnaics')
raw = raw[,..keep]
trimmed = trimmed[,..keep]

all_newcolnamesandorder = foreach(i_table=list(raw,trimmed,final))%do%{
oldcolnamesandorder = data.table(names = c('GlobalCompanyKey','datadate','DataYearFiscal','indfmt','consol','popsrc','datafmt','tic','cusip','conm','curcd','AssetsTotal','CommonSharesOutstanding','IntangibleAssetsTotal','OtherIntangibles','LiabilitiesTotal','PreferredPreferenceStockCapitalTotal','PreferredStockRedemptionValue','PreferredStockLiquidatingValue','PremiumonPreferredStock','PremiumonPreferenceStock','PreferredStockatCarryingValue','exchg','costat','PriceCloseAnnualCalendar','PriceCloseAnnualFiscal','loc','SIC','calendaryear','MktVal'))
oldcolnamesandorder[,order := .I]
customcolwidthtable = rbind(data.table(c(4,29,30),8.8),data.table(c(1,11,14,17:25),10),data.table(c(2,9,15,16),10.8),data.table(13,11.7),data.table(5,13),data.table(10,15))
setnames(customcolwidthtable,c('order','width'))
olddefault = data.table(order = (1:30)[!1:30%in%customcolwidthtable$order],width = 7.5)
oldcolnamesandorder[rbind(customcolwidthtable,olddefault),on='order',width := i.width]
newcolnamesandorder = data.table(names = names(i_table),order=1:length(names(i_table)))
newcolnamesandorder[oldcolnamesandorder,on='names',`:=`(width=i.width,oldorder=i.order)][is.na(oldorder)|grepl('Preferred|Preference',names),oldorder := 99L+order]
setkey(newcolnamesandorder,oldorder)
newcolnamesandorder[,neworder:=.I]
newcolnamesandorder[names=='SIC',width:=12]
newcolnamesandorder[names=='NAICS',width:=12]
newcolnamesandorder[names=='loc',width:=12.5]
newcolnamesandorder[names=='wasmissing',width:=17.2]
newcolnamesandorder[names=='intangiblesadded',width:=10]
newcolnamesandorder[names=='popsrc',width:=10]
newcolnamesandorder[names=='IntellectualProperty',width:=10.6]
newcolnamesandorder[names=='monopolywealth',width:=10]
newcolnamesandorder[names=='PreferredStockRedemptionValue',width:=11]
newcolnamesandorder[grepl('PreferredPreference',names),width:=10]
newcolnamesandorder[grepl('adjustment',names)&is.na(width),width:=13.2]
newcolnamesandorder[grepl('Improvements',names)&is.na(width),width:=13.2]
newcolnamesandorder[grepl('Construction',names)&is.na(width),width:=11.8]
newcolnamesandorder[grepl('Receivables',names)&is.na(width),width:=10.8]
newcolnamesandorder[grepl('Investment',names)&is.na(width),width:=11]
newcolnamesandorder[grepl('Development',names)&is.na(width),width:=14.2]
newcolnamesandorder[grepl('Equipment',names,ignore.case = T)&is.na(width),width:=10]
newcolnamesandorder[grepl('Inventories',names)&is.na(width),width:=11]
newcolnamesandorder[is.na(width),width:=9]
setcolorder(i_table,newcolnamesandorder$names)


varnames[,varnice:=ifelse(nchar(varname)+5 < nchar(combined),substring(combined,nchar(varname)+5),combined)]
varnames[,myvarname := gsub("[^[:alnum:]]","",varfull)]

customvars = c('GlobalCompanyKey','datadate','DataYearFiscal','indfmt','consol','popsrc','datafmt','tic','cusip','conm','curcd','AssetsTotal','CommonSharesOutstanding','IntangibleAssetsTotal','OtherIntangibles','LiabilitiesTotal','PreferredPreferenceStockCapitalTotal','PreferredStockRedemptionValue','PreferredStockLiquidatingValue','PremiumonPreferredStock','PremiumonPreferenceStock','PreferredStockatCarryingValue','exchg','costat','PriceCloseAnnualCalendar','PriceCloseAnnualFiscal',
               'loc','SIC','NAICS','calendaryear',
               'intangiblesadded','liabilitiesadded','wasmissing',
               'MktVal','monopolywealth','totalwealth','IntellectualProperty','preferred','realestate','equipment',
               'reweightedIntellectualPropertyadjustment','reweightedRealEstateadjustment',
               'reweightedEquipmentadjustment','reweightedInventoriesadjustment')
customvarnames = c('Global Company Key','Date','Fiscal Year','Industry Format','Level of Consolidation - Company Annual Descriptor','Population Source','Data Format','Ticker Symbol','CUSIP','Company Name','ISO Currency Code','Assets - Total','Common Shares Outstanding','Intangible Assets - Total','Other Intangibles','Liabilities - Total','Preferred/Preference Stock (Capital) - Total','Preferred Stock Redemption Value','Preferred Stock Liquidating Value','Premium on Preferred Stock','Premium on Preference Stock','Preferred Stock at Carrying Value','Stock Exchange Code','Active/Inactive Status Marker','Price Close - Annual - Calendar','Price Close - Annual - Fiscal',
                   'Current ISO Country Code - Headquarters','Standard Industry Classification Code','North American Industry Classification Code','Calendar Year',
                   'Intangibles Imputed','Liabilities Imputed','Missing Calendar Year Data Because of Fiscal Year-End Month Changing',
                   'Market Value','Monopoly Wealth','Total Wealth','Intellectual Property (Intangibles excluding Goodwill)','Preferred Stock Total','Real Estate','Equipment',
                   'Intellectual Property and Goodwill Book Value to Market Value Adjustment','Real Estate Book Value to Market Value Adjustment',
                   'Equipment Book Value to Market Value Adjustment','Inventories Book Value to Market Value Adjustment')

needsvarname = data.table(myvarname = names(i_table)[!names(i_table) %in% customvars])
needsvarname[varnames,on='myvarname',varnice := i.varnice]
needsvarname[is.na(varnice),varnice := myvarname]

setnames(i_table,customvars[customvars %in% names(i_table)],customvarnames[customvars %in% names(i_table)])
setnames(i_table,needsvarname$myvarname,needsvarname$varnice)
newcolnamesandorder
}
Sys.time()
updating_old_files = TRUE
all_years = seq(min(final$`Calendar Year`),max(final$`Calendar Year`))
pb = txtProgressBar(max = length(all_years), style = 3)
progress = function(n) setTxtProgressBar(pb, n)
opts = list(progress = progress)
clusters=makeCluster(7)
registerDoSNOW(clusters)
throwaway = foreach(yr=2013:2017,.options.snow = opts,.packages = 'openxlsx')%do%{
  if(updating_old_files==TRUE) {
    filename = paste0('Compustat/Compustat Annual Files Updated/What I Sent Mordecai/Final Compustat Edited Files/Final File ',yr,'.xlsx')
    tryCatch({wb=loadWorkbook(filename)},
           error = function(e) {wb <<- createWorkbook()})
    if("Update2019" %in% names(wb)) removeWorksheet(wb, "Update2019")
    if("Updated Raw WRDS Data" %in% names(wb)) removeWorksheet(wb, "Updated Raw WRDS Data")
    if("Updated Trimmed Firm Universe" %in% names(wb)) removeWorksheet(wb, "Updated Trimmed Firm Universe")
    if("Updated Monopoly Wealth Calc" %in% names(wb)) removeWorksheet(wb, "Updated Monopoly Wealth Calc")
    raw_sheet_name = "Updated Raw WRDS Data"
    trimmed_sheet_name = "Updated Trimmed Firm Universe"
    final_sheet_name = "Updated Monopoly Wealth Calc"
  } else {
    filename = paste0('Compustat/Compustat Annual Files Updated/Nathans Updated Files/Update Only File ',yr,'.xlsx')
    wb = createWorkbook()
    if("Raw WRDS Data" %in% names(wb)) removeWorksheet(wb, "Raw WRDS Data")
    if("Trimmed Universe of Firms" %in% names(wb)) removeWorksheet(wb, "Trimmed Universe of Firms")
    if("Monopoly Wealth Calculation" %in% names(wb)) removeWorksheet(wb, "Monopoly Wealth Calculation")
    raw_sheet_name = "Raw WRDS Data"
    trimmed_sheet_name = "Trimmed Universe of Firms"
    final_sheet_name = "Monopoly Wealth Calculation"
  }
  
  addWorksheet(wb, raw_sheet_name)
  addWorksheet(wb, trimmed_sheet_name)
  addWorksheet(wb, final_sheet_name) 
  
  sty = createStyle(wrapText = T,halign = "center",border="bottom",borderColour = "#000000",borderStyle='thick',textDecoration = "bold")
  
  writeData(wb, raw_sheet_name,raw[`Calendar Year`==yr],headerStyle=sty)
  setColWidths(wb,raw_sheet_name,cols = all_newcolnamesandorder[[1]]$neworder,widths = all_newcolnamesandorder[[1]]$width)
  
  
  writeData(wb, trimmed_sheet_name,trimmed[`Calendar Year`==yr],headerStyle=sty)
  setColWidths(wb,trimmed_sheet_name,cols = all_newcolnamesandorder[[2]]$neworder,widths = all_newcolnamesandorder[[2]]$width)
  
  
  writeData(wb, final_sheet_name,final[`Calendar Year`==yr],headerStyle=sty)
  setColWidths(wb,final_sheet_name,cols = all_newcolnamesandorder[[3]]$neworder,widths = all_newcolnamesandorder[[3]]$width)
  
  saveWorkbook(wb, filename,overwrite = T)
}
stopCluster(clusters)
Sys.time()



# setColWidths(wb,"Update2019", cols=1:ncol(raw), widths =7.5)
# setColWidths(wb,"Update2019", cols=c(4,29,30), widths=8.8)
# setColWidths(wb,"Update2019", cols=c(1,11,14,17:25), widths=10)
# setColWidths(wb,"Update2019", cols=c(2,9,15,16), widths=10.8)
# setColWidths(wb,"Update2019", cols=13, widths=11.7)
# setColWidths(wb,"Update2019", cols=5, widths=13)
# setColWidths(wb,"Update2019", cols=10, widths=15)