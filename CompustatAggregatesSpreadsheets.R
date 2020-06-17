library(data.table)
library(openxlsx)
library(foreach)

#Aggregates

setwd('C:/Users/Nathan/Downloads')
dtcut = readRDS('Compustat/dtcut_for_spreadsheets.rds')

sheet_names = c('All Firms','All Firms With Market Value',
                'Non-Financial, Has Market Value','Financial, Has Market Value',
                'Non-Financial, No Market Value','Financial, No Market Value')
throwaway = foreach(sheet_name=sheet_names)%do%{

if(sheet_name == 'All Firms') exclusions = c(F,F,F,F)
if(sheet_name == 'All Firms With Market Value') exclusions = c(T,F,F,F)

if(sheet_name == 'Non-Financial, Has Market Value') exclusions = c(T,F,T,F)
if(sheet_name == 'Financial, Has Market Value') exclusions = c(T,F,F,T)

if(sheet_name == 'Non-Financial, No Market Value') exclusions = c(F,T,T,F)
if(sheet_name == 'Financial, No Market Value') exclusions = c(F,T,F,T)

excl_firms_without_market_value = exclusions[1]
excl_firms_with_market_value = exclusions[2]
exclude_financial_firms = exclusions[3]
exclude_nonfinancial_firms = exclusions[4]


if(excl_firms_without_market_value==T) dtcut = dtcut[!is.na(MktVal)&MktVal>0]
if(excl_firms_with_market_value==T) dtcut = dtcut[is.na(MktVal)|MktVal<=0]

if(exclude_financial_firms == T) dtcut = dtcut[SIC<6000|SIC>6499]
if(exclude_nonfinancial_firms == T) dtcut = dtcut[SIC>=6000&SIC<=6499]

# aggregates = dtcut[, sapply(dtcut, is.numeric), with = FALSE][,c(.N,lapply(.SD, sum)),calendaryear,
#                                                               .SDcols=-c('fyr','DataYearFiscal','exchg','StandardIndustrialClassificationHistorical','currentsic','SIC','wasmissing','MarketValueTotalFiscal','mwtw','mwv','intangibleratio','liabilityratio','predictedintangibleratio','predictedliabilityratio')]
# aggregates[,numericgvkey:=NULL]

aggregates = dtcut[,c(.N,lapply(.SD,sum)),calendaryear,
                   .SDcols=c('AssetsTotal','IntangibleAssetsTotal','LiabilitiesTotal',
                             'CommonSharesOutstanding','MktVal','monopolywealth','totalwealth')]
aggregates = cbind(aggregates[,.(calendaryear,N)],aggregates[,!c('calendaryear','N'),with = F])
setkey(aggregates,calendaryear)
aggregates[,mwtw:=monopolywealth/totalwealth][,mwv:=monopolywealth/MktVal]
oldnames = c('MktVal','GlobalCompanyKey','datadate','DataYearFiscal','indfmt',
             'consol','popsrc','datafmt','tic','cusip','conm','curcd',
             'AssetsTotal','CommonSharesOutstanding','IntangibleAssetsTotal',
             'OtherIntangibles','LiabilitiesTotal','PreferredPreferenceStockCapitalTotal',
             'PreferredStockRedemptionValue','PreferredStockLiquidatingValue','PremiumonPreferredStock',
             'PremiumonPreferenceStock','PreferredStockatCarryingValue','exchg',
             'costat','PriceCloseAnnualCalendar','PriceCloseAnnualFiscal',
             'loc','SIC',
             'calendaryear','monopolywealth','totalwealth','mwtw','mwv')
newnames = c('Market Value','Global Company Key','Date','Fiscal Year','Industry Format',
             'Level of Consolidation - Company Annual Descriptor','Population Source',
             'Data Format','Ticker Symbol','CUSIP','Company Name','ISO Currency Code',
             'Assets - Total','Common Shares Outstanding','Intangible Assets - Total',
             'Other Intangibles','Liabilities - Total','Preferred/ Preference Stock (Capital) - Total',
             'Preferred Stock Redemption Value','Preferred Stock Liquidating Value','Premium on Preferred Stock',
             'Premium on Preference Stock','Preferred Stock at Carrying Value','Stock Exchange Code',
             'Active/Inactive Status Marker','Price Close - Annual - Calendar','Price Close - Annual - Fiscal',
             'Current ISO Country Code - Headquarters','Standard Industry Classification Code',
             'Year','Monopoly Wealth','Total Wealth','Monopoly Wealth/Total Wealth','Monopoly Wealth/Value')
indices = which(oldnames%in%names(aggregates))
setnames(aggregates,c(oldnames[indices],'N'),c(newnames[indices],'Number of Firms'))
setcolorder(aggregates,c('Year','Number of Firms','Assets - Total',names(aggregates)[!names(aggregates)%in%c('Year','Number of Firms','Assets - Total')]))
wb_filename = 'Compustat/CompustatAggregatesFinal.xlsx'
tryCatch({wb=loadWorkbook(wb_filename)},
         error = function(e) {wb <<- createWorkbook()})
if(!sheet_name%in%wb$sheet_names) addWorksheet(wb, sheet_name)
sty <- createStyle(wrapText = T,halign = "center",border="bottom",borderColour = "#000000",borderStyle='thick',textDecoration = "bold")
writeData(wb, sheet_name,aggregates,headerStyle=sty)
setColWidths(wb,sheet_name, cols=1:ncol(aggregates), widths = 11)
saveWorkbook(wb, wb_filename,overwrite = T)

NULL
}
