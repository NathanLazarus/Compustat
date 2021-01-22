# asdf = readLines('8520MA/9899MA.txt')
# asdf2 = readLines('7020MA/9801MA.txt')
# # # evens = asdf[(1:length(asdf))[(1:length(asdf))%%2 == 0]]
# # # odds = asdf[(1:length(asdf))[(1:length(asdf))%%2 == 1]]
# # # write.table(odds,'testodds.txt', row.names = F)
# # # write.table(evens,'testevens.txt', row.names = F)
# # 
# positions = as.numeric(names(table(unlist(gregexpr('\\ \\ [A-Za-z0-9]',asdf[10:length(asdf)]),c(T,F,F,F)))[table(unlist(gregexpr('\\ \\ [A-Za-z0-9]',asdf[10:length(asdf)]),c(T,F,F,F)))>100]))
# jkl = data.table(read_fwf('8520MA/9899MA.txt', fwf_positions(c(1,positions),c(positions - 1, NA_integer_)),
#                           col_types = cols(.default = col_character())))
# 
# positions2 = as.numeric(names(table(unlist(gregexpr('\\ \\ [A-Za-z0-9]',asdf2[10:length(asdf2)]),c(T,F,F,F)))[table(unlist(gregexpr('\\ \\ [A-Za-z0-9]',asdf[10:length(asdf)]),c(T,F,F,F)))>100]))
# jkl2 = data.table(read_fwf('7020MA/9801MA.txt', fwf_positions(c(1,positions2),c(positions2 - 1, NA_integer_)),
#                           col_types = cols(.default = col_character())))


library(readr)
library(lubridate)

rbind_and_fill = function(...) rbind(...,fill=T)

files = list.files('7020MA', full.names = T)

maxcol = 0
yearlydata = foreach(file = files)%do%{
  asdf = readLines(file)
  col_break_table = table(unlist(gregexpr('\\ \\ [A-Za-z0-9]',asdf[10:length(asdf)]),c(T,F,F,F)))
  positions = as.numeric(names(col_break_table)[col_break_table>2])
  positions = positions[positions >= 0]
  hm = unlist(gregexpr('\\ \\ [A-Za-z0-9%]',asdf[1:10]),c(T,F,F,F))
  positions = sort(unique(hm[!((hm - 1) %in% hm) & !((hm-2) %in% hm) & hm > 0]))
  # eval(parse(text = paste0('the', substr(file,8,13), " = 
  yearly_table = data.table(read_fwf(file,
                                     fwf_positions( c(1, positions), c(positions - 1, NA_integer_)),
                                     col_types = cols(.default = col_character())))
  # ")))
  # eval(parse(text = paste0('the', substr(file,8,13))))
  if(ncol(yearly_table) >= maxcol) {
    MA_data_names = gsub(' NA', '', paste(yearly_table[1],
                                          yearly_table[2],
                                          yearly_table[3],
                                          yearly_table[4],
                                          yearly_table[5],
                                          yearly_table[6]))
    maxcol = ncol(yearly_table)
  }
  yearly_table[7:nrow(yearly_table)]
}

MA_data = rbindlist(yearlydata, fill = T)
setnames(MA_data, MA_data_names)
setnames(MA_data, "Target Current Liabili- ties ($mil)", "Target Current Liabilities ($mil)")
setnames(MA_data, "% Owned After Trans- action", "% Owned After Transaction")
setnames(MA_data, which(names(MA_data)=='High Tech Industry')[1], "Target High Tech Industry")
setnames(MA_data, which(names(MA_data)=='High Tech Industry')[1], "Acquiror High Tech Industry")
setnames(MA_data, which(names(MA_data)=='Target Name')[2], 'Target Full Name')
MA_data[, `Rank Date` := mdy(`Rank Date`)]
MA_data[, `Date Announced` := mdy(`Date Announced`)]
MA_data[, `Date Effective` := mdy(`Date Effective`)]
MA_data[, `Date Effective/ Unconditional` := mdy(`Date Effective/ Unconditional`)]
MA_data[, `Date Withdrawn` := mdy(`Date Withdrawn`)]
MA_data[, `Target Company Date of Fin.` := mdy(`Target Company Date of Fin.`)]

saveRDS(MA_data[`M&A Type` %in% c('Disclosed Dollar Value', 'Undisclosed Dollar Value') &
                  Status == 'Completed' # &
                  # !`Acquiror Name` %in% c('Investor Group', 'Undisclosed Acquiror', 'Creditors',
                  #                         'Investor', 'Employee Stock Ownership Plan', 'Undisclosed Joint Venture',
                  #                         'Bondholders', 'Investors', 'Shareholders', 'Seeking Buyer', 'Preferred Shareholders')
                ], 'SDC_data.rds')
