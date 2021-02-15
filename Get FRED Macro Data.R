output_files = c(FREDData = 'Data/FRED Data (Inflation and Interest Rates).rds')

FREDkey = fread('FRED APIkey.csv')
fredr_set_key(FREDkey$APIkey)


fedfunds =
  setkey(
    rbind(
      data.table(fredr('FEDFUNDS'))[, year := year(date)][, i := value],
      setnames(data.table(fredr('FFWSJLOW')), 'value', 'i_low'
             )[data.table(fredr('FFWSJHIGH')), on = 'date', i_high := i.value
             ][, i := (i_low + i_high) / 2
             ][, .(i = mean(i, na.rm = T)), .(month = month(date), year = year(date))],
      fill = T
   )[, .(FedFundsRate = mean(i, na.rm = T)), year],
  year)[year >= 1950]


inflation_measures_raw = rbind(data.table(fredr('CPIAUCSL'))[, measure := 'cpi'],
      data.table(fredr('PPIACO'))[, measure := 'ppi'],
      data.table(fredr('A008RD3Q086SBEA'))[, measure := 'investmentPrice'],
      data.table(fredr('GDPDEF'))[, measure := 'gdpdef'], fill = T)
inflation_measures =
  dcast(inflation_measures_raw[, .(level = mean(value)), .(year = year(date), measure)
                             ][, .(year, level, inflation =  100*(level - shift(level))/level), measure
                             ][year >= 1950],
        year ~ measure, value.var = c('inflation', 'level'))
setnames(inflation_measures, sapply(strsplit(names(inflation_measures), '_', fixed = TRUE), function(x) paste(rev(x), collapse = '_')))


FREDdata = inflation_measures[fedfunds, on = 'year']

saveRDS(FREDdata, output_files['FREDData'])
