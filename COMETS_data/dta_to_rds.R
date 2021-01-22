library(foreach)
library(haven)
library(iterators)
library(snow)
library(doSNOW)

setwd("C:/Users/Nathan/Downloads/Compustat")

dtafiles = grep(
  '\\.dta',
  list.files('COMETS_data', recursive = T),
  value = T
  )
clusters=makeCluster(7)
registerDoSNOW(clusters)
foreach(file = dtafiles, .packages = c('haven', 'data.table'))%dopar%{
  data = data.table(read_dta(paste0('COMETS_data/',file)))
  saveRDS(data, gsub('\\.dta','\\.rds',paste0('COMETS_data/',file)))
  NULL
}
stopCluster(clusters)
