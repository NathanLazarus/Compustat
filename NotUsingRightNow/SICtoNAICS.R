library(htmltab)
library(foreach)
digits = c(2,3,4)
bigtable = foreach(digit=digits,.combine = rbind)%do%{
ratiotable = data.table(htmltab(paste0('https://www.bls.gov/ces/naics/sic-',digit,'-digit-to-naics-2002-ratios.htm'),which = 1))
ratiotable[,sic:=substr(`CES SIC Tabulating Code`,4,7)]
ratiotable[,naics:=substr(`CES NAICS Tabulating Code`,4,9)]
ratiotable[,sic_ratio:=as.numeric(`SIC to NAICS Employment Ratio`)]
ratiotable
}
saveRDS(bigtable,'C:/Users/Nathan/Downloads/Compustat/SIC1987toNAICS2002ratios_bls.rds')