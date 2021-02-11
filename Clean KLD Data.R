library(data.table)

KLDdata = readRDS('Data/KLD Data (Anticompetitive Practices).rds')

na0 = function(x) ifelse(!is.na(x),x,0)

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

saveRDS(KLDstack, 'IntermediateFiles/KLD Data Clean.rds')