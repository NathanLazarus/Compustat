# takes the KLD stats data and identifies if a company was noted
# for having anti-competitive practices in the given year,
# or two years before or after

input_data = c(KLDData = 'Data/KLD Data (Anticompetitive Practices).rds')

output_files = c(KLDDataClean = 'IntermediateFiles/KLD Data Clean.rds')


KLDdata = readRDS(input_data['KLDData'])

setnames(KLDdata,
         c('pro_con_e', 'emp_str_a', 'emp_str_c',
           'emp_str_d', 'emp_str_g',
           'emp_str_j', 'emp_str_l',
           'emp_str_m', 'emp_str_x',
           'emp_str_num', 'emp_con_a',
           'emp_con_b', 'emp_con_h',
           'emp_con_x', 'emp_con_num'),
         c('Anticompetitive Practices', 'Union Relations Strength', 'Cash Profit Sharing',
           'Employee Involvement', 'Heatlh and Safety Strength',
           'Employee Relations Strength', 'Human Capital Development',
           'Labor Management Strength', 'Other Employee Relations Strength',
           'Total Number of Employee Relations Strengths', 'Union Relations Concerns',
           'Health and Safety Concerns', 'Labor Management Concerns',
           'Other Employee Relations Concerns', 'Total Number of Employee Relations Concerns'))

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

saveRDS(KLDstack, output_files['KLDDataClean'])