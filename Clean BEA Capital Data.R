# takes the BEA data on IT capital by industry and
# puts it into a usable, though obnoxiously wide, format

input_data = c(overallCapital = 'Data/cap_details.xlsx',
               intellectualPropertyCapital = 'Data/cap_details_ipp.xlsx',
               informationCapital = 'Data/cap_details_ipe.xlsx')

output_files = c(BEACapitalByIndustry = 'IntermediateFiles/BEA Capital Assets by Industry.rds')

aggregate_capital = 
  data.table(read.xlsx(input_data['overallCapital'], sheet = 'DATA')
           )[NAICS == 'FB', NAICS := '111,112'
           ][grep('487', NAICS), NAICS := '487488491492'
           ][grep('[0-9]',NAICS)
           ][Measure == 'Productive capital stock (direct aggregate-billions of 2012 dollars)' &
               Asset.Category == 'All assets' & Duration.Title == 'Levels'
           ][Asset.Category == 'All assets',Asset.Category := 'All_Assets']

intellectual_property_capital =
  data.table(read.xlsx(input_data['intellectualPropertyCapital'], sheet = 'Data')
           )[NAICS == 'FB', NAICS := '111,112'
           ][grep('487', NAICS), NAICS := '487488491492'
           ][grep('[0-9]',NAICS)
           ][Measure == 'Productive capital stock (direct aggregate-billions of 2012 dollars)' &
               Asset.Category != 'Total' & Duration.Title == 'Levels']

information_capital = 
  data.table(read.xlsx(input_data['informationCapital'],sheet = 'Data')
           )[NAICS == 'FB', NAICS := '111,112'
           ][grep('487', NAICS), NAICS := '487488491492'
           ][grep('[0-9]', NAICS)
           ][Measure == 'Productive capital stock (direct aggregate-billions of 2012 dollars)' &
               Asset.Category != 'Total' & Duration.Title == 'Levels']

yearcols_to_numeric = function(x) suppressWarnings(
  x[,
    (names(x)[names(x) %in% as.character(1900:2030)]) := lapply(.SD,as.numeric),
    .SDcols = names(x)[names(x) %in% as.character(1900:2030)]
    ]
)
yearcols_to_numeric(aggregate_capital)
yearcols_to_numeric(information_capital)
yearcols_to_numeric(intellectual_property_capital)

intellectual_property_capital = dcast(intellectual_property_capital, NAICS + NAICS.Title + Measure + Duration.Title ~ Asset.Category,
                                      value.var = grep('^[0-9]{4}$',names(intellectual_property_capital), value = T))
information_capital = dcast(information_capital, NAICS + NAICS.Title + Measure + Duration.Title ~ Asset.Category,
                            value.var = grep('^[0-9]{4}$',names(information_capital), value = T))
aggregate_capital = dcast(aggregate_capital, NAICS + NAICS.Title + Measure + Duration.Title ~ Asset.Category,
                          value.var = grep('^[0-9]{4}$',names(aggregate_capital), value = T))
setnames(aggregate_capital,sub('(?<=[0-9]{4})','_capital',names(aggregate_capital),perl = T))
capital_data = aggregate_capital[information_capital, on = 'NAICS'
                               ][intellectual_property_capital, on = 'NAICS']

aggregate_capital[grep('^[0-9]{5,}$',NAICS),NAICS := prettyNum(NAICS, big.mark = ',')] #insert commas into the NAICS codes of 5-digits or more (which are actually three digit codes squished together)


aggregate_capital = aggregate_capital[aggregate_capital[,.(split = trimws(unlist(strsplit(NAICS,',')))),by = NAICS],on = 'NAICS']
aggregate_capital[, c('NAICSmin','NAICSmax') := tstrsplit(split, '-')
                ][is.na(NAICSmax),NAICSmax := NAICSmin]
aggregate_capital[, NAICSmin := as.numeric(str_pad(NAICSmin, width = 6, side = 'right', pad = '0'))]
aggregate_capital[, NAICSmax := as.numeric(str_pad(NAICSmax, width = 6, side = 'right', pad = '9'))]
BEA_capital_assets = aggregate_capital[intellectual_property_capital,on = c('NAICS.Title','Measure','Duration.Title')
                                     ][, i.NAICS := NULL
                                     ][information_capital,on = c('NAICS.Title', 'Measure', 'Duration.Title')
                                     ][, i.NAICS := NULL
                                     ]
setnames(BEA_capital_assets, 'NAICS', 'BEA_NAICS_str')

foreach(year = 1987:2018) %do% {
  eval(parse(text = paste0('BEA_capital_assets[,it_capital_',year,' := sum(`',year,'_Software`, `',year,'_Computers`, `',year,'_Communication`, `',year,'_Other`, na.rm = T), by = 1:nrow(BEA_capital_assets)]')))
  eval(parse(text = paste0('BEA_capital_assets[,it_capital_ratio_',year,' := it_capital_',year,' / `',year,'_capital_All_Assets`]')))
  eval(parse(text = paste0('BEA_capital_assets[,computer_capital_ratio_',year,' := `',year,'_Computers` / `',year,'_capital_All_Assets`]')))
  eval(parse(text = paste0('BEA_capital_assets[,communication_capital_ratio_',year,' := `',year,'_Communication` / `',year,'_capital_All_Assets`]')))
  eval(parse(text = paste0('BEA_capital_assets[,other_capital_ratio_',year,' := `',year,'_Other` / `',year,'_capital_All_Assets`]')))
  eval(parse(text = paste0('BEA_capital_assets[,software_capital_ratio_',year,' := `',year,'_Software` / `',year,'_capital_All_Assets`]')))
  eval(parse(text = paste0('BEA_capital_assets[,artistic_capital_ratio_',year,' := `',year,'_Artistic originals` / `',year,'_capital_All_Assets`]')))
  eval(parse(text = paste0('BEA_capital_assets[,rd_capital_ratio_',year,' := `',year,'_Research and development` / `',year,'_capital_All_Assets`]')))
  eval(parse(text = paste0('BEA_capital_assets[,ip_capital_',year,' := sum(`',year,'_Software`, `',year,'_Artistic originals`, `',year,'_Research and development`, na.rm = T), by = 1:nrow(BEA_capital_assets)]')))
  eval(parse(text = paste0('BEA_capital_assets[,ip_capital_ratio_',year,' := ip_capital_',year,' / `',year,'_capital_All_Assets`]')))
  NULL
}

BEA_capital_assets[, average_it_capital_ratio := rowMeans(.SD, na.rm = T), .SDcols = grep('it_capital_ratio', names(BEA_capital_assets), value = T)]
BEA_capital_assets[, average_ip_capital_ratio := rowMeans(.SD, na.rm = T), .SDcols = grep('ip_capital_ratio', names(BEA_capital_assets), value = T)]
BEA_capital_assets[, average_computer_capital_ratio := rowMeans(.SD, na.rm = T), .SDcols = grep('computer_capital_ratio', names(BEA_capital_assets), value = T)]

saveRDS(BEA_capital_assets, output_files['BEACapitalByIndustry'])