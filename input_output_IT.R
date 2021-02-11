library(openxlsx)
library(foreach)

output_table = data.table(read.xlsx('Data/IxI_TR_2007_2012_PRO_DET.xlsx',3,startRow = 5))

input_required_table = transpose(output_table[,3:ncol(output_table)])
setnames(input_required_table,output_table$Code)
input_required_table[, BEA_Code := names(output_table)[3:ncol(output_table)]]
foreach(industry = names(input_required_table))%do%{ #diagonals - 1
  input_required_table[BEA_Code == industry, (industry) := eval(parse(text = paste0('`', industry, '`', ' - 1')))]
  NULL
}
it_producing_industries = c('5415','511','516','334')
it_cols = names(input_required_table)[substr(names(input_required_table),1,3) %in% it_producing_industries |
                       substr(names(input_required_table),1,4) %in% it_producing_industries]
input_required_table[, it_input_elasticity := rowSums(.SD, na.rm=T), .SDcols = it_cols]


BEA_industries_to_NAICS = data.table(read.xlsx('Data/Use_SUT_Framework_2007_2012_DET.xlsx',2,startRow = 5))
setnames(BEA_industries_to_NAICS, c('Detail', 'Related.2012.NAICS.Codes'), c('BEA_Code', 'NAICS_Code'))
BEA_industries_to_NAICS = BEA_industries_to_NAICS[
  !is.na(BEA_Code) & !is.na(NAICS_Code) &
    NAICS_Code!= 'n.a.' & NAICS_Code!= 'n.a'
]

#construction can only be matched at the two digit level. Need to come back and take a weighted mean here.
input_required_table = rbind(input_required_table,
                              input_required_table[substr(BEA_Code,1,2) == '23',
                                                   .(BEA_Code = '23', it_input_elasticity = mean(it_input_elasticity))],
                              fill = T
                             )[substr(BEA_Code,1,2) != '23' | BEA_Code == '23']
BEA_industries_to_NAICS = rbind(BEA_industries_to_NAICS,
                                data.table(BEA_Code = '23', NAICS_Code = '23'),
                                fill = T)[substr(BEA_Code,1,2) != '23' | BEA_Code == '23']

input_required_table = rbind(input_required_table,
                             input_required_table[BEA_Code %in% c('531HST', '531ORE'),
                                                  .(BEA_Code = '531HST_ORE', it_input_elasticity = mean(it_input_elasticity))],
                             fill = T
                            )[(!BEA_Code %in% c('531HST', '531ORE'))]
BEA_industries_to_NAICS = rbind(BEA_industries_to_NAICS,
                                data.table(BEA_Code = '531HST_ORE', NAICS_Code = '531'),
                                fill = T)[(!BEA_Code %in% c('531HST', '531ORE'))]



BEA_industries_to_NAICS = BEA_industries_to_NAICS[BEA_industries_to_NAICS[,
                                                                          .(split=trimws(unlist(strsplit(NAICS_Code,',')))),
                                                                          by=NAICS_Code],
                                                  on='NAICS_Code']
BEA_industries_to_NAICS[, c('NAICSmin','NAICSmax') := tstrsplit(split,'-')]
BEA_industries_to_NAICS[
  !is.na(NAICSmax), NAICSmax := substr(NAICSmin, 1, nchar(NAICSmin) - 1)
][
  is.na(NAICSmax),NAICSmax:=NAICSmin
]

BEA_industries_to_NAICS[,NAICSmin:=as.numeric(str_pad(NAICSmin, width=6, side='right', pad='0'))]
BEA_industries_to_NAICS[,NAICSmax:=as.numeric(str_pad(NAICSmax, width=6, side='right', pad='9'))]

BEA_industries_to_NAICS[input_required_table[, .(BEA_Code, it_input_elasticity)],
                        on = 'BEA_Code',
                        it_input_elasticity := i.it_input_elasticity]

fwrite(BEA_industries_to_NAICS[,.(BEA_Code, NAICS_Code, NAICSmin, NAICSmax, it_input_elasticity)],
       'IntermediateFiles/input_output_IT.csv',
       quote = T)