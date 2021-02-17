output_files = c(FernaldITcodes = 'IntermediateFiles/Fernald IT Codes.feather')

it_producing_industries = c(5415, 511, 516, 334)

it_intensive_industries = c(22, 42, 481, 483, 486, 
                            512, 515, 517, 518, 519, 
                            5412, 5413, 5414, 5416, 5417, 5418, 5419, 
                            55, 561, 621)

non_IT_industries_non_financial = c(311, 312, 313, 314, 315, 316, 322, 
                                    323, 324, 325, 326, 321, 327, 331, 
                                    332, 333, 335, 336, 337, 339, 44, 
                                    45, 482, 484, 485, 487, 488, 492, 
                                    493, 5411, 562, 61, 622, 623, 624, 
                                    711, 712, 713, 721, 722, 81)

FernaldIT = rbind(data.table(NAICS = it_producing_industries, IT_type = 'IT Producing'), 
                  data.table(NAICS = it_intensive_industries, IT_type = 'IT Intensive'), 
                  data.table(NAICS = non_IT_industries_non_financial, IT_type = 'Non IT'))
FernaldIT[, IT_type := factor(IT_type, levels = c('IT Producing', 'IT Intensive', 'Non IT'))]

FernaldIT[, NAICSmin := as.numeric(str_pad(NAICS, width = 6, side = 'right', pad = '0'))]
FernaldIT[, NAICSmax := as.numeric(str_pad(NAICS, width = 6, side = 'right', pad = '9'))]
setkey(FernaldIT, NAICSmin, NAICSmax)

write_feather(FernaldIT, output_files['FernaldITCodes'])