library(data.table)
library(foreach)
library(iterators)
library(stringr)
library(openxlsx)
library(readxl)
library(readr)

setwd("C:/Users/Nathan/Downloads/Compustat")

rbind_and_fill = function(...) rbind(...,fill=T)

getCharCols = function(x) {
  second_line = readLines(x,n = 2)[2]
  cols = strsplit(second_line, ',')[[1]]
  grep('"',cols)
}

fread_and_getCharCols = function(x) {
  fread(x, colClasses = list(character = getCharCols(x)))
}

dtcut = fread_and_getCharCols('IntermediateFiles/dtcut_for_spreadsheets.csv')


# companydata = readRDS('companydata.rds')
# dt = readRDS('fundamentalsannualdata.rds')
# SICtoNAICS_bls = readRDS('SIC1987toNAICS2002ratios_bls.rds')
# SICtoNAICS_bls[,sic:=as.numeric(sic)]
# SICtoNAICS_bls[,naics:=as.numeric(naics)]
# SICtoNAICS_bls[,sic_ratio:=sic_ratio/100]

# companydata[,sic:=as.numeric(sic)]
# companydata[,naics:=as.numeric(naics)]
# varnames = fread('CompustatVarnames.csv',header=F)
# setnames(varnames,c('combined','varname','varfull'))
# varnames[,lowervarname := tolower(varname)]
# merge = varnames[data.table(lowervarname =tolower(names(dt))),on = 'lowervarname',nomatch = 0]
# merge[, UpperCamel := gsub("[^[:alnum:]]","",varfull)]
# setnames(dt,merge$lowervarname,merge$UpperCamel)
# 
# dt[companydata,on=c(GlobalCompanyKey='gvkey'),`:=`(currentsic=i.sic,currentnaics=i.naics,loc=i.loc)]
# dt[,SIC:=StandardIndustrialClassificationHistorical]
# dt[is.na(SIC),SIC:=currentsic]
# dt[,NAICS:=NorthAmericaIndustrialClassificationSystemHistorical]
# dt[is.na(NAICS),NAICS:=currentnaics]
# 
# dt[,calendaryear:=year(datadate)]
# 
# 
# dtcut = dt[curcd=='USD'&!is.na(curcd)
#            &loc=='USA'
#            &indfmt=='INDL'
#            &consol=='C'
#            &datafmt=='STD'
#            #&(SIC<6000|SIC>6499)
#            &AssetsTotal!=0]
# 
# nrow(dtcut)
# dtcut[conm=='DELHAIZE AMERICA INC'&calendaryear==2001&CommonSharesOutstanding==91125.785,
#       CommonSharesOutstanding:=dtcut[conm=='DELHAIZE AMERICA INC'&calendaryear==2001]$CommonSharesOutstanding]
# dtcut[,MktVal:=MarketValueTotalFiscal]
# dtcut[is.na(MktVal),MktVal:=PriceCloseAnnualFiscal*CommonSharesOutstanding]
# dtcut[is.na(MktVal),MktVal:=PriceCloseAnnualCalendar*CommonSharesOutstanding]
# dtcut = dtcut[!is.na(MktVal)&MktVal>0]
# nrow(dtcut)
# #dtcut = dtcut[is.na(MktVal)|MktVal<=0]
# 
# dtcut[,haspreviousfiscalyear:=(DataYearFiscal-1)%in%DataYearFiscal,GlobalCompanyKey]
# dtcut[,hascalendaryear:=DataYearFiscal%in%calendaryear,GlobalCompanyKey]
# dtcut[,missing:=haspreviousfiscalyear&!hascalendaryear][,wasmissing:=0]
# missings = dtcut[missing==T]
# missings[,calendaryear:=DataYearFiscal][,wasmissing:=1]
# nrow(dtcut)
# dtcut = rbind(dtcut,missings)
# dtcut[,keep:=datadate==max(datadate),.(calendaryear,GlobalCompanyKey)]
# dtcut = dtcut[keep==T]
# nrow(dtcut)

# dtcut[is.na(NAICS),NAICS:=currentnaics]
# dtcut[,SIC:=as.integer(SIC)]
# missingSICvalues = setdiff(100*unique(dtcut[,as.numeric(twodigitsic)]),SICtoNAICS_bls$sic)

crosswalk97_02 = data.table(read_xls('Data/NAICS Crosswalks/1997_NAICS_to_2002_NAICS.xls', sheet = 2))[, NAICS97 := as.integer(NAICS97)]
crosswalk02_07 = data.table(read_xls('Data/NAICS Crosswalks/2002_to_2007_NAICS.xls', skip = 2))
crosswalk07_12 = data.table(read_xls('Data/NAICS Crosswalks/2007_to_2012_NAICS.xls', skip = 2))
crosswalk12_17 = data.table(read_xlsx('Data/NAICS Crosswalks/2012_to_2017_NAICS.xlsx', skip = 2))
crosswalk97_17 = crosswalk97_02[crosswalk02_07, on = c(NAICS02 = '2002 NAICS Code'),
                                `:=`(`2007 NAICS Code` = `i.2007 NAICS Code`, `2007 NAICS Title` = `i.2007 NAICS Title`)
                              ][crosswalk07_12, on = '2007 NAICS Code',
                                `:=`(`2012 NAICS Code` = `i.2012 NAICS Code`, `2012 NAICS Title` = `i.2012 NAICS Title`)
                              ][crosswalk12_17, on = '2012 NAICS Code',
                                `:=`(`2017 NAICS Code` = `i.2017 NAICS Code`, `2017 NAICS Title` = `i.2017 NAICS Title`)
                              ][, NAICS_year := 1997]
crosswalk02_17 = crosswalk02_07[crosswalk07_12, on = '2007 NAICS Code',
                                `:=`(`2012 NAICS Code` = `i.2012 NAICS Code`, `2012 NAICS Title` = `i.2012 NAICS Title`)
                              ][crosswalk12_17, on = '2012 NAICS Code',
                                `:=`(`2017 NAICS Code` = `i.2017 NAICS Code`, `2017 NAICS Title` = `i.2017 NAICS Title`)
                              ][, NAICS_year := 2002]
crosswalk07_17 = crosswalk07_12[crosswalk12_17, on = '2012 NAICS Code',
                                `:=`(`2017 NAICS Code` = `i.2017 NAICS Code`, `2017 NAICS Title` = `i.2017 NAICS Title`)
                              ][, NAICS_year := 2007]


crosswalk97_17[, is_unique_97 := .N == 1, NAICS97] #this is BROKEN
crosswalk02_17[, is_unique_02 := .N == 1, `2002 NAICS Code`]
crosswalk07_17[, is_unique_07 := .N == 1, `2007 NAICS Code`]
crosswalk12_17[, is_unique_12 := .N == 1, `2012 NAICS Code`]
setnames(crosswalk02_17, gsub("Title.*", "Title", names(crosswalk02_17)))
setnames(crosswalk07_17, gsub("Title.*", "Title", names(crosswalk07_17)))
setnames(crosswalk12_17, gsub("Title.*", "Title", names(crosswalk12_17)))

crosswalk97_17[crosswalk02_17, on = c(NAICS02 = '2002 NAICS Code'), is_unique_02 := i.is_unique_02]
crosswalk97_17[crosswalk07_17, on = '2007 NAICS Code', is_unique_07 := i.is_unique_07]
crosswalk97_17[crosswalk12_17, on = '2012 NAICS Code', is_unique_12 := i.is_unique_12]
crosswalk97_17[is_unique_97 == F,
               `:=`(startyear = 1997,
                    endyear = fifelse(is_unique_02 == T, 2002,
                                      fifelse(is_unique_07 == T, 2007,
                                              fifelse(is_unique_12 == T, 2012,
                                                      2017))))]
crosswalk97_17[endyear == 2002, `:=`(possible_NAICS = NAICS02, possible_NAICS_name = `2002 NAICS Title`)
             ][endyear == 2007, `:=`(possible_NAICS = `2007 NAICS Code`, possible_NAICS_name = `2007 NAICS Title`)
             ][endyear == 2012, `:=`(possible_NAICS = `2012 NAICS Code`, possible_NAICS_name = `2012 NAICS Title`)
             ][endyear == 2017, `:=`(possible_NAICS = `2017 NAICS Code`, possible_NAICS_name = `2017 NAICS Title`)
             ] #[, var_name := paste0('possible_NAICS', as.character(1:.N)), NAICS97]
crosswalk97_17 = unique(crosswalk97_17, by = c('NAICS97', 'possible_NAICS'))
hm97 = foreach(code = unique(crosswalk97_17[is_unique_97 == F, NAICS97]), .combine = rbind_and_fill)%do%{
  shallowcopy = dtcut[, .(DataYearFiscal, NAICS, GlobalCompanyKey)]
  shallowcopy[(DataYearFiscal == 1997 - 1 | DataYearFiscal == 1997 - 2) & NAICS == as.integer(code), was_in_industry := 1]
  shallowcopy[, followthese := sum(was_in_industry == 1, na.rm = T) > 0, GlobalCompanyKey]
  shallowcopy[followthese == T & DataYearFiscal >= 1997, found_new_naics := NAICS %in% crosswalk97_17[NAICS97 == code, possible_NAICS]
            ][, endyear := min(crosswalk97_17[NAICS97 == code, endyear])]
  # dtcut[found_new_naics == F, found_new_naics := NA]
  if(nrow(shallowcopy[found_new_naics == T]) == 0) {
    ratiotable = crosswalk97_17[NAICS97 == code, .(NAICS97, `1997 NAICS Title`, possible_NAICS, possible_NAICS_name, endyear)]
    setnames(ratiotable, 'NAICS97', 'starting_NAICS')
    setnames(ratiotable, 'possible_NAICS', 'NAICS')
    ratiotable[, ratio := 0.0 + (starting_NAICS == NAICS)]
    if(sum(ratiotable$ratio) < 1) ratiotable[, ratio := 0.0 + (`1997 NAICS Title` == possible_NAICS_name)]
    if(sum(ratiotable$ratio) < 1) ratiotable[, ratio := 1/.N]
  } else {
    shallowcopy[found_new_naics == T, is_modal_subsequent_NAICS := .N == max(.N), .(NAICS, GlobalCompanyKey)]
    shallowcopy[is_modal_subsequent_NAICS == T, is_really_modal_subsequent_NAICS := DataYearFiscal == min(DataYearFiscal), GlobalCompanyKey]
    dtcut[shallowcopy[is_really_modal_subsequent_NAICS == T, .(GlobalCompanyKey, NAICS, endyear)][, past_NAICS := code],
          on = c('GlobalCompanyKey', NAICS = 'past_NAICS'),
          `:=`(subsequent_NAICS = i.NAICS, subsequent_NAICS_endyear = i.endyear)]
    ratiotable = shallowcopy[is_really_modal_subsequent_NAICS == T, .(num_firms = .N, endyear = min(endyear)), NAICS #not actually min, just for aggregation. All endyear values should be the same
                           ][, ratio := num_firms/sum(num_firms, na.rm = T)
                           ][, starting_NAICS := code]
  }
  ratiotable[, .(starting_NAICS, NAICS, ratio, endyear)]
  # asdf = dtcut[, .(modal_subsequent_NAICS = as.integer(names(sort(table(NAICS * found_new_naics),decreasing=TRUE)[1]))), GlobalCompanyKey]
  # asdf[, .(num_firms = .N), modal_subsequent_NAICS][, ratio := num_firms/sum(num_firms, na.rm = T)]
}

hm97[endyear == 2002, NAICS17 := crosswalk02_17[is_unique_02 == T][hm97[endyear == 2002], on = c(`2002 NAICS Code` = 'NAICS'), `2017 NAICS Code`]]
hm97[endyear == 2007, NAICS17 := crosswalk07_17[is_unique_07 == T][hm97[endyear == 2007], on = c(`2007 NAICS Code` = 'NAICS'), `2017 NAICS Code`]]
hm97[endyear == 2012, NAICS17 := crosswalk12_17[is_unique_12 == T][hm97[endyear == 2012], on = c(`2012 NAICS Code` = 'NAICS'), `2017 NAICS Code`]]
hm97[endyear == 2017, NAICS17 := NAICS]
#then have to walk the deterministic paths of the crosswalk (is_unique == T)
deterministic = crosswalk97_17[is_unique_97 == T][, ratio := 1]
setnames(deterministic, '2017 NAICS Code', 'NAICS17')
setnames(deterministic, 'NAICS97', 'starting_NAICS')
full97 = rbind(deterministic[, .(starting_NAICS, NAICS17, ratio)], hm97[, .(starting_NAICS, NAICS17, ratio)])





crosswalk02_17[crosswalk07_17, on = '2007 NAICS Code', is_unique_07 := i.is_unique_07]
crosswalk02_17[crosswalk12_17, on = '2012 NAICS Code', is_unique_12 := i.is_unique_12]
crosswalk02_17[is_unique_02 == F,
               `:=`(startyear = 2002,
                    endyear = fifelse(is_unique_07 == T, 2007,
                                      fifelse(is_unique_12 == T, 2012,
                                              2017)))]
crosswalk02_17[endyear == 2007, `:=`(possible_NAICS = `2007 NAICS Code`, possible_NAICS_name = `2007 NAICS Title`)
                 ][endyear == 2012, `:=`(possible_NAICS = `2012 NAICS Code`, possible_NAICS_name = `2012 NAICS Title`)
                   ][endyear == 2017, `:=`(possible_NAICS = `2017 NAICS Code`, possible_NAICS_name = `2017 NAICS Title`)
                     ]
crosswalk02_17 = unique(crosswalk02_17, by = c('2002 NAICS Code', 'possible_NAICS'))
hm02 = foreach(code = unique(crosswalk02_17[is_unique_02 == F, `2002 NAICS Code`]), .combine = rbind_and_fill)%do%{
  shallowcopy = dtcut[, .(DataYearFiscal, NAICS, GlobalCompanyKey)]
  shallowcopy[(DataYearFiscal == 2002 - 1 | DataYearFiscal == 2002 - 2) & NAICS == as.integer(code), was_in_industry := 1]
  shallowcopy[, followthese := sum(was_in_industry == 1, na.rm = T) > 0, GlobalCompanyKey]
  shallowcopy[followthese == T & DataYearFiscal >= 2002, found_new_naics := NAICS %in% crosswalk02_17[`2002 NAICS Code` == code, possible_NAICS]
              ][, endyear := min(crosswalk02_17[`2002 NAICS Code` == code, endyear])]
  # dtcut[found_new_naics == F, found_new_naics := NA]
  if(nrow(shallowcopy[found_new_naics == T]) == 0) {
    ratiotable = crosswalk02_17[`2002 NAICS Code` == code, .(`2002 NAICS Code`, `2002 NAICS Title`, possible_NAICS, possible_NAICS_name, endyear)]
    setnames(ratiotable, '2002 NAICS Code', 'starting_NAICS')
    setnames(ratiotable, 'possible_NAICS', 'NAICS')
    ratiotable[, ratio := 0.0 + (starting_NAICS == NAICS)]
    if(sum(ratiotable$ratio) < 1) ratiotable[, ratio := 0.0 + (`2002 NAICS Title` == possible_NAICS_name)]
    if(sum(ratiotable$ratio) < 1) ratiotable[, ratio := 1/.N]
  } else {
    shallowcopy[found_new_naics == T, is_modal_subsequent_NAICS := .N == max(.N), .(NAICS, GlobalCompanyKey)]
    shallowcopy[is_modal_subsequent_NAICS == T, is_really_modal_subsequent_NAICS := DataYearFiscal == min(DataYearFiscal), GlobalCompanyKey]
    dtcut[shallowcopy[is_really_modal_subsequent_NAICS == T, .(GlobalCompanyKey, NAICS, endyear)][, past_NAICS := code],
          on = c('GlobalCompanyKey', NAICS = 'past_NAICS'),
          `:=`(subsequent_NAICS = i.NAICS, subsequent_NAICS_endyear = i.endyear)]
    ratiotable = shallowcopy[is_really_modal_subsequent_NAICS == T, .(num_firms = .N, endyear = min(endyear)), NAICS #not actually min, just for aggregation. All endyear values should be the same
                             ][, ratio := num_firms/sum(num_firms, na.rm = T)
                               ][, starting_NAICS := code]
  }
  ratiotable[, .(starting_NAICS, NAICS, ratio, endyear)]
  # asdf = dtcut[, .(modal_subsequent_NAICS = as.integer(names(sort(table(NAICS * found_new_naics),decreasing=TRUE)[1]))), GlobalCompanyKey]
  # asdf[, .(num_firms = .N), modal_subsequent_NAICS][, ratio := num_firms/sum(num_firms, na.rm = T)]
}

hm02[endyear == 2007, NAICS17 := crosswalk07_17[is_unique_07 == T][hm02[endyear == 2007], on = c(`2007 NAICS Code` = 'NAICS'), `2017 NAICS Code`]]
hm02[endyear == 2012, NAICS17 := crosswalk12_17[is_unique_12 == T][hm02[endyear == 2012], on = c(`2012 NAICS Code` = 'NAICS'), `2017 NAICS Code`]]
hm02[endyear == 2017, NAICS17 := NAICS]
#then have to walk the deterministic paths of the crosswalk (is_unique == T)
deterministic = crosswalk02_17[is_unique_02 == T][, ratio := 1]
setnames(deterministic, '2017 NAICS Code', 'NAICS17')
setnames(deterministic, '2002 NAICS Code', 'starting_NAICS')
full02 = rbind(deterministic[, .(starting_NAICS, NAICS17, ratio)], hm02[, .(starting_NAICS, NAICS17, ratio)])








crosswalk07_17[crosswalk12_17, on = '2012 NAICS Code', is_unique_12 := i.is_unique_12]
crosswalk07_17[is_unique_07 == F,
               `:=`(startyear = 2007,
                    endyear = fifelse(is_unique_12 == T, 2012,
                                      2017))]
crosswalk07_17[endyear == 2012, `:=`(possible_NAICS = `2012 NAICS Code`, possible_NAICS_name = `2012 NAICS Title`)
                 ][endyear == 2017, `:=`(possible_NAICS = `2017 NAICS Code`, possible_NAICS_name = `2017 NAICS Title`)
                   ]
crosswalk07_17 = unique(crosswalk07_17, by = c('2007 NAICS Code', 'possible_NAICS'))
hm07 = foreach(code = unique(crosswalk07_17[is_unique_07 == F, `2007 NAICS Code`]), .combine = rbind_and_fill)%do%{
  shallowcopy = dtcut[, .(DataYearFiscal, NAICS, GlobalCompanyKey)]
  shallowcopy[(DataYearFiscal == 2007 - 1 | DataYearFiscal == 2007 - 2) & NAICS == as.integer(code), was_in_industry := 1]
  shallowcopy[, followthese := sum(was_in_industry == 1, na.rm = T) > 0, GlobalCompanyKey]
  shallowcopy[followthese == T & DataYearFiscal >= 2007, found_new_naics := NAICS %in% crosswalk07_17[`2007 NAICS Code` == code, possible_NAICS]
              ][, endyear := min(crosswalk07_17[`2007 NAICS Code` == code, endyear])]
  # dtcut[found_new_naics == F, found_new_naics := NA]
  if(nrow(shallowcopy[found_new_naics == T]) == 0) {
    ratiotable = crosswalk07_17[`2007 NAICS Code` == code, .(`2007 NAICS Code`, `2007 NAICS Title`, possible_NAICS, possible_NAICS_name, endyear)]
    setnames(ratiotable, '2007 NAICS Code', 'starting_NAICS')
    setnames(ratiotable, 'possible_NAICS', 'NAICS')
    ratiotable[, ratio := 0.0 + (starting_NAICS == NAICS)]
    if(sum(ratiotable$ratio) < 1) ratiotable[, ratio := 0.0 + (`2007 NAICS Title` == possible_NAICS_name)]
    if(sum(ratiotable$ratio) < 1) ratiotable[, ratio := 1/.N]
  } else {
    shallowcopy[found_new_naics == T, is_modal_subsequent_NAICS := .N == max(.N), .(NAICS, GlobalCompanyKey)]
    shallowcopy[is_modal_subsequent_NAICS == T, is_really_modal_subsequent_NAICS := DataYearFiscal == min(DataYearFiscal), GlobalCompanyKey]
    dtcut[shallowcopy[is_really_modal_subsequent_NAICS == T, .(GlobalCompanyKey, NAICS, endyear)][, past_NAICS := code],
          on = c('GlobalCompanyKey', NAICS = 'past_NAICS'),
          `:=`(subsequent_NAICS = i.NAICS, subsequent_NAICS_endyear = i.endyear)]
    ratiotable = shallowcopy[is_really_modal_subsequent_NAICS == T, .(num_firms = .N, endyear = min(endyear)), NAICS #not actually min, just for aggregation. All endyear values should be the same
                             ][, ratio := num_firms/sum(num_firms, na.rm = T)
                               ][, starting_NAICS := code]
  }
  ratiotable[, .(starting_NAICS, NAICS, ratio, endyear)]
  # asdf = dtcut[, .(modal_subsequent_NAICS = as.integer(names(sort(table(NAICS * found_new_naics),decreasing=TRUE)[1]))), GlobalCompanyKey]
  # asdf[, .(num_firms = .N), modal_subsequent_NAICS][, ratio := num_firms/sum(num_firms, na.rm = T)]
}

hm07[endyear == 2012, NAICS17 := crosswalk12_17[is_unique_12 == T][hm07[endyear == 2012], on = c(`2012 NAICS Code` = 'NAICS'), `2017 NAICS Code`]]
hm07[endyear == 2017, NAICS17 := NAICS]
#then have to walk the deterministic paths of the crosswalk (is_unique == T)
deterministic = crosswalk07_17[is_unique_07 == T][, ratio := 1]
setnames(deterministic, '2017 NAICS Code', 'NAICS17')
setnames(deterministic, '2007 NAICS Code', 'starting_NAICS')
full07 = rbind(deterministic[, .(starting_NAICS, NAICS17, ratio)], hm07[, .(starting_NAICS, NAICS17, ratio)])






crosswalk12_17[is_unique_12 == F,
               `:=`(startyear = 2012,
                    endyear = 2017)]
crosswalk12_17[endyear == 2017, `:=`(possible_NAICS = `2017 NAICS Code`, possible_NAICS_name = `2017 NAICS Title`)]
crosswalk12_17 = unique(crosswalk12_17, by = c('2012 NAICS Code', 'possible_NAICS'))
hm12 = foreach(code = unique(crosswalk12_17[is_unique_12 == F, `2012 NAICS Code`]), .combine = rbind_and_fill)%do%{
  shallowcopy = dtcut[, .(DataYearFiscal, NAICS, GlobalCompanyKey)]
  shallowcopy[(DataYearFiscal == 2012 - 1 | DataYearFiscal == 2012 - 2) & NAICS == as.integer(code), was_in_industry := 1]
  shallowcopy[, followthese := sum(was_in_industry == 1, na.rm = T) > 0, GlobalCompanyKey]
  shallowcopy[followthese == T & DataYearFiscal >= 2012, found_new_naics := NAICS %in% crosswalk12_17[`2012 NAICS Code` == code, possible_NAICS]
              ][, endyear := min(crosswalk12_17[`2012 NAICS Code` == code, endyear])]
  # dtcut[found_new_naics == F, found_new_naics := NA]
  if(nrow(shallowcopy[found_new_naics == T]) == 0) {
    ratiotable = crosswalk12_17[`2012 NAICS Code` == code, .(`2012 NAICS Code`, `2012 NAICS Title`, possible_NAICS, possible_NAICS_name, endyear)]
    setnames(ratiotable, '2012 NAICS Code', 'starting_NAICS')
    setnames(ratiotable, 'possible_NAICS', 'NAICS')
    ratiotable[, ratio := 0.0 + (starting_NAICS == NAICS)]
    if(sum(ratiotable$ratio) < 1) ratiotable[, ratio := 0.0 + (`2012 NAICS Title` == possible_NAICS_name)]
    if(sum(ratiotable$ratio) < 1) ratiotable[, ratio := 1/.N]
  } else {
    shallowcopy[found_new_naics == T, is_modal_subsequent_NAICS := .N == max(.N), .(NAICS, GlobalCompanyKey)]
    shallowcopy[is_modal_subsequent_NAICS == T, is_really_modal_subsequent_NAICS := DataYearFiscal == min(DataYearFiscal), GlobalCompanyKey]
    dtcut[shallowcopy[is_really_modal_subsequent_NAICS == T, .(GlobalCompanyKey, NAICS, endyear)][, past_NAICS := code],
          on = c('GlobalCompanyKey', NAICS = 'past_NAICS'),
          `:=`(subsequent_NAICS = i.NAICS, subsequent_NAICS_endyear = i.endyear)]
    ratiotable = shallowcopy[is_really_modal_subsequent_NAICS == T, .(num_firms = .N, endyear = min(endyear)), NAICS #not actually min, just for aggregation. All endyear values should be the same
                             ][, ratio := num_firms/sum(num_firms, na.rm = T)
                               ][, starting_NAICS := code]
  }
  ratiotable[, .(starting_NAICS, NAICS, ratio, endyear)]
  # asdf = dtcut[, .(modal_subsequent_NAICS = as.integer(names(sort(table(NAICS * found_new_naics),decreasing=TRUE)[1]))), GlobalCompanyKey]
  # asdf[, .(num_firms = .N), modal_subsequent_NAICS][, ratio := num_firms/sum(num_firms, na.rm = T)]
}

hm12[endyear == 2017, NAICS17 := NAICS]
#then have to walk the deterministic paths of the crosswalk (is_unique == T)
deterministic = crosswalk12_17[is_unique_12 == T][, ratio := 1]
setnames(deterministic, '2017 NAICS Code', 'NAICS17')
setnames(deterministic, '2012 NAICS Code', 'starting_NAICS')
full12 = rbind(deterministic[, .(starting_NAICS, NAICS17, ratio)], hm12[, .(starting_NAICS, NAICS17, ratio)])


dtcut[subsequent_NAICS_endyear == 2002, subsequent_NAICS17 := crosswalk02_17[is_unique_02 == T][dtcut[subsequent_NAICS_endyear == 2002], on = c(`2002 NAICS Code` = 'subsequent_NAICS'), `2017 NAICS Code`]]
dtcut[subsequent_NAICS_endyear == 2007, subsequent_NAICS17 := crosswalk07_17[is_unique_07 == T][dtcut[subsequent_NAICS_endyear == 2007], on = c(`2007 NAICS Code` = 'subsequent_NAICS'), `2017 NAICS Code`]]
dtcut[subsequent_NAICS_endyear == 2012, subsequent_NAICS17 := crosswalk12_17[is_unique_12 == T][dtcut[subsequent_NAICS_endyear == 2012], on = c(`2012 NAICS Code` = 'subsequent_NAICS'), `2017 NAICS Code`]]
dtcut[subsequent_NAICS_endyear == 2017, subsequent_NAICS17 := subsequent_NAICS]
dtcut[!is.na(subsequent_NAICS17), NAICS := subsequent_NAICS17]

fullyfull97 = rbind(full97,
                    full02[!starting_NAICS %in% full97$starting_NAICS],
                    full07[!starting_NAICS %in% full97$starting_NAICS & !starting_NAICS %in% full02$starting_NAICS],
                    full12[!starting_NAICS %in% full97$starting_NAICS & !starting_NAICS %in% full02$starting_NAICS & !starting_NAICS %in% full07$starting_NAICS]
                  )[ratio != 0][, `:=`(applicable_beginning = 1888, applicable_ending = 2001)]
fullyfull02 = rbind(full02,
                    full97[!starting_NAICS %in% full02$starting_NAICS],
                    full07[!starting_NAICS %in% full02$starting_NAICS & !starting_NAICS %in% full97$starting_NAICS],
                    full12[!starting_NAICS %in% full02$starting_NAICS & !starting_NAICS %in% full97$starting_NAICS & !starting_NAICS %in% full07$starting_NAICS]
                  )[ratio != 0][, `:=`(applicable_beginning = 2002, applicable_ending = 2006)]
fullyfull07 = rbind(full07,
                    full97[!starting_NAICS %in% full07$starting_NAICS],
                    full02[!starting_NAICS %in% full07$starting_NAICS & !starting_NAICS %in% full97$starting_NAICS],
                    full12[!starting_NAICS %in% full07$starting_NAICS & !starting_NAICS %in% full97$starting_NAICS & !starting_NAICS %in% full02$starting_NAICS]
                  )[ratio != 0][, `:=`(applicable_beginning = 2007, applicable_ending = 2011)]
fullyfull12 = rbind(full12,
               full97[!starting_NAICS %in% full12$starting_NAICS],
               full02[!starting_NAICS %in% full12$starting_NAICS & !starting_NAICS %in% full97$starting_NAICS],
               full07[!starting_NAICS %in% full12$starting_NAICS & !starting_NAICS %in% full97$starting_NAICS & !starting_NAICS %in% full02$starting_NAICS]
              )[ratio != 0][, `:=`(applicable_beginning = 2012, applicable_ending = 2016)]

# full97[ratio != 0][, applicable_beginning := 1888][full02[ratio != 0], on = ][, .(ratio = sum(ratio)), .(starting_NAICS, NAICS17, applicable_beginning]

defunct_NAICS_crosswalk = rbind(fullyfull97, fullyfull02, fullyfull07, fullyfull12
                              )[, .(ratio = sum(ratio)), .(starting_NAICS, NAICS17, applicable_beginning, applicable_ending)]
setkey(defunct_NAICS_crosswalk, starting_NAICS, applicable_beginning, applicable_ending)




# crosswalk97_17_for_real = dcast(crosswalk97_17[is_unique_97 == F, .(NAICS97, possible_NAICS, startyear, endyear, var_name)],
#                                 NAICS97 + startyear + endyear ~ var_name, value.var = 'possible_NAICS')[, NAICS97 := as.integer(NAICS97)]
# dtcut[crosswalk97_17_for_real, on = c(NAICS = 'NAICS97', DataYearFiscal = 'startyear'), roll = -1, `:=`(possible_NAICS1 = i.possible_NAICS1)]
# dtcut[dtcut[sum(!is.na(possible_NAICS1)) > 0 & DataYearFiscal == min(DataYearFiscal), .SD, GlobalCompanyKey],
#       on = c('GlobalCompanyKey', 'DataYearFiscal'), roll = T, `:=`(possible_NAICS1 = i.possible_NAICS1)]
# asdf = dtcut[NAICS %in% possible_NAICS1][year == min(year), .SD, GlobalCompanyKey]
# 
# defunct_NAICS_97 = crosswalk97_02[!NAICS97%in%crosswalk12_17$`2017 NAICS Code`, NAICS97]
# defunct_NAICS_crosswalks = rbind(crosswalk97_17, crosswalk02_17, crosswalk07_17,
#                                  crosswalk12_17[, NAICS_year := 2012])



dtcut[,                                                       two_digit_sic := floor(SIC/100)*100]
dtcut[substr(as.character(SIC),nchar(SIC)-1,nchar(SIC)-1)!=0, three_digit_sic := floor(SIC/10)*10]
dtcut[substr(as.character(SIC),nchar(SIC)-0,nchar(SIC)-0)!=0, four_digit_sic := floor(SIC/1)*1]
dt_for_crosswalk = dtcut[!is.na(SIC)&!is.na(NAICS)]
dt_for_crosswalk = dt_for_crosswalk[
  dt_for_crosswalk[,.(ind = .I[which.min(calendaryear)]),.(GlobalCompanyKey,SIC)]$ind,
  .(two_digit_sic,three_digit_sic,four_digit_sic,NAICS)]

possibleSICvalues = na.omit(unique(c(dtcut$two_digit_sic,dtcut$three_digit_sic,dtcut$four_digit_sic)))

SICtoNAICS = foreach(val=possibleSICvalues,.combine = rbind)%do%{
  if(val>=9900) {
    customcrosswalk = data.table(NAICS=999990,numfirms = NA_integer_,sic=val,sic_ratio=1)
  } else {
    customcrosswalk = dt_for_crosswalk[two_digit_sic==val | three_digit_sic==val | four_digit_sic==val,
                                       .(numfirms = .N),NAICS]
    customcrosswalk[,sic:=val][,sic_ratio:=numfirms/sum(numfirms)]
  }
  customcrosswalk
}
setnames(SICtoNAICS,'NAICS','naics')
SICtoNAICS[,numfirms:=NULL]
# additional_SICs = rbind(additional_SICs,data.table(sic=9900,naics=999990,sic_ratio=1))
# SICtoNAICS = rbind(SICtoNAICS_bls,additional_SICs,fill=T)

# suppressWarnings(dtcut[,sic:=NULL])
missingNAICS = dtcut[is.na(NAICS)]
missingNAICS[SIC%in%SICtoNAICS$sic,sic:=SIC]
missingNAICS[is.na(sic) & (SIC - SIC %% 10) %in% SICtoNAICS$sic, sic := SIC - SIC %% 10]
missingNAICS[is.na(sic) & (SIC - SIC %% 100) %in% SICtoNAICS$sic, sic := SIC - SIC %% 100]
# setkey(dtcut,sic)
# setkey(SICtoNAICS,sic)
# rbind(missingNAICS[SICtoNAICS,eval(.(names...)),by=.EACHI],missingNAICS[sic>9000])
addingNAICS = merge(missingNAICS,SICtoNAICS,by='sic',all.x = T,allow.cartesian=T)
addingNAICS[,NAICSadded:=1]
# addingNAICS[,`:=`(`CES SIC Tabulating Code`=NULL,
#                   `SIC Industry`=NULL,
#                   `CES NAICS Tabulating Code`=NULL,
#                   `NAICS Industry`=NULL,
#                   `SIC to NAICS Employment Ratio`=NULL)]
setnames(addingNAICS,'naics','imputed_NAICS')
withNAICS = rbind(dtcut,addingNAICS,fill=T)
withNAICS[is.na(NAICS)&is.na(NAICSadded),sic_ratio:=0]
#why did I do this? I left all the NA NAICS firms in there, with ratio equal to 0.
#I do this the whole time. It's so confusing and bad.
withNAICS[,realfirm:=is.na(NAICSadded)]
withNAICS[!is.na(NAICS)&is.na(NAICSadded),sic_ratio:=1]


# rbind(withNAICS[DataYearFiscal < 1997 | !is.na(subsequent_NAICS)],
#       withNAICS[DataYearFiscal >= 1997 & DataYearFiscal < 2002],
#       withNAICS[DataYearFiscal >= 2002 & DataYearFiscal < 2007],
#       withNAICS[DataYearFiscal >= 2007 & DataYearFiscal < 2012],
#       withNAICS[DataYearFiscal >= 2012])

withNAICS[, duplicate_fiscal_year1 := DataYearFiscal][, duplicate_fiscal_year2 := DataYearFiscal]
setkey(withNAICS, NAICS, duplicate_fiscal_year1, duplicate_fiscal_year2)

whathappenshere = foverlaps(withNAICS[is.na(subsequent_NAICS)], defunct_NAICS_crosswalk)
whathappenshere[!is.na(ratio), `:=`(NAICS = NAICS17, sic_ratio = sic_ratio * ratio)]
withNAICS = rbind_and_fill(whathappenshere, withNAICS[!is.na(subsequent_NAICS)])

withNAICS[!is.na(NAICS),true_NAICS:=NAICS][is.na(NAICS),true_NAICS:=imputed_NAICS]
withNAICS[,true_NAICS:=as.numeric(str_pad(true_NAICS, width=6, side='right', pad='0'))]

withNAICS[,two_digit_NAICS:=true_NAICS-true_NAICS%%10000]


withNAICS[true_NAICS%%10000!=0,three_digit_NAICS:=true_NAICS-true_NAICS%%1000]
missing_three_digit_NAICS = withNAICS[true_NAICS%%10000==0]
two_digit_to_three = unique(withNAICS,by=c('GlobalCompanyKey','two_digit_NAICS'))[
  !is.na(three_digit_NAICS)&realfirm==T,
  .(numfirms = .N,two_digit_NAICS=median(two_digit_NAICS)),
  by = three_digit_NAICS]
two_digit_to_three[,two_to_three_ratio:=numfirms/sum(numfirms),two_digit_NAICS]
setnames(two_digit_to_three,'three_digit_NAICS','imputed_three_digit_NAICS')
two_digit_to_three[,numfirms:=NULL]
addingThreeDigit = merge(missing_three_digit_NAICS,two_digit_to_three,by='two_digit_NAICS',all.x = T,allow.cartesian=T)
addingThreeDigit[,ThreeDigitadded:=1]
addingThreeDigit[,three_digit_ratio := two_to_three_ratio*sic_ratio]
withThreeDigit = rbind(withNAICS,addingThreeDigit,fill=T)
withThreeDigit[is.na(three_digit_NAICS)&is.na(ThreeDigitadded),three_digit_ratio:=0]
withThreeDigit[,realfirm:=is.na(NAICSadded)&is.na(ThreeDigitadded)]

withThreeDigit[!is.na(three_digit_NAICS)&is.na(ThreeDigitadded),three_digit_ratio:=sic_ratio]
withThreeDigit[!is.na(three_digit_NAICS),true_three_digit_NAICS:=three_digit_NAICS][is.na(three_digit_NAICS),true_three_digit_NAICS:=imputed_three_digit_NAICS]

fwrite(withThreeDigit,'IntermediateFiles/withThreeDigit.csv', quote = T)

withThreeDigit[true_NAICS%%1000!=0,four_digit_NAICS:=true_NAICS-true_NAICS%%100]
missing_four_digit_NAICS = withThreeDigit[true_NAICS%%1000==0]
three_digit_to_four = unique(withThreeDigit,by=c('GlobalCompanyKey','NAICS'))[!is.na(four_digit_NAICS)&realfirm==T,
                                                                              .(numfirms = .N,true_three_digit_NAICS=median(true_three_digit_NAICS)),
                                                                              by = four_digit_NAICS]
three_digit_to_four[,three_to_four_ratio:=numfirms/sum(numfirms),true_three_digit_NAICS]
setnames(three_digit_to_four,'four_digit_NAICS','imputed_four_digit_NAICS')
addingFourDigit = merge(missing_four_digit_NAICS,three_digit_to_four[,numfirms:=NULL],by='true_three_digit_NAICS',all.x = T,allow.cartesian=T)
addingFourDigit[,FourDigitadded:=1]
addingFourDigit[,four_digit_ratio := three_to_four_ratio*three_digit_ratio]
withFourDigit = rbind(withThreeDigit,addingFourDigit,fill=T)

withFourDigit = withFourDigit[!is.na(four_digit_NAICS)|!is.na(imputed_four_digit_NAICS)]

withFourDigit[is.na(four_digit_NAICS)&is.na(FourDigitadded),four_digit_ratio:=0]
withFourDigit[,realfirm:=is.na(NAICSadded)&is.na(ThreeDigitadded)&is.na(FourDigitadded)]
withFourDigit[!is.na(four_digit_NAICS)&is.na(FourDigitadded), four_digit_ratio := three_digit_ratio]
withFourDigit[!is.na(four_digit_NAICS),true_four_digit_NAICS:=four_digit_NAICS][is.na(four_digit_NAICS),true_four_digit_NAICS:=imputed_four_digit_NAICS]


withFourDigit[true_NAICS%%100!=0,five_digit_NAICS:=true_NAICS-true_NAICS%%10]
missing_five_digit_NAICS = withFourDigit[true_NAICS%%100==0]
four_digit_to_five = unique(withFourDigit,by=c('GlobalCompanyKey','NAICS'))[!is.na(five_digit_NAICS)&realfirm==T,
                                                                              .(numfirms = .N,true_four_digit_NAICS=median(true_four_digit_NAICS)),
                                                                              by = five_digit_NAICS]
four_digit_to_five[,four_to_five_ratio:=numfirms/sum(numfirms),true_four_digit_NAICS]
setnames(four_digit_to_five,'five_digit_NAICS','imputed_five_digit_NAICS')
addingFiveDigit = merge(missing_five_digit_NAICS,four_digit_to_five[,numfirms:=NULL],by='true_four_digit_NAICS',all.x = T,allow.cartesian=T)
addingFiveDigit[,FiveDigitadded:=1]
addingFiveDigit[,five_digit_ratio := four_to_five_ratio*four_digit_ratio]
withFiveDigit = rbind(withFourDigit,addingFiveDigit,fill=T)

withFiveDigit = withFiveDigit[!is.na(five_digit_NAICS)|!is.na(imputed_five_digit_NAICS)]

withFiveDigit[is.na(five_digit_NAICS)&is.na(FiveDigitadded),five_digit_ratio:=0]
withFiveDigit[,realfirm:=is.na(NAICSadded)&is.na(ThreeDigitadded)&is.na(FourDigitadded)&is.na(FiveDigitadded)]
withFiveDigit[!is.na(five_digit_NAICS)&is.na(FiveDigitadded), five_digit_ratio := four_digit_ratio]
withFiveDigit[!is.na(five_digit_NAICS),true_five_digit_NAICS:=five_digit_NAICS][is.na(five_digit_NAICS),true_five_digit_NAICS:=imputed_five_digit_NAICS]


# adds one firm to each six digit industry, to prevent missing values in the crosswalk
# Not actually sure that there's a missing values issue.
# Might just be adding stuff like "management of companies and enterprises"
# that never gets used to code firms in Compustat.
six_2017 = data.table(read.xlsx('Data/Six Digit NAICS Codes/6-digit_2017_Codes.xlsx'))[,1:2]
six_2012 = data.table(read_xls('Data/Six Digit NAICS Codes/6-digit_2012_Codes.xls'))
six_2007 = data.table(read_xls('Data/Six Digit NAICS Codes/naics07_6.xls'))
filename = 'Data/Six Digit NAICS Codes/naics_6_02.txt'
six_2002 = data.table(read_fwf(filename,
                               fwf_empty(filename,
                                         col_names = c('Code','2002 NAICS Title')
                               ),
                               skip = 1)
)[!is.na(Code)]

setnames(six_2017,c('Code','Title'))
setnames(six_2012,c('Code','Title'))
setnames(six_2007,c('Code','Title'))
setnames(six_2002,c('Code','Title'))
all_NAICS_Codes = unique(rbind(six_2017, six_2012, six_2007, six_2002), by = 'Code')
dummy_firms = all_NAICS_Codes[nchar(Code) == 6]
dummy_firms[,
  six_digit_NAICS := as.numeric(Code)
][,
  true_five_digit_NAICS := as.numeric(paste0(substr(Code, 1, 5), '0'))
][,
  dummyfirm := T
]

#some five digit codes that end in 0 are valid six digit codes.
# Like there's nothing more specific than 488210 Support Activities for Rail Transportation.
# Also, some years codes only go to 5 digits, like 517210 Wireless Telecommunications Carriers (except Satellite),
# which sometimes has the subcategories 517211 and 517212.
# I'm breaking it down into those subcodes every year, even when they aren't in the official NAICS
all_six_digit_codes = data.table(six = unique(c(dummy_firms$six_digit_NAICS,
                                                unique(withFiveDigit$true_NAICS))
                                              ))
all_six_digit_codes = all_six_digit_codes[six %% 100 != 0 ]
all_six_digit_codes[, five := six-six%%10]
all_six_digit_codes[, n_subcodes := .N - 1, five]
all_six_digit_codes = all_six_digit_codes[n_subcodes == 0 | substr(six, 6, 6) != 0]

withFiveDigit[true_NAICS%in%all_six_digit_codes$six,six_digit_NAICS:=true_NAICS]
missing_six_digit_NAICS = withFiveDigit[!true_NAICS%in%all_six_digit_codes$six]
five_digit_to_six = rbind(unique(withFiveDigit,by=c('GlobalCompanyKey','NAICS')),
                          dummy_firms,
                          fill = T
                          )[!is.na(six_digit_NAICS)&(realfirm==T|dummyfirm == T),
                                    .(numfirms = .N,true_five_digit_NAICS=median(true_five_digit_NAICS)),
                                    by = six_digit_NAICS]
five_digit_to_six[,five_to_six_ratio:=numfirms/sum(numfirms),true_five_digit_NAICS]
setnames(five_digit_to_six,'six_digit_NAICS','imputed_six_digit_NAICS')
addingSixDigit = merge(missing_six_digit_NAICS,five_digit_to_six[,numfirms:=NULL],by='true_five_digit_NAICS',all.x = T,allow.cartesian=T)
addingSixDigit[,SixDigitadded:=1]
addingSixDigit[,six_digit_ratio := five_to_six_ratio*five_digit_ratio]
withSixDigit = rbind(withFiveDigit,addingSixDigit,fill=T)

withSixDigit = withSixDigit[!is.na(six_digit_NAICS)|!is.na(imputed_six_digit_NAICS)]

withSixDigit[is.na(six_digit_NAICS)&is.na(SixDigitadded),six_digit_ratio:=0]
withSixDigit[,realfirm:=is.na(NAICSadded)&is.na(ThreeDigitadded)&is.na(FourDigitadded)&is.na(FiveDigitadded)&is.na(SixDigitadded)]
withSixDigit[!is.na(six_digit_NAICS)&is.na(SixDigitadded), six_digit_ratio := five_digit_ratio]
withSixDigit[!is.na(six_digit_NAICS),true_six_digit_NAICS:=six_digit_NAICS
           ][is.na(six_digit_NAICS),true_six_digit_NAICS:=imputed_six_digit_NAICS]

fwrite(withSixDigit,'IntermediateFiles/withSixDigit.csv', quote = T)
