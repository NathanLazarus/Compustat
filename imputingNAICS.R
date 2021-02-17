input_data = c(dtcut = 'IntermediateFiles/dtcut.feather', 
               oldNAICSTo2017Crosswalk = 'IntermediateFiles/Old NAICS to 2017 Crosswalk.feather', 
               NAICSCodes2017 = 'Data/Six Digit NAICS Codes/6-digit_2017_Codes.xlsx', 
               NAICSCodes2012 = 'Data/Six Digit NAICS Codes/6-digit_2012_Codes.xls', 
               NAICSCodes2007 = 'Data/Six Digit NAICS Codes/naics07_6.xls', 
               NAICSCodes2002 = 'Data/Six Digit NAICS Codes/naics_6_02.txt')

output_files = c(withThreeDigit = 'IntermediateFiles/withThreeDigit.feather', 
                 withSixDigit = 'IntermediateFiles/withSixDigit.feather')

dtcut = read_feather_dt(input_data['dtcut'])



# companydata = readRDS('companydata.rds')
# dt = readRDS('fundamentalsannualdata.rds')
# SICtoNAICS_bls = readRDS('SIC1987toNAICS2002ratios_bls.rds')
# SICtoNAICS_bls[, sic := as.numeric(sic)]
# SICtoNAICS_bls[, naics := as.numeric(naics)]
# SICtoNAICS_bls[, sic_ratio := sic_ratio/100]

# companydata[, sic := as.numeric(sic)]
# companydata[, naics := as.numeric(naics)]
# varnames = fread('CompustatVarnames.csv', header = F)
# setnames(varnames, c('combined', 'varname', 'varfull'))
# varnames[, lowervarname := tolower(varname)]
# merge = varnames[data.table(lowervarname =tolower(names(dt))), on = 'lowervarname', nomatch = 0]
# merge[, UpperCamel := gsub("[^[:alnum:]]", "", varfull)]
# setnames(dt, merge$lowervarname, merge$UpperCamel)
# 
# dt[companydata, on = c(GlobalCompanyKey = 'gvkey'), `:=`(currentsic = i.sic, currentnaics = i.naics, loc = i.loc)]
# dt[, SIC := StandardIndustrialClassificationHistorical]
# dt[is.na(SIC), SIC := currentsic]
# dt[, NAICS := NorthAmericaIndustrialClassificationSystemHistorical]
# dt[is.na(NAICS), NAICS := currentnaics]
# 
# dt[, calendaryear := year(datadate)]
# 
# 
# dtcut = dt[curcd == 'USD' & !is.na(curcd)
#            & loc == 'USA'
#            & indfmt == 'INDL'
#            & consol == 'C'
#            & datafmt == 'STD'
#            #& (SIC<6000|SIC>6499)
#            & AssetsTotal != 0]
# 
# nrow(dtcut)
# dtcut[conm == 'DELHAIZE AMERICA INC'&calendaryear == 2001&CommonSharesOutstanding == 91125.785, 
#       CommonSharesOutstanding := dtcut[conm == 'DELHAIZE AMERICA INC'&calendaryear == 2001]$CommonSharesOutstanding]
# dtcut[, MktVal := MarketValueTotalFiscal]
# dtcut[is.na(MktVal), MktVal := PriceCloseAnnualFiscal*CommonSharesOutstanding]
# dtcut[is.na(MktVal), MktVal := PriceCloseAnnualCalendar*CommonSharesOutstanding]
# dtcut = dtcut[!is.na(MktVal)&MktVal>0]
# nrow(dtcut)
# #dtcut = dtcut[is.na(MktVal)|MktVal<=0]
# 
# dtcut[, haspreviousfiscalyear := (DataYearFiscal-1) %in% DataYearFiscal, GlobalCompanyKey]
# dtcut[, hascalendaryear := DataYearFiscal %in% calendaryear, GlobalCompanyKey]
# dtcut[, missing := haspreviousfiscalyear&!hascalendaryear][, wasmissing := 0]
# missings = dtcut[missing == T]
# missings[, calendaryear := DataYearFiscal][, wasmissing := 1]
# nrow(dtcut)
# dtcut = rbind(dtcut, missings)
# dtcut[, keep := datadate == max(datadate), .(calendaryear, GlobalCompanyKey)]
# dtcut = dtcut[keep == T]
# nrow(dtcut)

# dtcut[is.na(NAICS), NAICS := currentnaics]
# dtcut[, SIC := as.integer(SIC)]
# missingSICvalues = setdiff(100*unique(dtcut[, as.numeric(twodigitsic)]), SICtoNAICS_bls$sic)



dtcut[,                                                       two_digit_sic := floor(SIC/100)*100]
dtcut[substr(as.character(SIC), nchar(SIC) - 1, nchar(SIC) - 1) != 0, three_digit_sic := floor(SIC/10)*10]
dtcut[substr(as.character(SIC), nchar(SIC) - 0, nchar(SIC) - 0) != 0, four_digit_sic := floor(SIC/1)*1]
dt_for_crosswalk = dtcut[!is.na(SIC) & !is.na(NAICS)]
dt_for_crosswalk = dt_for_crosswalk[
  dt_for_crosswalk[, .(ind = .I[which.min(calendaryear)]), .(GlobalCompanyKey, SIC)]$ind, 
  .(two_digit_sic, three_digit_sic, four_digit_sic, NAICS)]

possibleSICvalues = na.omit(unique(c(dtcut$two_digit_sic, dtcut$three_digit_sic, dtcut$four_digit_sic)))

SICtoNAICS = foreach(val = possibleSICvalues, .combine = rbind) %do% {
  if (val >= 9900) {
    customcrosswalk = data.table(NAICS = 999990, numfirms = NA_integer_, sic = val, sic_ratio = 1)
  } else {
    customcrosswalk = dt_for_crosswalk[two_digit_sic == val | three_digit_sic == val | four_digit_sic == val, 
                                       .(numfirms = .N), NAICS]
    customcrosswalk[, sic := val][, sic_ratio := numfirms/sum(numfirms)]
  }
  customcrosswalk
}
setnames(SICtoNAICS, 'NAICS', 'naics')
SICtoNAICS[, numfirms := NULL]
# additional_SICs = rbind(additional_SICs, data.table(sic = 9900, naics = 999990, sic_ratio = 1))
# SICtoNAICS = rbind(SICtoNAICS_bls, additional_SICs, fill = T)

# suppressWarnings(dtcut[, sic := NULL])
missingNAICS = dtcut[is.na(NAICS)]
missingNAICS[SIC %in% SICtoNAICS$sic, sic := SIC]
missingNAICS[is.na(sic) & (SIC - SIC %% 10) %in% SICtoNAICS$sic, sic := SIC - SIC %% 10]
missingNAICS[is.na(sic) & (SIC - SIC %% 100) %in% SICtoNAICS$sic, sic := SIC - SIC %% 100]
# setkey(dtcut, sic)
# setkey(SICtoNAICS, sic)
# rbind(missingNAICS[SICtoNAICS, eval(.(names...)), by = .EACHI], missingNAICS[sic>9000])
addingNAICS = merge(missingNAICS, SICtoNAICS, by = 'sic', all.x = T, allow.cartesian = T)
addingNAICS[, NAICSadded := 1]
# addingNAICS[, `:=`(`CES SIC Tabulating Code`=NULL, 
#                   `SIC Industry`=NULL, 
#                   `CES NAICS Tabulating Code`=NULL, 
#                   `NAICS Industry`=NULL, 
#                   `SIC to NAICS Employment Ratio`=NULL)]
setnames(addingNAICS, 'naics', 'imputed_NAICS')
withNAICS = rbind(dtcut, addingNAICS, fill = T)
withNAICS[is.na(NAICS) & is.na(NAICSadded), sic_ratio := 0]
#why did I do this? I left all the NA NAICS firms in there, with ratio equal to 0.
#I do this the whole time. It's so confusing and bad.
withNAICS[, realfirm := is.na(NAICSadded)]
withNAICS[!is.na(NAICS) & is.na(NAICSadded), sic_ratio := 1]


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

withNAICS[!is.na(NAICS), true_NAICS := NAICS][is.na(NAICS), true_NAICS := imputed_NAICS]
withNAICS[, true_NAICS := as.numeric(str_pad(true_NAICS, width = 6, side = 'right', pad = '0'))]

withNAICS[, two_digit_NAICS := true_NAICS - true_NAICS %% 10000]


withNAICS[true_NAICS %% 10000 != 0, three_digit_NAICS := true_NAICS - true_NAICS %% 1000]
missing_three_digit_NAICS = withNAICS[true_NAICS %% 10000 == 0]
two_digit_to_three = unique(withNAICS, by = c('GlobalCompanyKey', 'two_digit_NAICS'))[
  !is.na(three_digit_NAICS) & realfirm == T, 
  .(numfirms = .N, two_digit_NAICS = median(two_digit_NAICS)), 
  by = three_digit_NAICS]
two_digit_to_three[, two_to_three_ratio := numfirms / sum(numfirms), two_digit_NAICS]
setnames(two_digit_to_three, 'three_digit_NAICS', 'imputed_three_digit_NAICS')
two_digit_to_three[, numfirms := NULL]
addingThreeDigit = merge(missing_three_digit_NAICS, two_digit_to_three, by = 'two_digit_NAICS', all.x = T, allow.cartesian = T)
addingThreeDigit[, ThreeDigitadded := 1]
addingThreeDigit[, three_digit_ratio := two_to_three_ratio*sic_ratio]
withThreeDigit = rbind(withNAICS, addingThreeDigit, fill = T)
withThreeDigit[is.na(three_digit_NAICS) & is.na(ThreeDigitadded), three_digit_ratio := 0]
withThreeDigit[, realfirm := is.na(NAICSadded) & is.na(ThreeDigitadded)]

withThreeDigit[!is.na(three_digit_NAICS) & is.na(ThreeDigitadded), three_digit_ratio := sic_ratio]
withThreeDigit[!is.na(three_digit_NAICS), true_three_digit_NAICS := three_digit_NAICS][is.na(three_digit_NAICS), true_three_digit_NAICS := imputed_three_digit_NAICS]

write_feather(withThreeDigit, output_files['withThreeDigit'])

withThreeDigit[true_NAICS %% 1000 != 0, four_digit_NAICS := true_NAICS - true_NAICS %% 100]
missing_four_digit_NAICS = withThreeDigit[true_NAICS %% 1000 == 0]
three_digit_to_four = unique(withThreeDigit, by = c('GlobalCompanyKey', 'NAICS'))[!is.na(four_digit_NAICS) & realfirm == T, 
                                                                              .(numfirms = .N, true_three_digit_NAICS = median(true_three_digit_NAICS)), 
                                                                              by = four_digit_NAICS]
three_digit_to_four[, three_to_four_ratio := numfirms/sum(numfirms), true_three_digit_NAICS]
setnames(three_digit_to_four, 'four_digit_NAICS', 'imputed_four_digit_NAICS')
addingFourDigit = merge(missing_four_digit_NAICS, three_digit_to_four[, numfirms := NULL], by = 'true_three_digit_NAICS', all.x = T, allow.cartesian = T)
addingFourDigit[, FourDigitadded := 1]
addingFourDigit[, four_digit_ratio := three_to_four_ratio*three_digit_ratio]
withFourDigit = rbind(withThreeDigit, addingFourDigit, fill = T)

withFourDigit = withFourDigit[!is.na(four_digit_NAICS) | !is.na(imputed_four_digit_NAICS)]

withFourDigit[is.na(four_digit_NAICS) & is.na(FourDigitadded), four_digit_ratio := 0]
withFourDigit[, realfirm := is.na(NAICSadded) & is.na(ThreeDigitadded) & is.na(FourDigitadded)]
withFourDigit[!is.na(four_digit_NAICS) & is.na(FourDigitadded), four_digit_ratio := three_digit_ratio]
withFourDigit[!is.na(four_digit_NAICS), true_four_digit_NAICS := four_digit_NAICS][is.na(four_digit_NAICS), true_four_digit_NAICS := imputed_four_digit_NAICS]


withFourDigit[true_NAICS %% 100 != 0, five_digit_NAICS := true_NAICS - true_NAICS %% 10]
missing_five_digit_NAICS = withFourDigit[true_NAICS %% 100 == 0]
four_digit_to_five = unique(withFourDigit, by = c('GlobalCompanyKey', 'NAICS'))[!is.na(five_digit_NAICS) & realfirm == T, 
                                                                                .(numfirms = .N, true_four_digit_NAICS = median(true_four_digit_NAICS)), 
                                                                                by = five_digit_NAICS]
four_digit_to_five[, four_to_five_ratio := numfirms/sum(numfirms), true_four_digit_NAICS]
setnames(four_digit_to_five, 'five_digit_NAICS', 'imputed_five_digit_NAICS')
addingFiveDigit = merge(missing_five_digit_NAICS, four_digit_to_five[, numfirms := NULL], by = 'true_four_digit_NAICS', all.x = T, allow.cartesian = T)
addingFiveDigit[, FiveDigitadded := 1]
addingFiveDigit[, five_digit_ratio := four_to_five_ratio * four_digit_ratio]
withFiveDigit = rbind(withFourDigit, addingFiveDigit, fill = T)

withFiveDigit = withFiveDigit[!is.na(five_digit_NAICS) | !is.na(imputed_five_digit_NAICS)]

withFiveDigit[is.na(five_digit_NAICS) & is.na(FiveDigitadded), five_digit_ratio := 0]
withFiveDigit[, realfirm := is.na(NAICSadded) & is.na(ThreeDigitadded) & is.na(FourDigitadded) & is.na(FiveDigitadded)]
withFiveDigit[!is.na(five_digit_NAICS) & is.na(FiveDigitadded), five_digit_ratio := four_digit_ratio]
withFiveDigit[!is.na(five_digit_NAICS), true_five_digit_NAICS := five_digit_NAICS][is.na(five_digit_NAICS), true_five_digit_NAICS := imputed_five_digit_NAICS]


# adds one firm to each six digit industry, to prevent missing values in the crosswalk
# Not actually sure that there's a missing values issue.
# Might just be adding stuff like "management of companies and enterprises"
# that's only for establishments and never used to code firms in Compustat.
six_2017 = data.table(read.xlsx(input_data['NAICSCodes2017']))[, 1:2]
six_2012 = data.table(read_xls(input_data['NAICSCodes2012']))
six_2007 = data.table(read_xls(input_data['NAICSCodes2007']))
six_2002 = data.table(read_fwf(input_data['NAICSCodes2002'], 
                               fwf_empty(input_data['NAICSCodes2002'], 
                                         col_names = c('Code', '2002 NAICS Title')
                               ), 
                               skip = 1)
                    )[!is.na(Code)]

setnames(six_2017, c('Code', 'Title'))
setnames(six_2012, c('Code', 'Title'))
setnames(six_2007, c('Code', 'Title'))
setnames(six_2002, c('Code', 'Title'))
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
all_six_digit_codes[, five := six - six %% 10]
all_six_digit_codes[, n_subcodes := .N - 1, five]
all_six_digit_codes = all_six_digit_codes[n_subcodes == 0 | substr(six, 6, 6) != 0]

withFiveDigit[true_NAICS %in% all_six_digit_codes$six, six_digit_NAICS := true_NAICS]
missing_six_digit_NAICS = withFiveDigit[!true_NAICS %in% all_six_digit_codes$six]
five_digit_to_six = rbind(unique(withFiveDigit, by = c('GlobalCompanyKey', 'NAICS')), 
                          dummy_firms, 
                          fill = T
                          )[!is.na(six_digit_NAICS) & (realfirm == T | dummyfirm == T), 
                                    .(numfirms = .N, true_five_digit_NAICS = median(true_five_digit_NAICS)), 
                                    by = six_digit_NAICS]
five_digit_to_six[, five_to_six_ratio := numfirms/sum(numfirms), true_five_digit_NAICS]
setnames(five_digit_to_six, 'six_digit_NAICS', 'imputed_six_digit_NAICS')
addingSixDigit = merge(missing_six_digit_NAICS, five_digit_to_six[, numfirms := NULL], by = 'true_five_digit_NAICS', all.x = T, allow.cartesian = T)
addingSixDigit[, SixDigitadded := 1]
addingSixDigit[, six_digit_ratio := five_to_six_ratio*five_digit_ratio]
withSixDigit = rbind(withFiveDigit, addingSixDigit, fill = T)

withSixDigit = withSixDigit[!is.na(six_digit_NAICS) | !is.na(imputed_six_digit_NAICS)]

withSixDigit[is.na(six_digit_NAICS) & is.na(SixDigitadded), six_digit_ratio := 0]
withSixDigit[, realfirm := is.na(NAICSadded) & is.na(ThreeDigitadded) & is.na(FourDigitadded) & is.na(FiveDigitadded) & is.na(SixDigitadded)]
withSixDigit[!is.na(six_digit_NAICS) & is.na(SixDigitadded), six_digit_ratio := five_digit_ratio]
withSixDigit[!is.na(six_digit_NAICS), true_six_digit_NAICS := six_digit_NAICS
           ][is.na(six_digit_NAICS), true_six_digit_NAICS := imputed_six_digit_NAICS]

write_feather(withSixDigit, output_files['withSixDigit'])
