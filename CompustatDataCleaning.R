input_data = c(companyData = 'Data/Company Data (fixed identifying variables).feather', 
               fundamentalsData = 'Data/Annual Fundamentals (most variables, raw).feather', 
               CompustatVariableNames = 'Data/Variable Names and Descriptions.csv')

output_files = c(raw_dt = 'IntermediateFiles/raw_dt.feather', 
                 dtcut = 'IntermediateFiles/dtcut.feather')

#TRANE CO, TRANE INC? Duplicates?

#This leaves financial firms and firms with missing market values in the data

companyData = read_feather_dt(input_data['companyData'])
fundamentalsData = read_feather_dt(input_data['fundamentalsData'])

# TFP
# write_dta(companyData[fundamentalsData[, c('year1', 'year2') := .(min(fyear), max(fyear)), gvkey], on = 'gvkey', `:=`(year1 = i.year1, year2 = i.year2)], 
#           'C:/Users/Nathan/Downloads/Programs-20201123T183843Z-001/Programs/compustat_names.dta')
# write_dta(fundamentalsData, 'C:/Users/Nathan/Downloads/Programs-20201123T183843Z-001/Programs/compustat_data.dta')


companyData[, sic := as.numeric(sic)]
companyData[, naics := as.numeric(naics)]
CompustatVariableNames = fread(input_data['CompustatVariableNames'])

give_descriptive_variable_names(data = fundamentalsData, variable_names = CompustatVariableNames)
give_descriptive_variable_names(data = companyData, variable_names = CompustatVariableNames)

setnames(fundamentalsData,
         c('NorthAmericaIndustrialClassificationSystemHistorical', 'StandardIndustrialClassificationHistorical'),
         c('NAICSHistorical', 'SICHistorical'))

fundamentalsData[companyData, on = 'GlobalCompanyKey', 
   `:=`(currentSIC = i.StandardIndustryClassificationCode, 
        currentNAICS = i.NorthAmericanIndustryClassificationCode, 
        loc = i.CurrentISOCountryCodeHeadquarters)]

fundamentalsData[, calendaryear := year(datadate)]
fundamentalsData[, cusip6 := substr(cusip, 1, 6)]


write_feather(fundamentalsData, output_files['raw_dt'])


# Make it unique (one observation per GlobalCompanyKey x fiscal year) --------------
# this is a better implementation of the common WRDS filters,
# consol == 'C', datafmt == 'STD', indfmt == 'INDL,'
# which are intended to get unique values but leave out a lot of data
# from datafmt 'SUMM_STD' and indfmt 'INDL'
# (note that 3625 obs of datafmt SUMM_STD with no corresponding STD report
# are excluded. These can include duplicative statements for merging firms
# as well as some private firms.)

fundamentalsData = fundamentalsData[consol == 'C']
#remove duplicative entries like pro forma or unconsolidated pre-FASB statements

fundamentalsData = fundamentalsData[!(GlobalCompanyKey == '008299' & DataYearFiscal == 1986 & datadate == as.Date('1986-12-31'))]
#two reports in the same fiscal year

with_restatements = merge_and_reconcile(fundamentalsData[datafmt == 'SUMM_STD' & indfmt == 'INDL'], 
                                        fundamentalsData[datafmt == 'STD' & indfmt == 'INDL'], 
                                        join_cols = c('GlobalCompanyKey', 'datadate'), 
                                        all_prioritized = F, 
                                        all_deprioritized = T)

with_financial_format_statements = merge_and_reconcile(with_restatements, 
                                                       fundamentalsData[datafmt == 'STD' & indfmt == 'FS'], 
                                                       join_cols = c('GlobalCompanyKey', 'datadate'))


dtcut = with_financial_format_statements[((curcd == 'USD' & !is.na(curcd)) & loc == 'USA') |
                                            conm == 'STEALTH BIOTHERA -ADR' | conm == 'SIGNET JEWELERS LTD' |
                                            conm == 'WHITE MTNS INS GROUP LTD']
# some firms with LOC == 'CYM' or 'BMU' are really US firms in tax havens.
# But there's also plenty of foreign firms in the tax havens.
# Maybe CurrentPrimaryIssueTagCanada PrimaryIssueTagRestofWorld
# CurrentPrimaryIssueTagUS InternationalDomesticBothIndicator can help
# stealth, signet and white mountains are three I've found so far.
# CRSP doesn't seem to help;
# I was trying to use the country codes in the ISINs
# FIC is uniformly worse than LOC


# When I pulled the Compustat data in fall 2020, there were a handful of firms missing SIC codes.
# All of them had announced plans to IPO, but hadn't made their IPO yet.
# Now in February 2021, the firms missing SIC codes are ones that IPO-ed in October-December 2020.
# I was removing them, because they had yet to become public, but now I'm keeping them.
# I don't understand the connection between missing SIC codes and having yet to IPO,
# and it's especially weird that that changes with the seasons.

nrow(with_financial_format_statements[is.na(AssetsTotal)])


write_feather(dtcut, output_files['dtcut'])