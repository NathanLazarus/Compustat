input_data = c(withThreeDigit = 'Data/withThreeDigit.csv',
               firstAndLastYear = 'Data/First and Last Year in Compustat.rds',
               firmFoundingDates = 'Data/Firm Founding Dates.xlsx',
               CRSPCompustatLink = 'Data/CRSP Compustat Link (gvkey to permno and permco).rds',
               myHandCodedFoundingDates = 'Data/My Hand Coded Founding Dates.xlsx',
               FREDData = 'Data/FRED Data (Inflation and Interest Rates).rds')

output_files = c(firmAgeDistributions = 'SpreadsheetOutputs/Firm Age Distributions.xlsx',
                 companyData = 'Data/Company Data (fixed identifying variables).rds')

withThreeDigit = fread_and_getCharCols(input_data['withThreeDigit'])
went_public_data = merge(readRDS(input_data['firstAndLastYear']), readRDS(input_data['companyData']), all.x = T, all.y = T)

firm_ages = data.table(read.xlsx(input_data['firmFoundingDates'])
                     )[Founding != -99
                     ][, cusip6 := substr(CUSIP, 1, 6)]
gvkey_to_permno = unique(readRDS(input_data['CRSPCompustatLink']), by = c('gvkey', 'lpermno')
                       )[, PERM := as.character(lpermno)
                       ][!is.na(PERM)]

my_firm_ages = data.table(read.xlsx(input_data['myHandCodedFoundingDates']))

setnames(gvkey_to_permno, 'gvkey', 'GlobalCompanyKey')
just_compustat = unique(withThreeDigit, by = c('GlobalCompanyKey', 'calendaryear'))
just_compustat[, cusip6 := substr(cusip, 1, 6)]
just_compustat[firm_ages, on = 'cusip6', cusip_founded := i.Founding]
just_compustat[firm_ages, on = 'cusip6', my_hand_coding_founded := i.Founding]
gvkey_to_age = gvkey_to_permno[firm_ages, on = 'PERM', permno_founded := i.Founding]


foreach(year_to_merge = unique(just_compustat$calendaryear)) %do% {
  to_merge = copy(gvkey_to_age)[, calendaryear := year_to_merge
                              ][!is.na(permno_founded) & year(linkdt) <= year_to_merge,
                                .(first_component_firm_founded = min(permno_founded, na.rm = T), calendaryear = first(calendaryear)),
                                GlobalCompanyKey]
  just_compustat[to_merge, on = c('calendaryear', 'GlobalCompanyKey'), first_component_firm_founded := i.first_component_firm_founded]
  NULL
}

went_public_data[, earliest_compustat_date := pmin(year1, year(ipodate), na.rm = T)]
just_compustat[went_public_data, on = c(GlobalCompanyKey = 'gvkey'), earliest_compustat_date := i.earliest_compustat_date]
just_compustat[!is.na(my_hand_coding_founded), founded := pmin(first_component_firm_founded, cusip_founded, earliest_compustat_date, na.rm = T)]
just_compustat[is.na(my_hand_coding_founded), founded := pmin(first_component_firm_founded, cusip_founded, earliest_compustat_date, na.rm = T)]

just_compustat[, firm_age := calendaryear - founded][firm_age < 0, firm_age := 0][firm_age > 100, firm_age := 100][, firm_age_mod_5 := firm_age %/% 5]
just_compustat[data.table(firm_age_mod_5 = 0:20,firm_age_cat = factor(c(paste(seq(0,95,5),seq(4,99,5), sep = '-'),'100+'), levels = c(paste(seq(0,95,5),seq(4,99,5), sep = '-'),'100+')))
               , on = 'firm_age_mod_5', firm_age_cat := i.firm_age_cat]
gdp_deflator = readRDS(input_data['FREDData'])[, .(gdpdef_level, calendaryear = year)]
gdp_deflator[, to_2019_dollars := gdp_deflator[calendaryear == 2019, GDPDEF]/GDPDEF]
just_compustat[gdp_deflator, on = 'calendaryear', to_2019_dollars := i.to_2019_dollars]
just_compustat[, real_MW := monopolywealth * to_2019_dollars]
# just_compustat[calendaryear == 2019 & !is.na(firm_age) & !is.na(monopolywealth), sum(real_MW), firm_age]
just_compustat = rbind(just_compustat,
                       data.table(calendaryear = unique(just_compustat$calendaryear), dummy_0_mw = 1, key = 'dummy_0_mw')[
                         data.table(firm_age_mod_5 = 0:20,
                                    firm_age = 0:20 * 5,
                                    firm_age_cat = factor(c(paste(seq(0,95,5),seq(4,99,5), sep = '-'),'100+'),
                                                                                levels = c(paste(seq(0,95,5),seq(4,99,5), sep = '-'),'100+')),
                                    dummy_0_mw = 1,
                                    key = 'dummy_0_mw'),
                         allow.cartesian = T
                       ][, c('real_MW', 'monopolywealth') := 0][, founded := calendaryear - firm_age], fill = T)

years_and_weights = data.table(yr_shows_up = rep(1950:2019, each = 5), yr_diff = rep(-2:2, times = 70), weight = rep(c(0.1, 0.2, 0.4, 0.2, 0.1), times = 70))
years_and_weights[, yr_to_mimic := yr_shows_up + yr_diff]
years_and_weights = years_and_weights[yr_to_mimic >= 1950 & yr_to_mimic <= 2019]
years_and_weights[, weight := weight/sum(weight), yr_shows_up]

fake_firms_to_dummy_for_those_missing_founding_years = foreach(this_year_pair = iter(years_and_weights, by = 'row'),
                                                               .combine = rbind) %do% {
  missing_founding_year_MW_to_allocate = just_compustat[is.na(cusip_founded) & is.na(first_component_firm_founded) &
                                                          earliest_compustat_date == this_year_pair$yr_shows_up & !is.na(real_MW), 
                                                        .(to_allocate = sum(real_MW)),
                                                        calendaryear]
    just_compustat[(!is.na(cusip_founded) | !is.na(first_component_firm_founded)) & earliest_compustat_date == this_year_pair$yr_to_mimic & !is.na(real_MW),
                   .(conm, real_MW, GlobalCompanyKey, calendaryear, cusip_founded, first_component_firm_founded)
                 ][, founding_date_data_founded := pmin(cusip_founded, first_component_firm_founded, this_year_pair$yr_to_mimic, na.rm = T)
                 ][, founding_date_age := this_year_pair$yr_to_mimic - founding_date_data_founded
                 ][, .(total_firms_by_age = .N), .(founding_date_age, calendaryear)
                 ][, pct_allocated_by_age := total_firms_by_age / sum(total_firms_by_age), calendaryear
                 ][missing_founding_year_MW_to_allocate, on = 'calendaryear', to_allocate := i.to_allocate
                 ][, `:=`(real_MW = pct_allocated_by_age * this_year_pair$weight *  to_allocate,
                          monopolywealth = pct_allocated_by_age * this_year_pair$weight * to_allocate,
                          firm_age = founding_date_age)
                 ][, founded := calendaryear - firm_age
                 ][, firm_age_mod_5 := pmin(firm_age %/% 5, 20)
                 ][data.table(firm_age_mod_5 = 0:20,firm_age_cat = factor(c(paste(seq(0,95,5),seq(4,99,5), sep = '-'),'100+'),
                                                                          levels = c(paste(seq(0,95,5),seq(4,99,5), sep = '-'),'100+'))),
                   on = 'firm_age_mod_5',
                   firm_age_cat := i.firm_age_cat
                 ][, dummy_for_MW_for_firms_missing_founding_year := 1][]
}

to_calculate_age_dist = rbind(just_compustat[!(is.na(cusip_founded) & is.na(first_component_firm_founded)) | dummy_0_mw == 1],
                              fake_firms_to_dummy_for_those_missing_founding_years,
                              fill = T)

age_dist_over_time = cbind(setkey(unique(to_calculate_age_dist[, .(firm_age_cat)]),firm_age_cat), 
    foreach(year = 2019:1950, .combine = cbind) %do% {
      total_MW_for_year = to_calculate_age_dist[calendaryear == year & !is.na(firm_age) & !is.na(real_MW), sum(real_MW)]
      agedist = setkey(to_calculate_age_dist[calendaryear == year & !is.na(firm_age) & !is.na(real_MW),
                                      .(`Real Monopoly Wealth` = sum(real_MW)/1000, #(billions) pct: 100*sum(real_MW) / total_MW_for_year,
                                        `Firm Age` = first(firm_age_cat)),
                                      firm_age_mod_5], firm_age_mod_5
                       )
      setnames(agedist[, .(`Real Monopoly Wealth`)], paste0(year, ' Real Monopoly Wealth (Billions)'))[]
    }
  )

to_calculate_mw_by_founding_year = copy(to_calculate_age_dist
                                      )[founded < 1900, founded := 1900
                                      ]
mw_by_founding_year = 
  data.table(calendaryear = unique(to_calculate_mw_by_founding_year$calendaryear), k = 1
           )[data.table(founded = unique(to_calculate_mw_by_founding_year$founded), k = 1), on = 'k', allow.cartesian = T
           ][, k := NULL
           ][to_calculate_mw_by_founding_year[!is.na(real_MW), .(`Real Monopoly Wealth` = sum(real_MW)/1000), .(calendaryear, founded)],
             on = c('calendaryear', 'founded'),
             `Real Monopoly Wealth` := `i.Real Monopoly Wealth`]

mw_by_founding_year_wide =
  setcolorder(
    setorder(dcast(mw_by_founding_year,
               founded ~ paste0('Real Monopoly Wealth ', calendaryear, ' (Billions)'),
               value.var = 'Real Monopoly Wealth'),
         -founded),
    c(1, ncol(mw_by_founding_year_wide):2))

write.xlsx(list(age_dist_over_time, mw_by_founding_year_wide),
           output_files['firmAgeDistributions'])
