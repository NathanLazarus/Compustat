input_data = c(dtcut = 'IntermediateFiles/dtcut.feather', 
               crosswalk97_02 = 'Data/NAICS Crosswalks/1997_NAICS_to_2002_NAICS.xls', 
               crosswalk02_07 = 'Data/NAICS Crosswalks/2002_to_2007_NAICS.xls', 
               crosswalk07_12 = 'Data/NAICS Crosswalks/2007_to_2012_NAICS.xls', 
               crosswalk12_17 = 'Data/NAICS Crosswalks/2012_to_2017_NAICS.xlsx')

output_files = c(defunct_NAICS_to_2017_crosswalk = 'IntermediateFiles/Old NAICS to 2017 Crosswalk.feather')

dtcut = read_feather_dt(input_data['dtcut'])

# Add Industry Classification Codes for Firms that have them some years but not others ----------
tictoc::tic()
dtcut[, NAICSHistorical := as.numeric(str_pad(NAICSHistorical, width = 6, side = 'right', pad = '0'))]
dtcut[, SICHistorical := as.numeric(str_pad(SICHistorical, width = 4, side = 'right', pad = '0'))]
dtcut[, currentNAICS := as.numeric(str_pad(currentNAICS, width = 6, side = 'right', pad = '0'))]
dtcut[, currentSIC := as.numeric(str_pad(currentSIC, width = 4, side = 'right', pad = '0'))]

dtcut[NAICSHistorical >= 990000, NAICSHistorical := NA]
dtcut[SICHistorical >= 9900, SICHistorical := NA]
dtcut[currentNAICS >= 990000, currentNAICS := NA]
dtcut[currentSIC >= 9900, currentSIC := NA]

dtcut[, NAICSHistorical :=
         dtcut[!is.na(NAICSHistorical)
             ][dtcut, NAICSHistorical,
               on = c('GlobalCompanyKey', 'DataYearFiscal'),
               roll = 'nearest']]

dtcut[, SICHistorical :=
         dtcut[!is.na(SICHistorical)
             ][dtcut, SICHistorical,
               on = c('GlobalCompanyKey', 'DataYearFiscal'),
               roll = 'nearest']]
dtcut[, SIC := SICHistorical]
# dtcut[is.na(SIC), SIC := currentSIC]
dtcut[, NAICS := NAICSHistorical]
# dtcut[is.na(NAICS) & is.na(SICHistorical), NAICS := currentNAICS]
# There are only 41 obs with SIC_historical but not NAICSHistorical.
# I give SIC_historical precedence over currentNAICS in those cases.



crosswalk97_02 = data.table(read_xls(input_data['crosswalk97_02'], sheet = 2)
                          )[, `1997 NAICS Code` := as.numeric(NAICS97)
                          ][, `2002 NAICS Code` := NAICS02]
crosswalk02_07 = data.table(read_xls(input_data['crosswalk02_07'], skip = 2))
crosswalk07_12 = data.table(read_xls(input_data['crosswalk07_12'], skip = 2))
crosswalk12_17 = data.table(read_xlsx(input_data['crosswalk12_17'], skip = 2))


all_six_digit_crosswalks_long = rbind(crosswalk97_02[, .(NAICS_start = `1997 NAICS Code`, NAICS_end = `2002 NAICS Code`,
                                               year_of_NAICS_change = 2002)],
                            crosswalk02_07[, .(NAICS_start = `2002 NAICS Code`, NAICS_end = `2007 NAICS Code`,
                                               year_of_NAICS_change = 2007)],
                            crosswalk07_12[, .(NAICS_start = `2007 NAICS Code`, NAICS_end = `2012 NAICS Code`,
                                               year_of_NAICS_change = 2012)],
                            crosswalk12_17[, .(NAICS_start = `2012 NAICS Code`, NAICS_end = `2017 NAICS Code`,
                                               year_of_NAICS_change = 2017)]
                           )[, splits := .N > 1, .(NAICS_start, year_of_NAICS_change)
                           ][, n_targets := .N, .(NAICS_end, year_of_NAICS_change)] #[, `:=`(start_year = year_of_NAICS_change - 5, end_year = year_of_NAICS_change - 1)
                           # ][year_of_NAICS_change == 2002, start_year := -Inf
                           # ]
adding_less_than_six_digit_codes = CJ.dt(all_six_digit_crosswalks_long, data.table(n_digits = 2:6)
    )[, c('splits', 'n_targets') := NULL
    ][, NAICS_start := as.numeric(str_pad(substr(as.character(NAICS_start), 1, n_digits), width = 6, side = 'right', pad = '0'))
    ][, not_repeated := n_digits == max(n_digits), .(NAICS_start, NAICS_end, year_of_NAICS_change)]
all_crosswalks_long = adding_less_than_six_digit_codes[not_repeated == TRUE
                                                     ][, splits := .N > 1, .(NAICS_start, year_of_NAICS_change)
                                                     ][, n_targets := .N, .(NAICS_end, year_of_NAICS_change, n_digits)]


actual_NAICS_in_Compustat = dtcut[, .(DataYearFiscal, NAICSHistorical, GlobalCompanyKey)]

clusters = makeCluster(7)
registerDoSNOW(clusters)

old_to_new_NAICS_ratios_for_codes_that_split = foreach(
  code_that_splits = iter(unique(all_crosswalks_long[splits == TRUE,
                                                     .(code = NAICS_start,
                                                       year = year_of_NAICS_change)]),
                          by = 'row'),
  .combine = rbind
  ) %dopar% {
                
    potential_NAICS = all_crosswalks_long[NAICS_start == code_that_splits$code &
                                            year_of_NAICS_change == code_that_splits$year,
                                          .(NAICS_start, NAICS_end, n_targets, year_of_NAICS_change)]
    to_find_firms_with_codes_that_split = copy(actual_NAICS_in_Compustat)
    to_find_firms_with_codes_that_split[(DataYearFiscal == code_that_splits$year - 1 | DataYearFiscal == code_that_splits$year - 2) &
                                          code_that_splits$code %in% roundDown(NAICSHistorical, c(1, 10, 100, 1000, 10000)),
                                        was_in_industry := TRUE
                                      ][to_find_firms_with_codes_that_split[was_in_industry == TRUE, .(GlobalCompanyKey, follow_these = TRUE)],
                                        on = 'GlobalCompanyKey',
                                        follow_these := i.follow_these
                                      ][follow_these == TRUE & DataYearFiscal >= code_that_splits$year,
                                        found_NAICS_matching_potential := NAICSHistorical %in% potential_NAICS[, NAICS_end]]
    if (nrow(to_find_firms_with_codes_that_split[found_NAICS_matching_potential == TRUE]) == 0) {
      ratiotable = potential_NAICS
      ratiotable[, relative_weight := 1 / n_targets
               ][, ratio := relative_weight / sum(relative_weight)
               ][, relative_weight := NULL]
    } else {
      empirical_ratios =
        to_find_firms_with_codes_that_split[found_NAICS_matching_potential == TRUE, common_subsequent_NAICS_among_potential := .N == max(.N), .(NAICSHistorical, GlobalCompanyKey)
                                          ][common_subsequent_NAICS_among_potential == TRUE, is_modal_subsequent_NAICS := DataYearFiscal == min(DataYearFiscal), GlobalCompanyKey
                                          ][is_modal_subsequent_NAICS == TRUE, .(num_firms = .N), NAICSHistorical
                                          ][, ratio := num_firms / sum(num_firms)]
      #I don't weight by firm size or anything because the firms missing NAICS are probably more like the small firms than the large ones
      ratiotable = potential_NAICS[empirical_ratios, on = c(NAICS_end = 'NAICSHistorical'), ratio := i.ratio
                                 ][is.na(ratio), ratio := 0]
    }
    ratiotable
  }
stopCluster(clusters)


all_crosswalks_long[old_to_new_NAICS_ratios_for_codes_that_split,
                    on = c('NAICS_start', 'NAICS_end', 'year_of_NAICS_change'),
                    ratio := i.ratio
                  ][is.na(ratio), ratio := 1]

# crosswalk97_17 = crosswalk97_02[crosswalk02_07, on = '2002 NAICS Code'
#                               ][crosswalk07_12, on = '2007 NAICS Code'
#                               ][crosswalk12_17, on = '2012 NAICS Code']
# setnames(crosswalk97_17, gsub("Title.*", "Title", names(crosswalk97_17)))
# 
# crosswalk97_17[all_crosswalks_long[year_of_NAICS_change == 2002], on = c(`1997 NAICS Code` = 'NAICS_start', `2002 NAICS Code` = 'NAICS_end'), ratio97_02 := i.ratio
#              ][all_crosswalks_long[year_of_NAICS_change == 2007], on = c(`2002 NAICS Code` = 'NAICS_start', `2007 NAICS Code` = 'NAICS_end'), ratio02_07 := i.ratio
#              ][all_crosswalks_long[year_of_NAICS_change == 2012], on = c(`2007 NAICS Code` = 'NAICS_start', `2012 NAICS Code` = 'NAICS_end'), ratio07_12 := i.ratio
#              ][all_crosswalks_long[year_of_NAICS_change == 2017], on = c(`2012 NAICS Code` = 'NAICS_start', `2017 NAICS Code` = 'NAICS_end'), ratio12_17 := i.ratio
#              ][, ratio97_17 := ratio97_02 * ratio02_07 * ratio07_12 * ratio12_17
#              ][, ratio02_17 := ratio02_07 * ratio07_12 * ratio12_17
#              ][, ratio07_17 := ratio07_12 * ratio12_17]

crosswalk97_17 =
  all_crosswalks_long[year_of_NAICS_change == 2002,
                      .(`1997 NAICS Code` = NAICS_start, `2002 NAICS Code` = NAICS_end, ratio97_02 = ratio)
                    ][all_crosswalks_long[year_of_NAICS_change == 2007,
                        .(`2002 NAICS Code` = NAICS_start, `2007 NAICS Code` = NAICS_end, ratio02_07 = ratio)],
                      on = '2002 NAICS Code'
                    ][all_crosswalks_long[year_of_NAICS_change == 2012,
                        .(`2007 NAICS Code` = NAICS_start, `2012 NAICS Code` = NAICS_end, ratio07_12 = ratio)],
                      on = '2007 NAICS Code'
                    ][all_crosswalks_long[year_of_NAICS_change == 2017,
                        .(`2012 NAICS Code` = NAICS_start, `2017 NAICS Code` = NAICS_end, ratio12_17 = ratio)],
                      on = '2012 NAICS Code'
                    ][, ratio97_17 := ratio97_02 * ratio02_07 * ratio07_12 * ratio12_17
                    ][, ratio02_17 := ratio02_07 * ratio07_12 * ratio12_17
                    ][, ratio07_17 := ratio07_12 * ratio12_17]



defunct_NAICS_to_2017_crosswalk = rbind(unique(crosswalk97_17[, .(NAICS_start = `1997 NAICS Code`, NAICS_end = `2017 NAICS Code`,
                                                                  ratio = ratio97_17, year_of_NAICS_change = 2002)]),
                                        unique(crosswalk97_17[, .(NAICS_start = `2002 NAICS Code`, NAICS_end = `2017 NAICS Code`,
                                                                  ratio = ratio02_17, year_of_NAICS_change = 2007)]),
                                        unique(crosswalk97_17[, .(NAICS_start = `2007 NAICS Code`, NAICS_end = `2017 NAICS Code`,
                                                                  ratio = ratio07_17, year_of_NAICS_change = 2012)]),
                                        unique(crosswalk97_17[, .(NAICS_start = `2012 NAICS Code`, NAICS_end = `2017 NAICS Code`,
                                                                  ratio = ratio12_17, year_of_NAICS_change = 2017)])
                                      )[, `:=`(start_year = year_of_NAICS_change - 5, end_year = year_of_NAICS_change - 1)
                                      ][year_of_NAICS_change == 2002, start_year := -Inf
                                      ][ratio > 0]



# Now I want every NAICS_start code x applicable year pair to be full,
# except for NAICS_start for NAICS codes that are valid in 2017 or later

na_or_less_to_2016 = function(otherYear, thisYear) fifelse(otherYear < thisYear, 2016, otherYear, na = 2016)
na_or_greater_to_negInf = function(otherYear, thisYear) fifelse(otherYear > thisYear, -Inf, otherYear, na = -Inf)


roll_end_year_forward = function(start, end) pmin(na_or_less_to_2016(start[1] - 1, end),
                                                  na_or_less_to_2016(start[2] - 1, end),
                                                  na_or_less_to_2016(start[3] - 1, end),
                                                  na_or_less_to_2016(start[4] - 1, end))

roll_start_year_back = function(start, end) pmax(na_or_greater_to_negInf(end[1] + 1, start),
                                                 na_or_greater_to_negInf(end[2] + 1, start),
                                                 na_or_greater_to_negInf(end[3] + 1, start),
                                                 na_or_greater_to_negInf(end[4] + 1, start))

crosswalk_applicable_dates = unique(defunct_NAICS_to_2017_crosswalk[, .(NAICS_start, start_year, end_year)])
crosswalk_applicable_dates[, applicable_beginning := roll_start_year_back(start_year, end_year), NAICS_start]
crosswalk_applicable_dates[, applicable_ending := roll_end_year_forward(start_year, end_year), NAICS_start]
crosswalk_applicable_dates[applicable_ending == 2016 & !NAICS_start %in% defunct_NAICS_to_2017_crosswalk[, NAICS_end],
                           applicable_ending := Inf]

defunct_NAICS_to_2017_crosswalk[crosswalk_applicable_dates,
                                on = c('NAICS_start', 'start_year', 'end_year'),
                                `:=`(applicable_beginning = i.applicable_beginning,
                                     applicable_ending = i.applicable_ending)
                              ][, c('start_year', 'end_year') := NULL]

tictoc::toc()




tictoc::tic()

go_through_firm_specific_crosswalks = function(years_of_change, data, wide_crosswalk) {
  # years_of_change = c(2007, 2012)
  # data = dtcut
  # wide_crosswalk = crosswalk97_17
  
  clusters = makeCluster(7)
  registerDoSNOW(clusters)
  
  foreach(year_of_change = years_of_change) %do% {
    actual_NAICS_in_Compustat = data[, .(DataYearFiscal, NAICSHistorical, GlobalCompanyKey)]
    
    
    my_start_var = paste0(year_of_change - 5, ' NAICS Code')
    all_NAICS_cols = c('1997 NAICS Code', '2002 NAICS Code', '2007 NAICS Code', '2012 NAICS Code', '2017 NAICS Code')
    
    crosswalk_to_future_codes = melt(wide_crosswalk[, ..all_NAICS_cols],
                                    measure.vars = all_NAICS_cols[all_NAICS_cols != my_start_var],
                                    id.vars = my_start_var,
                                    variable.name = 'year_of_code',
                                    value.name = 'future_code'
                                   )[, year := as.numeric(substr(year_of_code, 1, 4))
                                   ][, NAICS_start := get(my_start_var)
                                   ][year >= year_of_change, .(NAICS_start, NAICS_end = future_code)]
    
    firm_specific_crosswalks = foreach(
        code_to_start_from = unique(crosswalk_to_future_codes$NAICS_start),
        .combine = rbind,
        .packages = c('data.table')
      ) %dopar% {
    
        potential_NAICS = crosswalk_to_future_codes[NAICS_start == code_to_start_from]
    
        firm_crosswalk = copy(actual_NAICS_in_Compustat)
        setkey(firm_crosswalk, GlobalCompanyKey)
        firm_crosswalk[
          (DataYearFiscal == year_of_change - 1 | DataYearFiscal == year_of_change - 2) & NAICSHistorical == code_to_start_from,
          was_in_industry := TRUE
        ][
          firm_crosswalk[was_in_industry == TRUE, .(GlobalCompanyKey, follow_these = TRUE)],
                       on = 'GlobalCompanyKey',
                       follow_these := i.follow_these
        ][
          follow_these == TRUE & DataYearFiscal >= year_of_change,
          found_NAICS_matching_potential := NAICSHistorical %in% potential_NAICS[, NAICS_end]
        ][
              found_NAICS_matching_potential == TRUE,
              common_subsequent_NAICS_among_potential := .N == max(.N),
              .(NAICSHistorical, GlobalCompanyKey)
        ][
          common_subsequent_NAICS_among_potential == TRUE,
          is_modal_subsequent_NAICS := DataYearFiscal == suppressWarnings(min(DataYearFiscal)), #min gives a warning if there are 0 obs
          GlobalCompanyKey
        ][
          firm_crosswalk[is_modal_subsequent_NAICS == TRUE],
          on = 'GlobalCompanyKey',
          future_matching_NAICS := i.NAICSHistorical
        ]
        
        firm_crosswalk[NAICSHistorical == code_to_start_from & !is.na(future_matching_NAICS),
                       .(GlobalCompanyKey, DataYearFiscal, NAICSHistorical, future_matching_NAICS)]
      }
    
    data[firm_specific_crosswalks, on = c('GlobalCompanyKey', 'DataYearFiscal'), future_matching_NAICS := i.future_matching_NAICS
       ][!is.na(future_matching_NAICS), NAICSHistorical := future_matching_NAICS]
    NULL
  
  }
  stopCluster(clusters)
  
  NULL
}

go_through_firm_specific_crosswalks(seq(2002, 2017, by = 5), dtcut, crosswalk97_17)
tictoc::toc()



dtcut[, SIC := SICHistorical]
dtcut[is.na(SIC), SIC := currentSIC]
dtcut[, NAICS := NAICSHistorical]
dtcut[is.na(NAICS) & is.na(SICHistorical), NAICS := currentNAICS]



setkey(dtcut[, duplicate_DataYearFiscal := DataYearFiscal], NAICS, DataYearFiscal, duplicate_DataYearFiscal)
setkey(defunct_NAICS_to_2017_crosswalk, NAICS_start, applicable_beginning, applicable_ending)

with_stochastic_crosswalk_for_defunct_NAICS = foverlaps(dtcut, defunct_NAICS_to_2017_crosswalk)
with_stochastic_crosswalk_for_defunct_NAICS[, splits := .N, .(GlobalCompanyKey, DataYearFiscal)]
setnames(with_stochastic_crosswalk_for_defunct_NAICS, c('NAICS_end', 'NAICS'), c('NAICS', 'NAICS_before_updating'))
with_stochastic_crosswalk_for_defunct_NAICS[is.na(NAICS), NAICS := NAICS_before_updating]
