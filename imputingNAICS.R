input_data = c(dtcut = 'IntermediateFiles/dtcut.feather', 
               crosswalk97_02 = 'Data/NAICS Crosswalks/1997_NAICS_to_2002_NAICS.xls', 
               crosswalk02_07 = 'Data/NAICS Crosswalks/2002_to_2007_NAICS.xls', 
               crosswalk07_12 = 'Data/NAICS Crosswalks/2007_to_2012_NAICS.xls', 
               crosswalk12_17 = 'Data/NAICS Crosswalks/2012_to_2017_NAICS.xlsx')

output_files = c(defunct_NAICS_to_2017_crosswalk = 'IntermediateFiles/Old NAICS to 2017 Crosswalk.feather')
tictoc::tic()
dtcut = read_feather_dt(input_data['dtcut'])

# Add Industry classification codes for firms that have them some years but not others ----------
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
dtcut[, NAICS := NAICSHistorical]


# Update NAICS codes that no longer exist to their 2017 values ---------------------
# 
# 3 min runtime
#  * General Crosswalk -------------------------------------------------------------
# 
# The NAICS codes have changed from 1997, when they were first introduced, to the present
# In particular, they change during the economic census, which is been conducted every five years
# The greatest number of changes was in 2002.
# For consistent industry definitions, we want everything in 2017 terms.
# To do this, we look at the years in which the codes changed, and find the
# percentage of firms that moved into each of the subsequent codes.
# For example, in 2017, 211111 (oil and gas extraction) was split into
# 211120 (oil extraction) and 211130 (gas extraction).
# I find that 80% of firms that were in 211111 in 2015 or 2016 moved into
# 211120 in 2017, while 20% were in 211130. So for all the firms that have
# the code 211111 historically, I use imputation ratios of 80% and 20%.
# Now you might say, well, if you see a firm go from 211111 to 211130,
# it's obvious that that firm was engaged in natural gas extraction
# the whole time, so it should have the code 211130 with probability 1.
# That's the idea of the firm crosswalk, below. The imputation ratio is
# used only for firms that exit before 2017, so we never observe their
# "2017 vintage" industry.

crosswalk97_02 = data.table(read_xls(input_data['crosswalk97_02'], sheet = 2)
                          )[, `1997 NAICS Code` := as.numeric(NAICS97)
                          ][, `2002 NAICS Code` := NAICS02]
crosswalk02_07 = data.table(read_xls(input_data['crosswalk02_07'], skip = 2))
crosswalk07_12 = data.table(read_xls(input_data['crosswalk07_12'], skip = 2))
crosswalk12_17 = data.table(read_xlsx(input_data['crosswalk12_17'], skip = 2))


all_crosswalks_long = rbind(crosswalk97_02[, .(NAICS_start = `1997 NAICS Code`, NAICS_end = `2002 NAICS Code`,
                                               year_of_NAICS_change = 2002)],
                            crosswalk02_07[, .(NAICS_start = `2002 NAICS Code`, NAICS_end = `2007 NAICS Code`,
                                               year_of_NAICS_change = 2007)],
                            crosswalk07_12[, .(NAICS_start = `2007 NAICS Code`, NAICS_end = `2012 NAICS Code`,
                                               year_of_NAICS_change = 2012)],
                            crosswalk12_17[, .(NAICS_start = `2012 NAICS Code`, NAICS_end = `2017 NAICS Code`,
                                               year_of_NAICS_change = 2017)]
                           )[, splits := .N > 1, .(NAICS_start, year_of_NAICS_change)
                           ][, n_targets := .N, .(NAICS_end, year_of_NAICS_change)]


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
                                          NAICSHistorical %inIndustryCode% code_that_splits$code,
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


all_crosswalks_long[old_to_new_NAICS_ratios_for_codes_that_split,
                    on = c('NAICS_start', 'NAICS_end', 'year_of_NAICS_change'),
                    ratio := i.ratio
                  ][is.na(ratio), ratio := 1]

# This folds all the ratios up into one wide table, so that we can see
# the probability that a firm is in a 2017 industry given its 1997 industry,
# allowing for multiple splits or changes to a given code.

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
                                                                  ratio = ratio12_17, year_of_NAICS_change = 2017)]),
                                        unique(crosswalk97_17[, .(NAICS_start = `2017 NAICS Code`, NAICS_end = `2017 NAICS Code`,
                                                                  ratio = 1, year_of_NAICS_change = Inf)])
                                      )[, `:=`(start_year = year_of_NAICS_change - 5, end_year = year_of_NAICS_change - 1)
                                      ][year_of_NAICS_change == 2002, start_year := -Inf
                                      ][year_of_NAICS_change == Inf, start_year := 2017
                                      ]


#  * * Fill in crosswalk values for every NAICS_start code x year pair ----------------------
# I want every NAICS_start code x applicable year pair to be full, to catch stray codes
# so if a code hasn't been seen before 2012, it's 2012 crosswalk is valid for 2012 to 2016 as well as any year from -Inf to 2012
# and if a code is removed in the 2007 NAICS update, it's crosswalk is valid 2002-2006 as well as 2007 to Inf

na_or_less_to_Inf = function(otherYear, thisYear) fifelse(otherYear < thisYear, Inf, otherYear, na = Inf)
na_or_greater_to_negInf = function(otherYear, thisYear) fifelse(otherYear > thisYear, -Inf, otherYear, na = -Inf)


roll_end_year_forward = function(start, end) pmin(na_or_less_to_Inf(start[1] - 1, end),
                                                  na_or_less_to_Inf(start[2] - 1, end),
                                                  na_or_less_to_Inf(start[3] - 1, end),
                                                  na_or_less_to_Inf(start[4] - 1, end),
                                                  na_or_less_to_Inf(start[5] - 1, end))

roll_start_year_back = function(start, end) pmax(na_or_greater_to_negInf(end[1] + 1, start),
                                                 na_or_greater_to_negInf(end[2] + 1, start),
                                                 na_or_greater_to_negInf(end[3] + 1, start),
                                                 na_or_greater_to_negInf(end[4] + 1, start),
                                                 na_or_greater_to_negInf(end[5] + 1, start))

crosswalk_applicable_dates = unique(defunct_NAICS_to_2017_crosswalk[, .(NAICS_start, start_year, end_year)])
crosswalk_applicable_dates[, applicable_beginning := roll_start_year_back(start_year, end_year), NAICS_start]
crosswalk_applicable_dates[, applicable_ending := roll_end_year_forward(applicable_beginning, end_year), NAICS_start]

defunct_NAICS_to_2017_crosswalk[crosswalk_applicable_dates,
                                on = c('NAICS_start', 'start_year', 'end_year'),
                                `:=`(applicable_beginning = i.applicable_beginning,
                                     applicable_ending = i.applicable_ending)
                              ][, c('start_year', 'end_year') := NULL]

# the codes 454110 514199 and 541710 split and then re-merge
defunct_NAICS_to_2017_crosswalk_final = defunct_NAICS_to_2017_crosswalk[,
                                                                        .(ratio = sum(ratio),
                                                                          year_of_NAICS_change = first(year_of_NAICS_change),
                                                                          applicable_ending = max(applicable_ending)),
                                                                        .(NAICS_start, NAICS_end, applicable_beginning)
                                                                       ][ratio > 0]

go_through_firm_specific_crosswalks = function(years_of_change, data, wide_crosswalk) {

  foreach(year_of_change = years_of_change) %do% {
    actual_NAICS_in_Compustat = data[, .(DataYearFiscal, NAICSHistorical_with_firm_crosswalk_updates, GlobalCompanyKey)]
    
    
    my_start_var = paste0(year_of_change - 5, ' NAICS Code')
    all_NAICS_cols = c('1997 NAICS Code', '2002 NAICS Code', '2007 NAICS Code', '2012 NAICS Code', '2017 NAICS Code')
    
    crosswalk_to_future_codes = melt(wide_crosswalk[, ..all_NAICS_cols],
                                    measure.vars = all_NAICS_cols[all_NAICS_cols != my_start_var],
                                    id.vars = my_start_var,
                                    variable.name = 'year_of_code',
                                    value.name = 'future_code'
                                   )[, year := as.numeric(substr(year_of_code, 1, 4))
                                   ][, NAICS_start := get(my_start_var)
                                   ][year >= year_of_change, .(NAICS_start, NAICS_end = future_code, vintageYear = year)
                                   ][, minVintage := vintageYear == min(vintageYear), .(NAICS_start, NAICS_end)
                                   ][minVintage == TRUE, .(vintageYear, NAICS_start, NAICS_end)]
    
    firm_specific_crosswalks = foreach(
        code_to_start_from = unique(crosswalk_to_future_codes$NAICS_start),
        .combine = rbind,
        .packages = c('data.table')
      ) %dopar% {
    
        potential_NAICS = crosswalk_to_future_codes[NAICS_start == code_to_start_from]
    
        firm_crosswalk = copy(actual_NAICS_in_Compustat)
        setkey(firm_crosswalk, GlobalCompanyKey)
        firm_crosswalk[
          (DataYearFiscal == year_of_change - 1 | DataYearFiscal == year_of_change - 2) &
            NAICSHistorical_with_firm_crosswalk_updates == code_to_start_from,
          was_in_industry := TRUE
        ][
          firm_crosswalk[was_in_industry == TRUE, .(GlobalCompanyKey, follow_these = TRUE)],
                       on = 'GlobalCompanyKey',
                       follow_these := i.follow_these
        ][
          follow_these == TRUE & DataYearFiscal >= year_of_change,
          found_NAICS_matching_potential := NAICSHistorical_with_firm_crosswalk_updates %in% potential_NAICS[, NAICS_end]
        ][
              found_NAICS_matching_potential == TRUE,
              common_subsequent_NAICS_among_potential := .N == max(.N),
              .(NAICSHistorical_with_firm_crosswalk_updates, GlobalCompanyKey)
        ][
          common_subsequent_NAICS_among_potential == TRUE,
          is_modal_subsequent_NAICS := DataYearFiscal == suppressWarnings(min(DataYearFiscal)), #min gives a warning if there are 0 obs
          GlobalCompanyKey
        ][
          firm_crosswalk[is_modal_subsequent_NAICS == TRUE],
          on = 'GlobalCompanyKey',
          future_matching_NAICS := i.NAICSHistorical_with_firm_crosswalk_updates
        ]
        
        firm_crosswalk[NAICSHistorical_with_firm_crosswalk_updates == code_to_start_from & !is.na(future_matching_NAICS),
                       .(GlobalCompanyKey, DataYearFiscal, NAICSHistorical_with_firm_crosswalk_updates, future_matching_NAICS)
                     ][potential_NAICS, on = c(future_matching_NAICS = 'NAICS_end'), NAICSVintage := i.vintageYear]
      }
    
    data[firm_specific_crosswalks,
         on = c('GlobalCompanyKey', 'DataYearFiscal'),
         `:=`(future_matching_NAICS = i.future_matching_NAICS, NAICSVintage = i.NAICSVintage)
       ][!is.na(future_matching_NAICS), NAICSHistorical_with_firm_crosswalk_updates := future_matching_NAICS]
    NULL
  
  }
  NULL
}

dtcut[, NAICSHistorical_with_firm_crosswalk_updates := NAICSHistorical]
go_through_firm_specific_crosswalks(seq(2002, 2017, by = 5), dtcut, crosswalk97_17)


dtcut[, SIC := SICHistorical]
dtcut[is.na(SIC), SIC := currentSIC]
dtcut[, NAICS := NAICSHistorical_with_firm_crosswalk_updates]
dtcut[is.na(NAICS) & is.na(SICHistorical) & !is.na(currentNAICS), NAICSVintage := max(DataYearFiscal), GlobalCompanyKey
    ][is.na(NAICS) & is.na(SICHistorical), NAICS := currentNAICS]
# There are only 41 obs with SICHistorical but not NAICSHistorical.
# I give SICHistorical precedence over currentNAICS in those cases.

dtcut[is.na(NAICSVintage), NAICSVintage := DataYearFiscal
    ][, duplicate_NAICSVintage := NAICSVintage]

setkey(dtcut, NAICS, NAICSVintage, duplicate_NAICSVintage)
setkey(defunct_NAICS_to_2017_crosswalk_final, NAICS_start, applicable_beginning, applicable_ending)

with_stochastic_crosswalk_for_defunct_NAICS = foverlaps(dtcut, defunct_NAICS_to_2017_crosswalk_final)
with_stochastic_crosswalk_for_defunct_NAICS[, splits := .N, .(GlobalCompanyKey, DataYearFiscal)]
setnames(with_stochastic_crosswalk_for_defunct_NAICS, c('NAICS_end', 'NAICS'), c('NAICS', 'NAICS_before_updating'))
tictoc::toc()
nrow(with_stochastic_crosswalk_for_defunct_NAICS)
tictoc::tic()

# You look in the wrong place here:
# with_stochastic_crosswalk_for_defunct_NAICS[calendaryear == 2019 & NAICS_before_updating == 110000, .(conm, NAICS, NAICS_before_updating, ratio)]
# This is based off of all the firms with two digit NAICS == 110000 in 2015 and 2016, on the assumption that their NAICS code was no longer 110000 come 2017
# But if it isn't in your list for 2012 either, then you want to look more broadly for what industries firms with two digit NAICS = 11 are in
# The other failing is that you're ignoring firms that were, say in 111000, which should lead you to weight towards 111 three digit codes.

# Impute higher digit NAICS -----------------------------------
# 10 min runtime


every_start_and_interval = CJ.dt(data.table(year_of_validity_start = c(-Inf, 1990, 2002, 2007, 2012, 2017),
                                            year_of_validity_end = c(1989, 2001, 2006, 2011, 2016, Inf)),
                                 data.table(NAICS_start = unique(defunct_NAICS_to_2017_crosswalk[, NAICS_start])))

setkey(every_start_and_interval, NAICS_start, year_of_validity_start, year_of_validity_end)
setkey(defunct_NAICS_to_2017_crosswalk, NAICS_start, applicable_beginning, applicable_ending)
crosswalk_with_every_interval_for_low_digit_matching_split_out = foverlaps(every_start_and_interval, defunct_NAICS_to_2017_crosswalk
                                                                         )[, n_targets := .N, .(NAICS_end, year_of_validity_start, year_of_validity_end)]




with_less_than_six_digit_update = copy(with_stochastic_crosswalk_for_defunct_NAICS)

foreach(number_of_nonzero_digits = 5:2) %do% {

  n_digit_NAICS_to_2017_crosswalk = unique(copy(crosswalk_with_every_interval_for_low_digit_matching_split_out
                                              )[, n_digits := number_of_nonzero_digits
                                              ][, NAICS_start := as.numeric(str_pad(substr(as.character(NAICS_start), 1, n_digits), width = 6, side = 'right', pad = '0'))
                                              ][!NAICS_start %in% defunct_NAICS_to_2017_crosswalk$NAICS_start
                                              ][, .(NAICS_start, NAICS_end, n_targets, year_of_validity_start, year_of_validity_end)])
  
  
  
  
  
  actual_2017_NAICS_in_Compustat = with_less_than_six_digit_update[, .(GlobalCompanyKey, DataYearFiscal,
                                                                       NAICSHistorical, NAICS, ratio)]
  
  
  n_digit_general_crosswalk = foreach(
    code_that_splits = iter(unique(n_digit_NAICS_to_2017_crosswalk[,
                                                                   .(code = NAICS_start,
                                                                     year_of_validity_start,
                                                                     year_of_validity_end)]),
                            by = 'row'),
    .combine = rbind
    ) %dopar% {
                    
      potential_NAICS = n_digit_NAICS_to_2017_crosswalk[NAICS_start == code_that_splits$code &
                                              year_of_validity_start == code_that_splits$year_of_validity_start,
                                            .(NAICS_start, NAICS_end, n_targets, year_of_validity_start, year_of_validity_end)]
      to_find_firms_with_codes_that_split = copy(actual_2017_NAICS_in_Compustat)
      to_find_firms_with_codes_that_split[(DataYearFiscal >= code_that_splits$year_of_validity_start & DataYearFiscal <= code_that_splits$year_of_validity_end) &
                                            NAICSHistorical %inIndustryCode% code_that_splits$code,
                                          was_in_industry := TRUE
                                        ][to_find_firms_with_codes_that_split[was_in_industry == TRUE, .(GlobalCompanyKey, follow_these = TRUE)],
                                          on = 'GlobalCompanyKey',
                                          follow_these := i.follow_these
                                        ][follow_these == TRUE & (DataYearFiscal >= code_that_splits$year_of_validity_start &
                                                                    DataYearFiscal <= code_that_splits$year_of_validity_end),
                                          found_NAICS_matching_potential := NAICS %in% potential_NAICS[, NAICS_end]]
      
      
      #if you didnt find NAICS matching potential, try again without the year bounds
      if (nrow(to_find_firms_with_codes_that_split[found_NAICS_matching_potential == TRUE]) == 0) {
        to_find_firms_with_codes_that_split[NAICSHistorical %inIndustryCode% code_that_splits$code,
                                            was_in_industry := TRUE
                                          ][to_find_firms_with_codes_that_split[was_in_industry == TRUE, .(GlobalCompanyKey, follow_these = TRUE)],
                                            on = 'GlobalCompanyKey',
                                            follow_these := i.follow_these
                                          ][follow_these == TRUE,
                                            found_NAICS_matching_potential := NAICS %in% potential_NAICS[, NAICS_end]]
      }
      
      if (nrow(to_find_firms_with_codes_that_split[found_NAICS_matching_potential == TRUE]) == 0) {
        ratiotable = potential_NAICS
        ratiotable[, relative_weight := 1 / n_targets
                 ][, general_crosswalk_ratio := relative_weight / sum(relative_weight)
                 ][, relative_weight := NULL]
      } else {
    
        has_matching_six_digit_NAICS =
          to_find_firms_with_codes_that_split[found_NAICS_matching_potential == TRUE, common_subsequent_NAICS_among_potential := .N == max(.N), .(NAICS, GlobalCompanyKey)
                                            ][common_subsequent_NAICS_among_potential == TRUE, is_modal_subsequent_NAICS := DataYearFiscal == max(DataYearFiscal), GlobalCompanyKey
                                            ][to_find_firms_with_codes_that_split[is_modal_subsequent_NAICS == TRUE,
                                                                                  .(GlobalCompanyKey, DataYearFiscal, year_of_modal_subsequent_NAICS = TRUE)],
                                              on = .(GlobalCompanyKey, DataYearFiscal),
                                              year_of_modal_subsequent_NAICS := i.year_of_modal_subsequent_NAICS
                                            ][year_of_modal_subsequent_NAICS == TRUE
                                            ][potential_NAICS, on = c(NAICS = 'NAICS_end')]
        
        ratiotable = has_matching_six_digit_NAICS[,
                                                  .(general_crosswalk_ratio = sum(ratio)/sum(has_matching_six_digit_NAICS$ratio)),
                                                  .(NAICS, NAICS_start, year_of_validity_start, year_of_validity_end)
                                                ][, NAICS_end := NAICS]
        
        # [year_of_modal_subsequent_NAICS == TRUE, .(num_firms = sum(ratio)), NAICS
        #                                     ][, general_crosswalk_ratio := num_firms / sum(num_firms)]
        # ratiotable = potential_NAICS[empirical_ratios, on = c(NAICS_end = 'NAICS'), general_crosswalk_ratio := i.general_crosswalk_ratio
        #                            ][is.na(general_crosswalk_ratio), general_crosswalk_ratio := 0]
      }
        ratiotable[, .(NAICS_start,
                       general_crosswalk_updated_six_digit_NAICS = NAICS_end,
                       general_crosswalk_ratio,
                       year_of_validity_start,
                       year_of_validity_end)]
    }
  
  
  
  #Firm-specific 
  
  n_digit_firm_crosswalk = foreach(
    code_that_splits = unique(n_digit_NAICS_to_2017_crosswalk$NAICS_start),
    .combine = rbind
    ) %dopar% {
               
      potential_NAICS = n_digit_NAICS_to_2017_crosswalk[NAICS_start == code_that_splits,
                                                        .(NAICS_start, NAICS_end, n_targets,
                                                          year_of_validity_start, year_of_validity_end,
                                                          found_NAICS_matching_potential = TRUE)]
  
      setkey(potential_NAICS, NAICS_end, year_of_validity_start, year_of_validity_end)
      
      to_find_firms_with_codes_that_split = copy(actual_2017_NAICS_in_Compustat)
      # to_find_firms_with_codes_that_split = data.table(GlobalCompanyKey = '999999',
      #                                                  DataYearFiscal = c(2011, 2011, 2012, 2013, 2014, 2014, 2014),
      #                                                  NAICSHistorical = c(999999, 999999, 210000, 210000, 999999, 999999, 999999),
      #                                                  NAICS = c(212291, 212299, 210000, 210000, 212311, 212312, 212313),
      #                                                  ratio = c(0.7, 0.3, 1, 1, 0.4, 0.3, 0.3))
      # to_find_firms_with_codes_that_split = data.table(GlobalCompanyKey = '999999',
      #                                                  DataYearFiscal = c(2011, 2011, 2012, 2013, 2014, 2014, 2014),
      #                                                  NAICSHistorical = c(999999, 999999, 210000, 210000, 999999, 999999, 999999),
      #                                                  NAICS = c(999999, 999999, 210000, 210000, 999999, 999999, 999999),
      #                                                  ratio = c(0.7, 0.3, 1, 1, 0.4, 0.3, 0.3))
      to_find_firms_with_codes_that_split[code_that_splits == NAICSHistorical,
                                          was_in_industry := TRUE
                                        ][to_find_firms_with_codes_that_split[was_in_industry == TRUE, .(GlobalCompanyKey, follow_these = TRUE)],
                                          on = 'GlobalCompanyKey',
                                          follow_these := i.follow_these
                                        ][, duplicateDataYearFiscal := DataYearFiscal
                                        ][, uniqueid := .I
                                        ]
      NAICS_matching_potential = foverlaps(to_find_firms_with_codes_that_split[follow_these == TRUE],
                                           potential_NAICS,
                                           by.x = c('NAICS', 'DataYearFiscal', 'duplicateDataYearFiscal'),
                                           by.y = c('NAICS_end', 'year_of_validity_start', 'year_of_validity_end'),
                                           nomatch = 0
                                          )[, .(uniqueid, found_NAICS_matching_potential)]
      if (nrow(NAICS_matching_potential) == 0) {
        data.table(GlobalCompanyKey = character(),
                   DataYearFiscal = double(),
                   NAICSHistorical = double(),
                   firm_crosswalk_updated_six_digit_NAICS = double(),
                   firm_crosswalk_ratio = double())
      } else {
        to_find_firms_with_codes_that_split[NAICS_matching_potential,
                                            on = .(uniqueid),
                                            found_NAICS_matching_potential := i.found_NAICS_matching_potential
                                          ]
  
        # If 1992 has code 110000, 1991 has codes 111111 and 111112 with ratio = 0.5 each, and 1993 has codes 112222 and 112223 with ratio = 0.5 each.
        # I take both codes from 1993 (I arbitrarily choose the later one if two are equally near)
        to_find_firms_with_codes_that_split[was_in_industry == TRUE,
                                            year_of_best_value_for_n_digit_code :=
                                              to_find_firms_with_codes_that_split[found_NAICS_matching_potential == TRUE
                                                                                ][to_find_firms_with_codes_that_split[was_in_industry == TRUE],
                                                                                  duplicateDataYearFiscal,
                                                                                  on = c('GlobalCompanyKey', 'DataYearFiscal'),
                                                                                  roll = 'nearest']
                                          ]
        to_find_firms_with_codes_that_split[is.na(found_NAICS_matching_potential)
                                          ][to_find_firms_with_codes_that_split[,
                                                                                .(GlobalCompanyKey, current_iter_code = code_that_splits,
                                                                                  firm_crosswalk_updated_six_digit_NAICS = NAICS, firm_crosswalk_ratio = ratio, DataYearFiscal)],
                                            on = c('GlobalCompanyKey', NAICSHistorical = 'current_iter_code', year_of_best_value_for_n_digit_code = 'DataYearFiscal')
                                          ][, .(GlobalCompanyKey, DataYearFiscal, NAICSHistorical, firm_crosswalk_updated_six_digit_NAICS, firm_crosswalk_ratio)
                                          ]
         
      }
    }

  with_firm_crosswalk_update = n_digit_firm_crosswalk[, .(GlobalCompanyKey, DataYearFiscal, firm_crosswalk_updated_six_digit_NAICS, firm_crosswalk_ratio)
                                                    ][with_less_than_six_digit_update,
                                                      on = .(GlobalCompanyKey, DataYearFiscal)
                                                    ][, missing_firm_crosswalk_update := is.na(firm_crosswalk_updated_six_digit_NAICS)
                                                    ]
  
  n_digit_general_crosswalk[, missing_firm_crosswalk_update := TRUE]
  n_digit_general_crosswalk_final = n_digit_general_crosswalk[general_crosswalk_ratio > 0]
  setkey(n_digit_general_crosswalk_final, NAICS_start,     missing_firm_crosswalk_update, year_of_validity_start, year_of_validity_end)
  setkey(with_firm_crosswalk_update,      NAICSHistorical, missing_firm_crosswalk_update, NAICSVintage,           duplicate_NAICSVintage)
  
  with_less_than_six_digit_update = foverlaps(with_firm_crosswalk_update, n_digit_general_crosswalk_final)
  
  stopifnot(nrow(with_less_than_six_digit_update[ratio != 1 & missing_firm_crosswalk_update == FALSE]) == 0)
  stopifnot(nrow(with_less_than_six_digit_update[ratio != 1 & !is.na(general_crosswalk_updated_six_digit_NAICS)]) == 0)
  
  with_less_than_six_digit_update[missing_firm_crosswalk_update == FALSE,
                                  `:=`(NAICS = firm_crosswalk_updated_six_digit_NAICS, ratio = firm_crosswalk_ratio)
                                ][missing_firm_crosswalk_update == TRUE & !is.na(general_crosswalk_updated_six_digit_NAICS),
                                  `:=`(NAICS = general_crosswalk_updated_six_digit_NAICS, ratio = general_crosswalk_ratio)
                                ][, c('missing_firm_crosswalk_update', 'firm_crosswalk_updated_six_digit_NAICS',
                                      'firm_crosswalk_ratio', 'general_crosswalk_updated_six_digit_NAICS',
                                      'general_crosswalk_ratio', 'year_of_validity_start', 'year_of_validity_end') := NULL]
  NULL
  
}

tictoc::toc()
nrow(with_less_than_six_digit_update)

# Impute missing NAICS codes using SIC codes ----------------------
# 20 min runtime
# (because I decided to give every year its own 10 year window)
tictoc::tic()

SICs = unique(with_less_than_six_digit_update[, .(SIC, obsYear = DataYearFiscal)]
            )[, rolling_window_start_year := pmax(pmin(obsYear - 5, max(obsYear) - 10), min(obsYear))
            ][, rolling_window_end_year := pmax(pmin(obsYear + 5, max(obsYear)), min(obsYear) + 20)]


actual_2017_NAICS_in_Compustat = with_less_than_six_digit_update[, .(GlobalCompanyKey, DataYearFiscal,
                                                                     SIC, NAICSHistorical, NAICS, ratio)]

SIC_to_NAICS_crosswalk = foreach(
  code_that_splits = iter(unique(SICs[,
                                      .(code = SIC,
                                        obsYear,
                                        rolling_window_start_year,
                                        rolling_window_end_year)]),
                          by = 'row'),
  .combine = rbind
  ) %dopar% {
    
    to_find_firms_with_codes_that_split = copy(actual_2017_NAICS_in_Compustat)
    to_find_firms_with_codes_that_split[DataYearFiscal >= code_that_splits$rolling_window_start_year &
                                          DataYearFiscal <= code_that_splits$rolling_window_end_year &
                                          SIC %inIndustryCode% code_that_splits$code &
                                          !is.na(NAICS),
                                        found_NAICS_matching_potential := TRUE]
    
    if (nrow(to_find_firms_with_codes_that_split[found_NAICS_matching_potential == TRUE]) == 0) {
      to_find_firms_with_codes_that_split[SIC %inIndustryCode% code_that_splits$code &
                                            !is.na(NAICS),
                                          found_NAICS_matching_potential := TRUE]
    }
    if (nrow(to_find_firms_with_codes_that_split[found_NAICS_matching_potential == TRUE]) == 0) {
      ratiotable = data.table(SIC_start = double(),
                              NAICS_end = double(),
                              general_crosswalk_ratio = double(),
                              year_of_validity = double())
    } else {
  
      ratiotable =
        to_find_firms_with_codes_that_split[found_NAICS_matching_potential == TRUE, common_subsequent_NAICS_among_potential := .N == max(.N), .(NAICS, GlobalCompanyKey)
                                          ][common_subsequent_NAICS_among_potential == TRUE, is_modal_subsequent_NAICS := DataYearFiscal == max(DataYearFiscal), GlobalCompanyKey
                                          ][to_find_firms_with_codes_that_split[is_modal_subsequent_NAICS == TRUE,
                                                                                .(GlobalCompanyKey, DataYearFiscal, year_of_modal_subsequent_NAICS = TRUE)],
                                            on = .(GlobalCompanyKey, DataYearFiscal),
                                            year_of_modal_subsequent_NAICS := i.year_of_modal_subsequent_NAICS
                                          ][year_of_modal_subsequent_NAICS == TRUE, .(num_firms = sum(ratio)), NAICS
                                          ][, general_crosswalk_ratio := num_firms / sum(num_firms)
                                          ][, .(SIC_start = code_that_splits$code, NAICS_end = NAICS, general_crosswalk_ratio, year_of_validity = code_that_splits$obsYear)]
    }
      ratiotable[, .(SIC_start,
                     general_crosswalk_updated_six_digit_NAICS = NAICS_end,
                     general_crosswalk_ratio,
                     year_of_validity)]
  }
stopCluster(clusters)


SIC_to_NAICS_crosswalk[, missing_NAICS := TRUE]
SIC_to_NAICS_crosswalk_final = SIC_to_NAICS_crosswalk[general_crosswalk_ratio > 0]

with_less_than_six_digit_update[, missing_NAICS := is.na(NAICS)]
with_NAICS_imputed = SIC_to_NAICS_crosswalk_final[with_less_than_six_digit_update,
                                                  on = c(SIC_start = 'SIC',
                                                         year_of_validity = 'DataYearFiscal',
                                                         'missing_NAICS'),
                                                  allow.cartesian = TRUE
                                                ][!is.na(general_crosswalk_updated_six_digit_NAICS),
                                                  `:=`(NAICS = general_crosswalk_updated_six_digit_NAICS, ratio = general_crosswalk_ratio)
                                                ][, c('missing_NAICS', 'general_crosswalk_updated_six_digit_NAICS', 'general_crosswalk_ratio') := NULL
                                                ][, SIC := SIC_start
                                                ][, DataYearFiscal := year_of_validity
                                                ]

tictoc::toc()
nrow(with_NAICS_imputed)

with_NAICS_imputed[, `:=`(NAICS2 = roundDown(NAICS, 10000),
                          NAICS3 = roundDown(NAICS, 1000),
                          NAICS4 = roundDown(NAICS, 100),
                          NAICS5 = roundDown(NAICS, 10),
                          NAICS6 = NAICS)]
names(with_NAICS_imputed)
cols_to_delete = c('SIC_start', 'year_of_validity', 'NAICSHistorical', 'SICHistorical', 'NAICS_before_updating',
                   'applicable_beginning', 'applicable_ending', 'year_of_NAICS_change', 'NAICSHistorical_with_firm_crosswalk_updates',
                   'future_matching_NAICS', 'NAICSVintage', 'duplicate_NAICSVintage', 'splits', 'currentSIC', 'currentNAICS')

with_NAICS_imputed[, names(with_NAICS_imputed)[names(with_NAICS_imputed) %in% cols_to_delete] := NULL]
write_feather(with_NAICS_imputed, 'IntermediateFiles/With NAICS Imputed.feather')
