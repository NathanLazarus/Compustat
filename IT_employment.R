library(foreach)

setwd('C:/Users/Nathan/Downloads/Compustat')
acs = fread('Data/IPUMS ACS.csv')

acs[,INDNAICS := trimws(INDNAICS)]
all_codes = unique(acs[,.(INDNAICS, YEAR)])
withletters = all_codes[grepl('[A-z]',INDNAICS),.(YEAR, INDNAICS, naics = gsub('[A-z].*','',INDNAICS))]
to_drop = withletters[nchar(naics) <= 2]
acs = acs[substr(INDNAICS,1,1) != '9' & INDNAICS != '' & !INDNAICS %in% to_drop$INDNAICS]

to_aggregate = withletters[nchar(naics) > 2]
setkey(to_aggregate,naics)

all_codes[, aggregated := FALSE]
all_codes[, aggregation_because_of_letters := NA_character_]
foreach(strlen = 3:max(nchar(all_codes$INDNAICS)))%do%{
  all_codes[, n_digit_naics := substr(INDNAICS, 1, strlen)]
  all_codes[aggregated == FALSE, need_to_aggregate_now := sum(INDNAICS == n_digit_naics) > 0, .(YEAR, n_digit_naics)]
  all_codes[need_to_aggregate_now == TRUE, aggregation_because_of_short_codes := n_digit_naics]
  all_codes[need_to_aggregate_now == TRUE, `:=`(aggregated = TRUE, need_to_aggregate_now = FALSE)]
  all_codes[to_aggregate, on = c(n_digit_naics = 'naics', 'YEAR'), new_aggregation_because_of_letters := naics]
  all_codes[is.na(aggregation_because_of_letters), aggregation_because_of_letters := new_aggregation_because_of_letters]
  NULL
}

all_codes[, aggregate_to := fifelse((nchar(aggregation_because_of_letters) < nchar(aggregation_because_of_short_codes) |
                                      is.na(aggregation_because_of_short_codes)) & !is.na(aggregation_because_of_letters),
                                    aggregation_because_of_letters,
                                    aggregation_because_of_short_codes)]
all_codes[is.na(aggregate_to), aggregate_to := INDNAICS]

setkey(acs,YEAR,INDNAICS)
setkey(all_codes,YEAR,INDNAICS)
acs[all_codes, naics:=i.aggregate_to]

bessen_it_occ = c(64, 65, 229)
my_it_occ = c(110, 1220, seq(1000,1105))

#right now there are a bunch of NAICS codes in the overall averages that don't exist over the whole sample

bessen_it_by_year = acs[OCC1990 < 900,
                .(IT_employment = sum((OCC1990 %in% bessen_it_occ) * PERWT) / sum(PERWT)),
                .(naics, YEAR)]
bessen_it_averaged = acs[OCC1990 < 900,
                        .(IT_employment = sum((OCC1990 %in% bessen_it_occ) * PERWT) / sum(PERWT)),
                        naics]
my_it_by_year = acs[OCC > 0 & OCC < 9800,
                        .(IT_employment = sum((OCC %in% my_it_occ) * PERWT) / sum(PERWT)),
                        .(naics, YEAR)]
my_it_averaged = acs[OCC > 0 & OCC < 9800,
                         .(IT_employment = sum((OCC %in% my_it_occ) * PERWT) / sum(PERWT)),
                         naics]

fwrite(bessen_it_by_year, 'IntermediateFiles/IT_employment.csv', quote = T)