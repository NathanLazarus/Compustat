library(data.table)
library(ggplot2)
library(feather)
library(stringr)
import::from(lubridate, 'fast_strptime', 'mdy')

library(foreach)
library(iterators)
library(snow)
library(doSNOW)

library(fredr)
library(RPostgres)

library(openxlsx)
library(readxl)
library(haven)
import::from(readr, 'read_fwf', 'fwf_empty')

# library(gtools)
# library(Hmisc, quietly = T, warn.conflicts = F)

# the python scripts depend on casadi and numpy
# I should learn how to use reticulate, renv::use_python
# and be able to package those dependencies with everything.