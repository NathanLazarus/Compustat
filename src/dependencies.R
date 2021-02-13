library(data.table)
library(ggplot2)
library(fredr)
library(RPostgres)
library(foreach)
library(iterators)
library(snow)
library(doSNOW)
library(Hmisc, quietly = T, warn.conflicts = F)
library(stringr)
library(openxlsx)
library(readxl)
library(haven)
library(gtools)
import::from(lubridate, 'fast_strptime', 'mdy')

# the python scripts depend on casadi and numpy
# I should learn how to use reticulate, renv::use_python
# and be able to package those dependencies with everything.