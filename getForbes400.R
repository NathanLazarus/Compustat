#get Forbes 400
#devtools::install_github("abresler/forbesListR")
library(forbesListR)
library(magrittr)
library(stringr)
library(dplyr)
library(jsonlite)
library(foreach)

setwd('C:/Users/Nathan/Downloads/Racial Wealth Gap SCF')

rbind_and_fill = function(...) rbind(...,fill=T)

forbes400panel = foreach(yr = 2004:2020, .combine = rbind_and_fill, .multicombine = T, .maxcombine = 100)%do%{
  data.table(get_year_forbes_list_data(list = "Forbes 400", year = yr))
}

fwrite(forbes400panel, 'forbes400_2004_to_2020.csv')
