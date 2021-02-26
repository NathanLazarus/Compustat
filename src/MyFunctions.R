rbind_and_fill = function(...) rbind(...,fill=T)


read_feather_dt = function(file) data.table(read_feather(file))


na0 = function(x) fifelse(!is.na(x), x, 0)

roundDown = function(x, y) x - x %% y

`%inIndustryCode%` = function(narrow_code, broad_code) {
  narrow_code == broad_code | roundDown(narrow_code, 10) == broad_code |
  roundDown(narrow_code, 100) == broad_code | roundDown(narrow_code, 1000) == broad_code |
  roundDown(narrow_code, 10000) == broad_code
}

CJ.dt = function(X,Y) {
  stopifnot(is.data.table(X),is.data.table(Y))
  k = NULL
  X = X[, c(k = 1, .SD)]
  setkey(X, k)
  Y = Y[, c(k = 1, .SD)]
  setkey(Y, NULL)
  X[Y, allow.cartesian = TRUE][, k := NULL][]
}


merge_and_reconcile = function(prioritized_data, deprioritized_data, join_cols, all_prioritized = T, all_deprioritized = T) {
  merged_wide_with_duplicates = merge(prioritized_data, deprioritized_data, by = join_cols, 
                                      all.x = all_prioritized, all.y = all_deprioritized, 
                                      suffixes = c('.from_prioritized', '.from_deprioritized'))
  
  dupe_cols = gsub('\\.from_prioritized', '', grep('\\.from_prioritized', names(merged_wide_with_duplicates), value = T))
  non_duplicated_cols = names(merged_wide_with_duplicates)[!grepl('\\.from_prioritized|\\.from_deprioritized',
                                                                  names(merged_wide_with_duplicates))]
  
  foreach(var = dupe_cols) %do% {
    var_prioritized = paste0(var, '.from_prioritized')
    var_deprioritized = paste0(var, '.from_deprioritized')
    merged_wide_with_duplicates[, (var) := fifelse(!is.na(get(var_prioritized)) & get(var_prioritized) != 0, get(var_prioritized), 
                                                   fifelse(!is.na(get(var_prioritized)) & get(var_prioritized) == 0, 
                                                           fifelse(!is.na(get(var_deprioritized)) & get(var_deprioritized) != 0, get(var_deprioritized), get(var_prioritized)), 
                                                           fifelse(!is.na(get(var_deprioritized)), get(var_deprioritized), get(var_prioritized))))]
    NULL
  }
  
  return(merged_wide_with_duplicates[, c(non_duplicated_cols, dupe_cols), with = F])
  
}


give_descriptive_variable_names = function(data, variable_names) {
  variable_names[, shortVarName := tolower(shortVarName)
         ][, cleanDescriptiveVarName := gsub("[^[:alnum:]]", "", fullDescriptiveVarName)]
  descriptive_variable_names_for_Data = data.table(shortVarName = tolower(names(data))
                                                 )[variable_names, on = 'shortVarName', cleanDescriptiveVarName := i.cleanDescriptiveVarName
                                                 ][!is.na(cleanDescriptiveVarName)]
  setnames(data, 
           descriptive_variable_names_for_Data[, shortVarName], 
           descriptive_variable_names_for_Data[, cleanDescriptiveVarName])
}
