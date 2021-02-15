rbind_and_fill = function(...) rbind(...,fill=T)

getCharCols = function(x) {
  second_line = readLines(x,n = 2)[2]
  cols = strsplit(second_line, ',')[[1]]
  grep('"',cols)
}

fread_and_getCharCols = function(x) {
  fread(x, colClasses = list(character = getCharCols(x)))
}

na0 = function(x) ifelse(!is.na(x),x,0)