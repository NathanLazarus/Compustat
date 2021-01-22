import csv
tsv_file = open("patent.tsv")
read_tsv = csv.reader(tsv_file, delimiter="\t")
counter = 0
for row in read_tsv:
  print(row)
  counter += 1
  if counter>100:
  	break