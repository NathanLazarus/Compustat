#!/usr/bin/env

"""
This version: January 2019

Citation: 
Autor, David, David Dorn, Gordon H. Hanson, Gary Pisano, and Pian Shu, "Foreign Competition and Domestic Innovation: Evidence from U.S. Patents," NBER Working Paper No. 22879, December 2016.

Note that the input file should be in csv and should contain the firm ID and up to four search keys (each key as a separate column). The program will insert quotation marks around each key (each key can contain multiple words) and search the keys combined (e.g., "KEY 1" "KEY 2" "KEY 3" "KEY 4"). Our matching used one key for each firm, which is the punctuation-free assignee (or firm) name.
"""

import urllib
import urllib2
import json
import time
import logging
import bing_searchweb
import csv, string, re, sys
import datetime
import time

start_index = int(sys.argv[2])
end_index = int(sys.argv[3])
input_names = open(str(sys.argv[1]), 'rU')
output_file = open('output' + '_' + str(start_index) + '_' + str(end_index) + '.csv', 'w')
reader = csv.reader(input_names)
data = [row for row in reader]
input_names.close()
writer = csv.writer(output_file)
writer.writerow(['assignee_id', 'key1', 'key2', 'key3', 'key4', 'link1', 'title1', 'description1', 'link2', 'title2', 'description2', 'link3', 'title3', 'description3', 'link4', 'title4', 'description4', 'link5', 'title5', 'description5'])

print_counter = start_index
for row in data[start_index : end_index]:
    new_row = []
    write = True
    counter = 0
    record_id = ''
    key1 = ''
    key2 = ''
    key3 = ''
    key4 = ''

    if row[0] == 'assignee_id':
        continue
    for s in row:
        if counter == 0:
            record_id = s
            counter += 1
        elif counter == 1:
            key1 = s
            counter += 1
        elif counter == 2:
            key2 = s
            counter += 1
        elif counter == 3:
            key3 = s
            counter += 1
        elif counter == 4:
            key4 = s 
            counter += 1
        else:
            break
    keywords = [key1, key2, key3, key4]
    search_str = '%s: %s, %s, %s, %s' % (record_id, key1, key2, key3, key4)

    start = 0
    ts = time.time()
    track_time = datetime.datetime.fromtimestamp(ts).strftime('%Y-%m-%d %H:%M:%S')

    print print_counter
    print "Starting searches for %s %s %s %s (%s)... this may take a while." % (key1, key2, key3, key4, record_id)

    query = "\"" + key1 + "\" \"" + key2 + "\" \"" + key3 + "\" \"" + key4 + "\"" 

    links = bing_searchweb.get_all_links(query)
    print_counter += 1

    new_row = [record_id]

    for keyword in keywords:
        new_row.append(keyword)

    for link in links:
        new_row.append(link)
    writer.writerow(new_row)
output_file.close()
 
