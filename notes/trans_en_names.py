import csv
import sys
import os

ja2en = {}

with open('ennames.csv') as csvfh:
    for line in csv.reader(csvfh):
        ja2en[line[0]] = line[1]


print(ja2en)

with open(sys.argv[1]) as infh:
    for line in infh:
        line = line.rstrip()
        print(ja2en[line])
    

