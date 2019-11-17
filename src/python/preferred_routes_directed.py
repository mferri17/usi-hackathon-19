'''
	Creates a quick analisys/ready-to-export CSV with no same departure/arrival stations
	Given stations A and B, it considers both the routes A->B and B->A
'''

import pandas as pd
from collections import Counter

cols = ['from', 'to']
df = pd.read_csv("bikes_no_NA.csv", usecols=cols) # load only the colums of departure/arrival stations

# filtered will not contain rows with same departure/arrival station
filtered = Counter(map(lambda y: (y[0],y[1]), filter(lambda x: x[0] != x[1], df.values))) 

# print data for quick analisys or to export a CSV file
print("A,B,occurences")
for x,y in filtered:
	print(f"{x},{y},{filtered[(x,y)]}")