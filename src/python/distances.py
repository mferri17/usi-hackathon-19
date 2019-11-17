'''
	Calculates distances between coordinates of two stations and save them into a CSV file
'''

from sys import argv
import pandas as pd
from math import sin, cos, sqrt, atan2, radians

def get_coords(c):
	coord = []
	for x in c:
		x = x.split(",")
		coord.append((radians(float(x[0])),radians(float(x[1]))))
	
	return coord

R = 6373.0

df = pd.read_csv(argv[1] + ".csv")

coord_from = get_coords(df['loc_from'])
coord_to = get_coords(df['loc_to'])

distances = []

for i in range(len(coord_from)):
	dlon = coord_to[i][1] - coord_from[i][1]
	dlat = coord_to[i][0] - coord_from[i][0]

	a = sin(dlat / 2)**2 + cos(coord_to[i][0]) * cos(coord_from[i][0]) * sin(dlon / 2)**2
	c = 2 * atan2(sqrt(a), sqrt(1 - a))
	
	distance = R * c
	distances.append(distance)

df['distances[Km]'] = distances

df.to_csv(argv[1] + "with_distances.csv")
