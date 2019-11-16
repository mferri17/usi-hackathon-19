import pandas as pd
from collections import Counter

cols = ['from', 'to']
df = pd.read_csv("bikes_no_NA.csv", usecols=cols)

filtered = Counter(map(lambda y: (y[0],y[1]), filter(lambda x: x[0] != x[1], df.values)))

print("A,B,occurences")
for x,y in filtered:
	print(f"{x},{y},{filtered[(x,y)]}")

#df.to_csv("bikes_no_NA_filtered.csv",index=False)