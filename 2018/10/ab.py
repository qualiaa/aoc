#!/usr/bin/env python3

import sys
import numpy as np

filtered_digits = [''.join(c for c in l if c in " -" or c.isdigit()).split()
                        for l in sys.stdin.readlines()]
xyvw = np.array([tuple(map(int,digit_group)) for digit_group in filtered_digits])

r = xyvw[:,1::-1]
v = xyvw[:,-1:1:-1]

def mean_distance(r):
    d = 0
    for i,coord in enumerate(r[:-1]):
        d += np.abs(np.delete(r, range(i+1),0) - coord).sum()
    return d * 2 / (r.shape[0]*(r.shape[0]-1))

# skip a load of steps with very rough optimisation
time = 0
md = mean_distance(r)
while md > 100:
    r += v * int(md//50)
    md2 = mean_distance(r)
    if md2 > md:
        r -= v*int(md//50)
        break
    time += int(md//50)
    md = md2


# iterate the last few optimisation steps 
#
# take five additional steps in case the answer does not perfectly minimise the
# metric
lastmd = md
mds = np.array([])
while len(mds) < 2 or (mds[-5:-1] > lastmd).any():
    mds = np.append(mds, [lastmd])
    lastmd = mean_distance(r + v * len(mds))

# print the arrangement of points when the distance is minimised
closest_arrangement = r + v * np.argmin(mds)
time += np.argmin(mds)

closest_arrangement -= closest_arrangement.min(0)

grid = np.full(closest_arrangement.max(0)+1, ' ')

for coord in closest_arrangement:
    grid[tuple(coord)] = '#'

for row in grid:
    for c in row:
        print(c,end='')
    print()
print (time)
