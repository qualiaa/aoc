#!/usr/bin/env python3

from itertools import starmap
from functools import partial as p
from operator import sub
import sys

def distance(p1,p2):
    return sum(map(abs,starmap(sub, zip(p1,p2))))

def in_range(p1,p2):
    return distance(p1,p2) <= 3

def in_constellation(coord, constellation):
    return any(map(p(in_range, coord), constellation))

coords = [[*map(int,l.split(","))] for l in sys.stdin.readlines()]

constellations = []
for coord in coords:
    constellations_in_range = []

    for i, constellation in enumerate(constellations):
        if in_constellation(coord, constellation):
            constellations_in_range.append(i)

    if constellations_in_range:
        if len(constellations_in_range) > 1:
            new_constellation = [coord]
            for i in constellations_in_range[::-1]:
                new_constellation.extend(constellations[i])
                del constellations[i]
            constellations.append(new_constellation)
        else:
            constellations[constellations_in_range[0]].append(coord)
    else:
        constellations.append([coord])

print(len(constellations))
