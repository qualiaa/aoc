#!/usr/bin/env python3

import re
import sys

import numpy as np
from operator import itemgetter
from collections import OrderedDict, UserDict

buffer = 150 # hacky so shoot me
depth_str, target_str = re.findall("[0-9,]+", sys.stdin.read())

depth = int(depth_str)
ix,iy = [int(s) for s in target_str.split(",")]

vals = np.empty((iy+buffer,ix+buffer),dtype=np.int64)

erosion = lambda x: (x + depth) % 20183

vals[0]   = [erosion(i * 16807) for i in range(vals.shape[1])]
vals[:,0] = [erosion(i * 48271) for i in range(vals.shape[0])]

for y in range(1,vals.shape[0]):
    for x in range(1,vals.shape[1]):
        vals[y,x] = erosion(int(vals[y-1,x]) * int(vals[y,x-1]))

vals[iy,ix] = erosion(0)
vals %= 3

print(vals[:iy+1,:ix+1].sum())

ROCK   = 0
WET    = 1
NARROW = 2

cave = np.stack([
    np.logical_or(vals == ROCK, vals == NARROW), # torch
    np.logical_or(vals == ROCK, vals == WET),    # climbing
    np.logical_or(vals == WET,  vals == NARROW), # neither
    
])

start  = (0,0,0)
target = (0,iy,ix)

def cost(c,t):
    c = np.array(c)
    return np.abs(c[1:] - t[1:]).sum() + 7*(c[0] != t[0])

def valid(t):
    return t[1] >= 0  and t[2] >= 0 and cave[t]

def neighbours(c):
    c = np.array(c)
    return [tuple(c + d) for d in [
        [0, 1, 0],
        [0,-1, 0],
        [0, 0, 1],
        [0, 0,-1]
    ]] + [((c[0] + i) % 3, *c[1:]) for i in (1,2)]

class PQ (UserDict):
    def _s(s,it): return sorted(it,key=itemgetter(1), reverse=True)
    def __init__(s,it):
        s.data = OrderedDict(s._s(it))
    def __setitem__(s,k,v):
        s.data[k] = v
        s.data = OrderedDict(s._s(s.data.items()))
    def popitem(s): return s.data.popitem()


current_tiles = PQ([
    (start, cost(start, target))
])
times = {start: 0}

while current_tiles:
    c,_ = current_tiles.popitem()
    current_time = times[c]

    if c == target:
        break

    for n in filter(valid, neighbours(c)):
        last_time = times.get(n, sys.maxsize)
        next_time = current_time + cost(c,n)
        if next_time < last_time:
            current_tiles[n] = next_time + cost(n, target)
            times[n] = next_time

print(times[target])
