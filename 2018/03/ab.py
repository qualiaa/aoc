#!/usr/bin/env python3

import re
import sys
import numpy as np

class Rect:
    def __init__(self,string):
        mo = re.match("#(?P<id>\d+) @ (?P<x>\d+),(?P<y>\d+): (?P<w>\d+)x(?P<h>\d+)", string)
        if not mo: raise RuntimeError
        self.__dict__ = {k:int(v) for k,v in mo.groupdict().items()}

    def slices(s):
        return (slice(s.x, s.x + s.w), slice(s.y, s.y + s.h))
        
rects = list(map(Rect,sys.stdin.readlines()))

canvas = np.zeros((1000,1000),dtype=np.int16)
ones   = np.ones ((1000,1000),dtype=np.int16)

for r in rects:
    canvas[r.slices()] += 1

print((canvas > 1).sum())

for r in rects:
    if (canvas[r.slices()] == ones[r.slices()]).all():
        print (r.id)
        break
