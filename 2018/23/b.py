#!/usr/bin/env python3

import re
import sys
from functools import partial as p
from operator import sub, attrgetter
from itertools import starmap

import numpy as np
from PQ import PQ
from PartitionTree import partition_tree

def distance(p1,p2):
    return sum(map(abs,starmap(sub, zip(p1,p2))))

class Sphere:
    def __init__(s, line):
        s.centre = tuple(map(int,re.search("<(-?\d+),(-?\d+),(-?\d+)>",line).groups()))
        s.r = int(re.search("=(\d+)",line).groups()[0])

def clamp(v,vmin,vmax):
    if v < vmin: return vmin
    return v if vmin < vmax else vmax

def intersect_circle_circle(c1, c2):
    return c1.r + c2.r >= distance(c1.centre, c2.centre)

def intersect_aabb_circle(aabb, c):
    closest_point = starmap(clamp, zip(c.centre, aabb.min_vert, aabb.max_vert))
    return c.r >= distance(closest_point, c.centre)


"""
def intersectsAll(d1, ds):
    return all(map(p(intersects, d1), ds))


def allIntersect(ds):
    for i,d1 in enumerate(ds):
        if not all(intersect(d1,d2) for d2 in ds[i:])
        for d2 in ds[i:]:
            if return 
"""


drones = [Sphere(l) for l in sys.stdin]

Tree = partition_tree(pos_fn=attrgetter("centre"), membership_fn=intersect_aabb_circle)

to_split = PQ([(Tree(drones), len(drones))])

while to_split:
    node,_ = to_split.popitem()
    print(len(to_split))
    children = node.split()
    for c in children:
        print(c)
    to_split.update({c: len(c) for c in children if not c.done()})

