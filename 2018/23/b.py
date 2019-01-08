#!/usr/bin/env python3

import re
import sys
from functools import partial as pf
from operator import sub, attrgetter
from itertools import starmap, product

from PQ import PQ
from PartitionTree import partition_tree

def vect(f, *args):
    return starmap(f, zip(*args))

def distance(p1,p2):
    return sum(map(abs, vect(sub, p1, p2)))

class Sphere:
    def __init__(s, line):
        s.centre = tuple(map(int,re.search("<(-?\d+),(-?\d+),(-?\d+)>",line).groups()))
        s.r = int(re.search("=(\d+)",line).groups()[0])

def clamp(v,vmin,vmax):
    if v < vmin: return vmin
    return v if v < vmax else vmax

def intersect_circle_point(c, p):
    return c.r >= distance(c.centre, p)

def intersect_aabb_circle(aabb, c):
    closest_point = vect(clamp, c.centre, aabb.min_vert, aabb.max_vert)
    return c.r >= distance(closest_point, c.centre)




def count_intersections(point):
    return sum(map(lambda c: intersect_circle_point(c, point), drones))

def min_distance(aabb):
    return min(map(pf(distance, (0,0,0)), product(aabb.min_vert, aabb.max_vert)))

def priority(n):
    return len(n), -min_distance(n.bounds)

def find_highest_priority(aabb):
    ranges = [range(start, end+1)
              for start, end in zip(aabb.min_vert, aabb.max_vert)]
    points = product(*ranges)

    point_priorities = map(lambda point: (
            count_intersections(point), -distance((0,0,0), point)
        ), points)

    return max(point_priorities)



drones = [Sphere(l) for l in sys.stdin]


Tree = partition_tree(pos_fn=attrgetter("centre"),
                      membership_fn=intersect_aabb_circle)

node_queue = PQ([(Tree(drones), (len(drones), 0))])
best_priority = -sys.maxsize, -sys.maxsize


while node_queue:
    node, priority_metric = node_queue.popitem()

    if priority_metric < best_priority:
        break

    children = node.split()
    for c in children:
        if not c.done():
            node_queue[c] = priority(c)

        elif priority(c) >= best_priority and len(c) > Tree.max_bucket:
            best_priority = max(best_priority, find_highest_priority(c.bounds))


print(-best_priority[1])
