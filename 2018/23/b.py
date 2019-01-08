#!/usr/bin/env python3

import sys
from functools import partial as pf
from operator import attrgetter
from itertools import product

from intersection import distance, intersect_circle_point, intersect_aabb_circle
from PQ import PQ
from PartitionTree import partition_tree
from shapes import Sphere

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

drones = [*map(Sphere.from_string, sys.stdin)]

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
