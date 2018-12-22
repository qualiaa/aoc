#!/usr/bin/env python3

import sys
from collections import defaultdict

sys.setrecursionlimit(4000)

dirs = {
    'N': ( 0,-1),
    'S': ( 0, 1),
    'E': ( 1, 0),
    'W': (-1, 0)
}

class Room:
    def __init__(s,x=0,y=0):
        s.links = {}

    # modified from problem 15
    def distance_map(s, distance=0, results=None):
        if results is None:
            results = {}

        # recurse through linked nodes
        for r in s.links.values():
            if results.get(r, sys.maxsize) > distance+1:
                results[r] = distance+1
                r.distance_map(distance+1, results)

        # if we are the first node, return results
        if distance == 0: return results

building = defaultdict(Room)

def move(dir, pos):
    r1 = building[pos]
    next_pos = (pos[0] + dir[0], pos[1] + dir[1])

    r2 = building[next_pos]
    r1.links[dir] = r2
    r2.links[tuple(-x for x in dir)] = r1

    return next_pos

def parse_option(input_it, coords):
    for c in input_it:
        if c in ")|":
            return coords, c
        elif c == '(':
            coords = parse_group(input_it, coords)
        elif c in "NESW":
            coords = {move(dirs[c], coord) for coord in coords}

def parse_group(input_it, start_coords):
    c = None
    new_coords = set()

    while c != ")":
        coords, c = parse_option(input_it, start_coords)
        new_coords |= coords
    return new_coords

start_coord = (0,0)
input_it = iter(sys.stdin.read())

parse_option(input_it, {start_coord})

distance_map = building[start_coord].distance_map()

print(max(distance_map.values()))
print(len([x for x in distance_map.values() if x >= 1000]))
