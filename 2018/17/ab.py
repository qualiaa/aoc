#!/usr/bin/env python3

import re
import sys
from time import sleep
import numpy as np

class Water:
    active = []
    to_add=[]
    def flow():
        Water.active.extend(Water.to_add)
        Water.to_add=[]
        for w in Water.active:
            w.step()
        Water.active = [w for w in Water.active if not w.done]
        return bool(Water.active)
        
    def __init__(s,x,y,grid,seen_wall=None,seen_done=None):
        s.x = x
        s.y = y
        s.grid = grid
        s.settled = False # settled => done
        s.done = False
        s.seen_wall = seen_wall
        s.seen_done = seen_done
        Water.to_add.append(s)

    def step(s):
        below = s.look(DOWN)
        if type(below) is Water:
            if below.settled:
                s.split()
            else: # not below.settled:
                s.done = below.done

        elif below == EMPTY:
            grid[s.y+1][s.x] = Water(s.x, s.y+1, s.grid)

        elif below == WALL:
            return s.split()

        elif below is None:
            s.done = True

    def split(s):
        sides = (s.look(LEFT), s.look(RIGHT))
        if all(side == WALL for side in sides):
            s.settled = True
            s.done = True

        if all(type(side) == Water and side.done for side in sides):
            s.done = True

        s.handle_side(LEFT)
        s.handle_side(RIGHT)

    def handle_side(s,d):
        dx,_ = d
        side = s.look(d)

        if type(side) is Water:
            if s.seen_wall and side.seen_wall and side.seen_wall != s.seen_wall:
                s.settle()
            if s.seen_done and side.seen_done and side.seen_done != s.seen_done:
                s.done = True
            s.seen_wall = s.seen_wall or side.seen_wall
            s.seen_done = s.seen_done or side.seen_done

            if side.done and (s.seen_wall or (s.seen_done and s.seen_done is not s)):
                s.done = True
            elif side.done and not s.seen_done:
                s.spread_done(s)

        elif side == WALL:
            if s.seen_wall and s.seen_wall is not s:
                s.settle()
            elif not s.seen_wall:
                s.spread_wall(s)
        elif side == EMPTY:
            grid[s.y][s.x+dx] = Water(s.x+dx, s.y, s.grid, s.seen_wall)
        elif side == None:
            if s.seen_wall or (s.seen_done and s.seen_done is not s):
                s.done = True
            elif not s.seen_done:
                s.spread_done(s)


    def settle(s):
        if s.settled:
            return
        s.settled = True
        s.done = True
        [side.settle() for side in (s.look(LEFT), s.look(RIGHT))
                if type(side) is Water and not side.settled]

    def spread_wall(s,t):
        if s.seen_wall: raise RuntimeError("wuhwh")
        s.seen_wall = t
        [side.spread_wall(t) for side in (s.look(LEFT),s.look(RIGHT))
                if type(side) is Water and not side.seen_wall]

    def spread_done(s,t):
        if s.seen_done: raise RuntimeError("wuhwh")
        s.seen_done = t
        [side.spread_done(t) for side in (s.look(LEFT),s.look(RIGHT))
                if type(side) is Water and not side.seen_done and not side.done]
        
                
    def look(s,d):
        x,y = d
        x += s.x
        y += s.y
        if 0 <= x < len(grid[0]) and 0 <= y < len(grid):
            return grid[y][x]
        return None

    def __str__(s): return "~" if s.settled else "|"
    def __repr__(s): return str(s)

lines = sys.stdin.readlines()

regex_pattern = "([xy])=([0-9.]+), ([xy])=([0-9.]+)"

xys = {"x":[],"y":[]}
for l in lines:
    mo = re.match(regex_pattern, l)
    for k,v in [mo.group(1,2), mo.group(3,4)]:
        if "." in v:
            ran = [int(i) for i in v.split("..")]
            v = range(ran[0],ran[1]+1)
        else:
            v = [int(v)]
        xys[k].append(v)

xys = list(zip(*xys.values()))
xys = np.array([(x,y) for (xs,ys) in xys for x in xs for y in ys])

offset = xys.min(0) - [1,0]
xys -= offset
wh = xys.max(0) + 1 + [2,0]

water_origin = (500-offset[0], 0)
    
WALL = ','
EMPTY = '.'
SETTLED = '~'
FLOWING = '|'
DOWN  = (0,1)
UP    = (0,-1)
LEFT  = (-1,0)
RIGHT = (1,0)


grid = []
for y in range(wh[1]):
    grid.append([EMPTY]*wh[0])

for x,y in xys:
    grid[y][x] = WALL

grid[0][500-offset[0]] = Water(500-offset[0], 0, grid)

while Water.flow(): pass

print(len([w for r in grid for w in r if type(w) is Water]))
print(len([w for r in grid for w in r if type(w) is Water and w.settled]))
