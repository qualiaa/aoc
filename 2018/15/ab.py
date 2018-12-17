#!/usr/bin/env python3

import sys
from itertools import groupby
from operator import attrgetter, methodcaller
from functools import partial

part = 1
elf_attack=3

class Positionable:
    def __init__(s,x,y):
        s.x = x
        s.y = y
    def __lt__(s,o):
        if s.y == o.y:
            return s.x < o.x
        return s.y < o.y

class Unit(Positionable):
    ELF=0
    GOBLIN=1
    def __init__(s,x,y,m,c):
        super().__init__(x,y)
        s.map = m
        s.type = Unit.ELF if c == 'E' else Unit.GOBLIN
        s.attack_power = 3 if s.type == Unit.GOBLIN else elf_attack
        s.hp = 200
        s.alive = True

    def turn(s):
        if s.alive and not s.attack():
            if s.move():
                s.attack()

    def move(s):
        targets = s.find_targets()
        if targets:
            target_coords = [(t.x, t.y)
                    for ts in [u.get_surrounding_tiles() for u in targets]
                    for t in ts if t.empty()]
            path = Path.find(s.map, (s.x,s.y), target_coords)
            if path:
                s.map[s.y][s.x].remove(s)
                next_tile = path.first_tile()
                s.x = next_tile.x
                s.y = next_tile.y
                next_tile.add(s)
                return True
        else:
            s.map.finished_early = True
        return False

    def attack(s):
        surrounding_enemies = s.get_surrounding_enemies()
        if surrounding_enemies:
            target = sorted(surrounding_enemies, key=attrgetter("hp","y","x"))[0]
            target.take_damage(s.attack_power)
            return True
        return False

    def take_damage(s,n):
        s.hp -= n
        if s.hp <= 0:
            s.die()

    def get_surrounding_tiles(s):
        return [s.map[s.y-1][s.x], s.map[s.y+1][s.x],
                s.map[s.y][s.x-1], s.map[s.y][s.x+1]]

    def get_surrounding_enemies(s):
        return [t.contents for t in s.get_surrounding_tiles() 
                if type(t.contents) is Unit and t.contents.type != s.type]

    def find_targets(s):
        return [u for u in s.map.units if u.type != s.type and u.alive]

    def die(s):
        s.map[s.y][s.x].remove(s)
        s.alive = False
        if part == 2 and s.type == Unit.ELF:
            s.map.elf_death = True

class Tile(Positionable):
    WALL=0
    def __init__(s, x, y, contents=None):
        s.contents = contents
        super().__init__(x,y)
    
    def empty(s):
        return s.contents is None

    def remove(s,o):
        if s.contents is o:
            s.contents = None

    def add(s,o):
        s.contents = o

    # flood fill
    def distance_map(s, distance=0, prev_coord=None, results=None):
        if results == None:
            results = {}

        # update current position
        lastShortest,lastTiles = results.get((s.x,s.y), (sys.maxsize, []))
        if lastShortest < distance:
            return
        elif lastShortest == distance:
            lastTiles.append(prev_coord)
            return
        else:
            results[(s.x,s.y)] = (distance,[prev_coord])

        # recurse through linked nodes
        [t.distance_map(distance+1, (s.x, s.y), results)
                for t in [s.left, s.right, s.up,   s.down]
                if t.empty()]

        # if we are the first node, return results
        if distance == 0: return results



class Map:
    def __init__(s,map_strings):
        s.units = []
        s.map = []
        s.finished_early = False
        s.elf_death = False
        for y, row in enumerate(map_strings):
            s.map.append([])
            for x, c in enumerate(row.strip()):
                if c == "#":
                    v = Tile(x,y,Tile.WALL)
                elif c in "GE":
                    u = Unit(x,y,s,c)
                    v = Tile(x,y,u)
                    s.units.append(u)
                elif c == ".":
                    v = Tile(x,y)
                s.map[y].append(v)
        s._link_tiles()

    def _link_tiles(s):
        #link = lambda x: None if x.contents == Tile.WALL else x
        link = lambda x: x
        for y in range(1,len(s.map)-1):
            for x in range(1,len(s.map[0])-1):
                s.map[y][x].up    = link(s.map[y-1][x])
                s.map[y][x].down  = link(s.map[y+1][x])
                s.map[y][x].left  = link(s.map[y][x-1])
                s.map[y][x].right = link(s.map[y][x+1])

    def round(s):
        for u in sorted(s.units):
            u.turn()
        s.units = [u for u in s.units if u.alive]

        return not s.finished_early

    def _finished(s):
        finished = s.finished_early or len(list(groupby(s.units,attrgetter("type")))) < 2
        if part == 1:
            return finished
        if part == 2:
            return s.elf_death or finished

    def __str__(s):
        l = []
        for y in s.map:
            l.append([])
            for x in y:
                if x.contents == None:
                    l[-1].append(".")
                elif type(x.contents) is Unit:
                    l[-1].append("G" if x.contents.type == Unit.GOBLIN else "E")
                elif x.contents == Tile.WALL:
                    l[-1].append("#")
        return "\n".join(["".join(row) for row in l])

    def __getitem__(s,y):
        return s.map[y]

class Path:
    def create(m, dmap, target_coord):
        dist, prev_coords = dmap[target_coord]

        paths = map(partial(Path,m), Path._follow(dmap, target_coord))
        path = sorted(paths)[0]
        return path

    def find(m, start, target_coords):
        sx,sy = start
        dmap = m[sy][sx].distance_map()

        min_dist = sys.maxsize
        min_coords = []
        for c in target_coords:
            dist = dmap.get(c)
            if dist:
                if dist[0] < min_dist:
                    min_coords = [c]
                    min_dist = dist[0]
                elif dist[0] == min_dist:
                    min_coords.append(c)

        if min_coords:
            target_pos = min([Positionable(*x) for x in min_coords])
            path = Path.create(m, dmap, (target_pos.x, target_pos.y))
            return path
        return None

    def _follow(dmap, current_coord):
        dist, prev_coords = dmap[current_coord]
        
        if dist == 1:
            return [[current_coord]]

        results = []
        for c in prev_coords:
            for path in Path._follow(dmap, c):
                path.append(current_coord)
                results.append(path)

        return results

    def __init__(s, m, path):
        s.map = m
        s.path = path

    def first_tile(s):
        x, y = s.path[0]
        return s.map[y][x]

    def __len__(s):
        return len(s.path)

    def __lt__(s,o):
        if len(s) == len(o):
            return s.first_tile() < o.first_tile()
        return len(s) < len(o)

map_input = sys.stdin.readlines()
m = Map(map_input)

num_rounds = 0
while not m._finished():
    if m.round():
        num_rounds += 1

total_hp = 0
for u in m.units:
    total_hp += u.hp

print(total_hp*num_rounds)

part = 2

elves_die = True
while elves_die:
    elf_attack += 1
    m = Map(map_input)
    num_rounds = 0
    while not m._finished():
        if m.round():
            num_rounds += 1
    if not m.elf_death: elves_die = False

total_hp = 0
for u in m.units:
    total_hp += u.hp

print(total_hp*num_rounds)
