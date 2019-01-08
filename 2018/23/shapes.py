import re
from operator import add, sub
from itertools import repeat

import numpy as np
from pygame import Color, Rect, draw

from utils import vect

class AABB:
    def __init__(s, min_vert, max_vert):
        s.min_vert = min_vert
        s.max_vert = max_vert
        s.shape = (*vect(sub,max_vert, min_vert),)

    def draw(s, surface, colour=Color(50,50,50), fill=False):
        shape = vect(add, s.shape, repeat(1))
        r = Rect(*s.min_vert, *shape)
        draw.rect(surface, colour, r, not fill)

    def move(s, disp):
        s.min_vert = (*vect(add, s.min_vert, disp),)
        s.max_vert = (*vect(add, s.max_vert, disp),)

    def move_to(s, pos):
        disp = (*vect(sub, pos, s.min_vert),)
        s.move(disp)

    def __str__(s): return f"{s.min_vert} to {s.max_vert}"

class Sphere:
    def from_string(line):
        centre = tuple(map(int,re.search("<(-?\d+),(-?\d+),(-?\d+)>",line).groups()))
        r = int(re.search("=(\d+)", line).groups()[0])
        return Sphere(centre, r)

    def __init__(s, centre, r):
        s.centre = centre
        s.r = r

    def draw(s, surface, colour=Color(50,50,50), fill=False):
        p = np.array(s.centre)
        draw.polygon(surface, colour,
                [p+(0,-s.r), p+(s.r,0), p+(0,s.r), p+(-s.r,0)], not fill)

    def move(s, disp):
        print(disp)
        s.centre = (*vect(add, s.centre, disp),)

    def move_to(s, pos):
        s.centre = pos
