#!/usr/bin/env python3

import time
import pygame
import sys
from abc import ABC
from functools import partial as p
from itertools import repeat, starmap
from operator import add, sub, gt

import numpy as np

from PartitionTree import AABB, partition_tree

def vect(f,*args):
    return starmap(f, zip(*args))

class Sphere:
    def __init__(s, centre, r):
        s.centre = centre
        s.r = r

    def draw(s, surface, colour=pygame.Color(50,50,50), fill=False):
        p = np.array(s.centre)
        pygame.draw.polygon(surface, colour,
                [p+(0,-s.r), p+(s.r,0), p+(0,s.r), p+(-s.r,0)], not fill)

    def move(s, disp):
        print(disp)
        s.centre = (*vect(add, s.centre, disp),)

    def move_to(s, pos):
        s.centre = pos

def _aabb_draw(s, surface, colour=pygame.Color(50,50,50), fill=False):
    shape = vect(add, s.shape, repeat(1))
    r = pygame.Rect(*s.min_vert, *shape)
    pygame.draw.rect(surface, colour, r, not fill)

def _aabb_move(s, disp):
    s.min_vert = (*vect(add, s.min_vert, disp),)
    s.max_vert = (*vect(add, s.max_vert, disp),)

def _aabb_move_to(s, pos):
    disp = (*vect(sub, pos, s.min_vert),)
    s.move(disp)


AABB.draw = _aabb_draw
AABB.move = _aabb_move
AABB.move_to = _aabb_move_to

def distance(p1,p2):
    return sum(map(abs, vect(sub, p1, p2)))

def clamp(v,vmin,vmax):
    if v < vmin: return vmin
    return v if v < vmax else vmax

def intersect_circle_circle(c1, c2):
    return c1.r + c2.r >= distance(c1.centre, c2.centre)

def intersect_circle_point(c, p):
    return c.r >= distance(c.centre, p)

def intersect_aabb_aabb(aabb1, aabb2):
    return not (any(vect(gt, aabb1.min_vert, aabb2.max_vert)) or
                any(vect(gt, aabb2.min_vert, aabb1.max_vert)))

def intersect_aabb_circle(aabb, c):
    closest_point = vect(clamp, c.centre, aabb.min_vert, aabb.max_vert)
    return c.r >= distance(closest_point, c.centre)

def intersect_aabb_point(aabb, p):
    for x, lower, upper in zip(p, aabb.min_vert, aabb.max_vert):
        if x < lower or x > upper: return False
    return True

def intersect(a,b):
    if type(a) is tuple:
        b, a = a, b
    if type(a) is Sphere:
        if type(b) is Sphere:
            return intersect_circle_circle(a,b)
        elif type(b) is AABB:
            return intersect_aabb_circle(b,a)
        elif type(b) is tuple:
            return intersect_circle_point(a,b)
    elif type(a) is AABB:
        if type(b) is AABB:
            return intersect_aabb_aabb(a,b)
        elif type(b) is Sphere:
            return intersect_aabb_circle(a,b)
        elif type(b) is tuple:
            return intersect_aabb_point(a,b)
    raise TypeError

pygame.init()
pygame.mouse.set_visible(False)

screen = pygame.display.set_mode((640, 480),0,32)
shapes = [Sphere([0,0],20), AABB((-10,-10),(10,10))]


selected = None
def select_object(point):
    global selected
    if selected:
        selected = None
    else:
        try:
            selected = next(filter(p(intersect,point), shapes))
        except:
            selected = None
            return False
    return True

PTree = partition_tree()
ptree = PTree([], bounds=AABB((0,0), screen.get_size()))

def draw_ptree(surface):
    bounds = []
    nodes = [ptree]

    while nodes:
        node = nodes.pop()
        if node.children:
            nodes.extend(node.children)
        else:
            bounds.append(node.bounds)

    global selected
    for aabb in bounds:
        col = pygame.Color(0x99,0x99,0x99)

        obj = selected if selected else pygame.mouse.get_pos()

        if intersect(aabb, obj):
            col.r = 0xFF
        aabb.draw(surface, col)

def split_ptree(point):
    leaves = []
    nodes = [ptree]
    while nodes:
        node = nodes.pop()
        if intersect(node.bounds, point):
            if node.children:
                nodes.extend(node.children)
            else:
                leaves.append(node)
    [*map(PTree.split, leaves)]



while True:
    mouse_pos = pygame.mouse.get_pos()
    mouse_disp = pygame.mouse.get_rel()

    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            sys.exit()
        if event.type == pygame.MOUSEBUTTONUP:
            select_object(mouse_pos) or split_ptree(mouse_pos)



    if selected:
        selected.move_to(mouse_pos)

    intersections = []
    hovered = []
    for i, s1 in enumerate(shapes):
        if intersect(s1, mouse_pos):
            hovered.append(s1)
        for s2 in shapes[i+1:]:
            if intersect(s1,s2):
                intersections.extend([s1,s2])

    for shape in shapes:
        col = pygame.Color(0,0,0,255)
        if shape is selected:
            col.r = 255
        elif shape in hovered:
            col.g = 255
        if shape in intersections:
            col.b = 255
        shape.draw(screen, col)

    draw_ptree(screen)

    pygame.draw.rect(screen, (0,0,0), pygame.Rect(pygame.mouse.get_pos(),(1,1)))

    pygame.display.flip()
    screen.fill((70,110,255))

    time.sleep(1/60)
