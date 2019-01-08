#!/usr/bin/env python3

import time
import pygame
import sys
from functools import partial as pf

from PartitionTree import partition_tree
from shapes import AABB, Sphere
from intersection import intersect

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
            selected = next(filter(pf(intersect,point), shapes))
        except:
            selected = None
            return False
    return True

PTree = partition_tree()
tree = PTree([], bounds=AABB((0,0), screen.get_size()))

def draw_tree(surface):
    bounds = []
    nodes = [tree]

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

def split_tree(point):
    leaves = []
    nodes = [tree]
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
            select_object(mouse_pos) or split_tree(mouse_pos)



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

    draw_tree(screen)

    pygame.draw.rect(screen, (0,0,0), pygame.Rect(pygame.mouse.get_pos(),(1,1)))

    pygame.display.flip()
    screen.fill((70,110,255))

    time.sleep(1/60)
