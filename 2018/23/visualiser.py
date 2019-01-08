import pygame
import sys
import time
from itertools import product, repeat
from functools import partial as pf
from operator import mul, truediv, attrgetter, itemgetter

from intersection import intersect
from intersection import distance, intersect_circle_point, intersect_aabb_circle
from PartitionTree import partition_tree
from PQ import PQ
from shapes import AABB, Sphere
from utils import vect

image_shape = (1000,1000)
black = origin = (0,0,0)

def count_intersections(point):
    return sum(map(lambda c: intersect_circle_point(c, point), drones))

def min_distance(aabb):
    return min(map(pf(distance, origin), product(aabb.min_vert, aabb.max_vert)))

def priority(n):
    return len(n), -min_distance(n.bounds)

def find_highest_priority(aabb):
    ranges = [range(start, end+1)
              for start, end in zip(aabb.min_vert, aabb.max_vert)]
    points = product(*ranges)

    point_priorities = map(lambda point: (
            point, (count_intersections(point), -distance(origin, point))
        ), points)

    return max(point_priorities, key=itemgetter(1))

drones = [*map(Sphere.from_string, sys.stdin)]

Tree = partition_tree(pos_fn=attrgetter("centre"),
                      membership_fn=intersect_aabb_circle)

tree = Tree(drones)

z_min = tree.bounds.min_vert[2]
z_range = tree.bounds.shape[2]
scale = min(vect(truediv, image_shape, tree.bounds.shape))
unit = int(1e7*scale)

def T(point):
    return (*(int(x*scale + w//2) for x, w in zip(point, image_shape)),)

pygame.init()

#image       = pygame.Surface(image_shape, pygame.SRCALPHA, 32)
drone_image = pygame.Surface(image_shape,0, 24)
#image.fill((0xFF,0xFF,0x00))
drone_image.fill((0xFF,0xFF,0xFF))

def get_colour(z):
    h = int(300*(z - z_min) / z_range)
    c = pygame.Color(*black,255)
    c.hsva = (h, 100, 100, 100)
    return c

for circ in drones:
    r = int(circ.r * scale)
    centre = T(circ.centre)

    Sphere(centre, r).draw(drone_image, colour=get_colour(circ.centre[2]))


node_queue = PQ([(tree, priority(tree))])
best_priority = -sys.maxsize, -sys.maxsize
best_point = None

def draw_aabb(surface, aabb, col=None):
    if not col:
        col = get_colour(aabb.min_vert[2] + aabb.shape[2]//2)
    min_vert = T(aabb.min_vert)
    max_vert = T(aabb.max_vert)

    AABB(min_vert, max_vert).draw(surface, col)

def draw_tree(surface):
    bounds = []
    nodes = [tree]

    while nodes:
        node = nodes.pop()
        if node.children:
            nodes.extend(node.children)
        else:
            bounds.append(node.bounds)

    for aabb in bounds:
        draw_aabb(surface, aabb)

font = pygame.font.Font(None, 8*unit)
font_pos = (unit, unit)
i = 0
while node_queue:
    image = drone_image.copy()
    node, priority_metric = node_queue.popitem()

    if priority_metric < best_priority:
        break

    children = node.split()
    for c in children:
        if not c.done():
            node_queue[c] = priority(c)

        elif priority(c) >= best_priority and len(c) > Tree.max_bucket:
            point, p = find_highest_priority(c.bounds)
            if p > best_priority:
                best_priority = p
                best_point = point



    draw_tree(image)
    image.blit(font.render(f"{i}", False, black), font_pos)
    for c in children:
        draw_aabb(image, c.bounds, (0xFF,0,0))
    pygame.image.save(image, f"img/img{i:03d}.png")
    i += 1

drone_image.blit(font.render(f"{i}: Finished", False, black), font_pos)
pygame.draw.circle(drone_image, (0xFF,0,0), T(best_point), 2)
for i in range(i,i+10):
    pygame.image.save(drone_image, f"img/img{i:03d}.png")
