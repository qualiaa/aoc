from operator import sub
from utils import clamp, vect

from shapes import AABB, Sphere


# NB: this is manhattan distance
def distance(p1,p2):
    return sum(map(abs, vect(sub, p1, p2)))

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
