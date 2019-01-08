from itertools import product, starmap
from functools import partial as p
from operator import gt, sub

import numpy as np

def intersect_aabb_point(aabb, p):
    for x, lower, upper in zip(p, aabb.min_vert, aabb.max_vert):
        if x < lower or x > upper: return False
    return True

class AABB:
    def __init__(s, min_vert, max_vert):
        s.min_vert = min_vert
        s.max_vert = max_vert
        s.shape = (*starmap(sub,zip(max_vert, min_vert)),)

    def __str__(s): return f"{s.min_vert} to {s.max_vert}"

def partition_tree(
        max_depth=32,
        max_bucket=100,
        pos_fn=None,
        membership_fn=None):
    class Tree:
        def __init__(s, it, depth=0, bounds=None):
            s.depth = depth
            s.bounds = bounds
            s.children = []
            possible_elements = list(it)

            if not bounds:
                s.elements = possible_elements
                positions = [Tree.pos_fn(el) for el in s.elements]
                min_coord = np.min(positions,0)
                max_coord = np.max(positions,0)
                s.bounds = AABB(min_coord, max_coord)
            else:
                s.elements = list(filter(p(Tree.membership_fn, s.bounds),
                                         possible_elements))

        def done(s):
            return len(s) < Tree.max_bucket or s.depth >= Tree.max_depth or any(
                    map(lambda x: x < 1,
                        starmap(sub, zip(s.bounds.max_vert, s.bounds.min_vert))))


        def split(s):
            if not s.children:
                middle = [x+w//2 for x,w in zip(s.bounds.min_vert, s.bounds.shape)]
                lb = zip(s.bounds.min_vert, middle) #xyz
                ub = zip([m+1 for m in middle], s.bounds.max_vert) #XYZ
                for xxyyzz in product(*tuple(zip(lb,ub))): # product(xX,yY,zZ)
                    # (x1, x2), (y1, y2), (z1, z2)
                    # -> (x1, y1, z1), (x2, y2, z2)
                    s.children.append(Tree(s.elements, s.depth+1,
                                           AABB(*zip(*xxyyzz))))
            return s.children

        def __repr__(s): return str(s)
        def __str__(s): return f"Tree level {s.depth} size {len(s)} bounds {s.bounds} {'DONE' if s.done() else ''} "
        def __len__(s): return len(s.elements)

    Tree.max_depth = max_depth
    Tree.max_bucket = max_bucket
    Tree.pos_fn = pos_fn if pos_fn else lambda x: x
    if not membership_fn:
        membership_fn = lambda aabb, obj: intersect_aabb_point(aabb, Tree.pos_fn(obj))
    Tree.membership_fn = membership_fn

    return Tree
