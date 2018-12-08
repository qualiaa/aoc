#!/usr/bin/env python3

import sys
import numpy as np

coords = np.array([[int(x) for x in s.split(", ")] for s in sys.stdin.readlines()])[:,-1::-1]

coords -= coords.min(0)
hw = tuple(coords.max(0) + 1)

coord_space = np.indices(hw).T

distance_tensor = np.stack([np.abs(coord_space - yx).T.sum(0) for yx in coords])

voronoi = np.argmin(distance_tensor,0)
flipped_argmin = (len(coords) - 1) - np.argmin(distance_tensor[-1::-1],0)

voronoi[voronoi != flipped_argmin] = -1

edges = [voronoi[0,:], voronoi[-1,:], voronoi[:,0], voronoi[:,-1]]
edge_elements = np.unique(np.concatenate(edges))

counts = [c for e, c in zip(*np.unique(voronoi, return_counts=True))
            if e not in edge_elements and e != -1]

print(max(counts))
print((distance_tensor.sum(0) < 10000).sum())
