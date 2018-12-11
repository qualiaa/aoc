#!/usr/bin/env python3

import sys
import numpy as np
from scipy.signal import convolve2d

s = int(sys.stdin.read())

ix = np.indices((300,300)) + 1
r = ix[1] + 10
grid = ((r*ix[0] + s)*r // 10 // 10 % 10) - 5

def get_conv(k):
    sums = convolve2d(grid, np.ones((k,k)), "valid")
    i = np.argmax(sums)
    yx = np.array(np.unravel_index(i, sums.shape))
    return (sums.ravel()[i], tuple(yx[::-1]+1) + (k,))

print(get_conv(3)[1][:2])

maxV = -sys.maxsize
maxCoords = None
for i in range(1,301):
    v, coords = get_conv(i)
    if v > maxV:
        maxV = v
        maxCoords = coords

print(maxCoords)
