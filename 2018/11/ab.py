#!/usr/bin/env python3

import sys
import numpy as np
from scipy.signal import convolve

s = int(sys.stdin.read())

ix = np.indices((300,300)) + 1
r = ix[1] + 10
grid = ((r*ix[0] + s)*r // 10 // 10 % 10) - 5

def get_conv(k):
    sums = convolve(grid, np.ones((k,k)), "valid")
    i = np.argmax(sums)
    yx = np.array(np.unravel_index(i, sums.shape))
    return (sums.ravel()[i], tuple(yx[::-1]+1) + (k,))

print(get_conv(3)[1][:2])

maxsums = [get_conv(i+1) for i in range(300)]
maxVal, maxCoord = maxsums[max(range(300), key=lambda i: maxsums[i][0])]
print(maxCoord)
