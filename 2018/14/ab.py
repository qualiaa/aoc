#!/usr/bin/env python3

import sys
from itertools import islice
from collections import deque

numRecipes = int(sys.stdin.read())

def digits(n):
    ds = []
    while n > 0:
        n, r = divmod(n, 10)
        ds.append(r)
    if not ds:
        return [0]
    return ds[::-1]

def nextScores(l, i, j):
    x, y = l[i], l[j]
    scores = digits(x+y)
    nl = len(l) + len(scores)
    return scores, ((i + x + 1) % nl, (j + y + 1) % nl)

def recipes(init_recipes=[3,7], indices=(0,1)):
    l = list(init_recipes)
    for s in l:
        yield s
    while True:
        scores, indices = nextScores(l, *indices)
        l.extend(scores)
        for s in scores:
            yield s

print(*list(islice(recipes(), numRecipes, numRecipes+10)), sep='')

ss = deque(digits(numRecipes))
d  = deque(maxlen=len(ss))

for i, v in enumerate(recipes()):
    d.append(v)
    if d == ss: break

print(i - len(ss) + 1)
