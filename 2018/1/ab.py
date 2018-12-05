#!/usr/bin/env python3

import sys
import itertools
import collections

dfs = list(map(int,sys.stdin.readlines()))
print(sum(dfs))

fs = collections.Counter([0])
for f in itertools.accumulate(itertools.cycle(dfs)):
    fs[f] += 1
    if fs[f] > 1: # assignment expressions soooooon
        print(f)
        break

