#!/usr/bin/env python3

from collections import Counter
from functools   import partial as p
from itertools   import starmap, combinations
from operator    import ne,eq
from sys         import stdin
from numpy       import argmin

t = lambda a: zip(*a)
l = p(lambda mf, f, it: list(mf(f, it)))
map_l    = p(l, map)
filter_l = p(l, filter)

strings = stdin.read().split()
counters = map_l(Counter, strings)

print(sum([2 in c.values() for c in counters]) *
      sum([3 in c.values() for c in counters]))

pairs = list(combinations(strings, 2))
distances = map_l(lambda pair: sum(starmap(ne, t(pair))), pairs)
pair = pairs[argmin(distances)]
shared_characters,_ = t(filter_l(lambda chars: eq(*chars), t(pair)))

print(''.join(shared_characters))
