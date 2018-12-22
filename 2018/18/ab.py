#!/usr/bin/env python3

import sys
import numpy as np
from itertools import islice

lines = [list(l.strip()) for l in sys.stdin.readlines()]

w = len(lines[0])+2
h = len(lines)+2

initial_state = np.full((h,w),'\0')
initial_state[1:-1,1:-1] = lines

def step(last_state):
    next_state = last_state.copy()
    count = lambda c: (window == c).sum()
    for y in range(1,h-1):
        for x in range(1,w-1):
            window = last_state[y-1:y+2,x-1:x+2]
            last = last_state[y,x] 
            if   last == '.' and count('|') >= 3: next_state[y,x] = '|'
            elif last == '|' and count('#') >= 3: next_state[y,x] = '#'
            elif last == '#' and (count('|') == 0 or count('#') == 1):
                next_state[y,x] = '.'
    return next_state

def score(state):
    return (state == '|').sum() * (state == '#').sum()

def scores(state):
    while True:
        yield score(state)
        state = step(state)

print(next(islice(scores(initial_state),10,11)))

score_record = [0]
states_seen = set()
for i, s in enumerate(scores(initial_state)):
    d = s - score_record[-1]

    if (s,d) in states_seen:
        steps_left = 1000000000 - i
        cycle = score_record[-score_record[::-1].index(s)-1:]
        cycle_length = len(cycle)
        print(cycle[steps_left % cycle_length])
        break

    states_seen.add((s,d))
    score_record.append(s)
