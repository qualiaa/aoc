#!/usr/bin/env python3

from collections import deque
import sys


def run_game(numPlayers, numMarbles):
    state = deque([0])
    score = [0] * numPlayers

    for i in range(1,numMarbles+1):
        if i % 23: 
            state.rotate(-1)
            state.append(i)
        else:
            state.rotate(7)
            score[(i-1) % numPlayers] += i + state.pop()
            state.rotate(-1)
    return max(score)


digit_pairs = [
        ''.join(c for c in l if c.isdigit() or c.isspace()).split()
                for l in sys.stdin.readlines()]
 
games = [tuple(map(int, digit_pair)) for digit_pair in digit_pairs]
for p, m in games:
    print(run_game(p, m))
    print(run_game(p, 100*m))
