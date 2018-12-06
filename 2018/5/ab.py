#!/usr/bin/env python3

import sys

def readPoly(s):
    p = []
    for u in s:
        try: p.pop() if p[-1] in [u-32,u+32] else p.append(u)
        except: p.append(u)
    return p

polymer = list(map(ord,sys.stdin.read().split()[0]))
print(len(readPoly(polymer)))
print(min([len(readPoly(filter(lambda c: c not in unit,polymer)))
            for unit in zip(range(65,97),range(97,123))]))
