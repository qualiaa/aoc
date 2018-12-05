#!/usr/bin/env python3

import sys

polymer = list(map(ord,sys.stdin.read().split()[0]))

combine = lambda a,b: abs(a-b) == 32

def singleStep(p):
    ix = iter(range(0, len(p)-1))
    result = []
    try: 
        for i in ix:
            if combine(p[i], p[i+1]):
                next(ix)
            else:
                result.append(p[i])
        result.append(p[-1])
    except: pass
    return result

def completeReaction(p):
    q = singleStep(p)
    while(q != p):
        p = q
        q = singleStep(p)
    return p

print(len(completeReaction(polymer)))

print(min([len(completeReaction(list(filter(lambda x: x not in unit, polymer))))
        for unit in zip(range(65,97),range(97,123))]))



