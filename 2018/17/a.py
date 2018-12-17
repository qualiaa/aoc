#!/usr/bin/env python3

import re
import sys
import numpy as np

regex_pattern = "([xy])=([0-9.]+), ([xy])=([0-9.]+)"

xys = {"x":[],"y":[]}
for l in sys.stdin.readlines():
    mo = re.match(regex_pattern, l)
    if not mo:
        print(l)
    for k,v in [mo.group(1,2), mo.group(3,4)]:
        if "." in v:
            ran = [int(i) for i in v.split("..")]
            v = range(ran[0],ran[1]+1)
        else:
            v = [int(v)]
        xys[k].append(v)

xys = list(zip(*xys.values()))
print(xys)
print([(x,y) for (xs,ys) in xys for x in xs for y in ys])

