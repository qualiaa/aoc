#!/usr/bin/env python3

import sys

seen = []

for v in map(int,sys.stdin):
    if v in seen:
        print(seen.pop())
        break
    seen.append(v)
