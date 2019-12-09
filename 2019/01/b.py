#!/usr/bin/env python3
import sys
from itertools import *


def gen(i,f):
    while True:
        i = f(i)
        yield i

print(sum(sum(takewhile(lambda x: x>0, gen(int(x), lambda x: x//3 - 2))) for x in sys.stdin))
