#!/usr/bin/env python3
import sys

class Node:
    def __init__(self, it):
        n,m = next(it),next(it)
        self.nodes = [Node(it) for _ in range(n)]
        self.md = [next(it) for _ in range(m)]

    def sum(self):
        return sum(self.md) + sum(map(Node.sum,self.nodes))

    def eval(self):
        if self.nodes:
            s = 0
            for i in self.md:
                try: s += self.nodes[i-1].eval()
                except: pass
            return s
        return sum(self.md)

n = Node(map(int,sys.stdin.read().split()))
print(n.sum())
print(n.eval())
