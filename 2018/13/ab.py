#!/usr/bin/env python3

import sys

class Cart:
    def __init__(self, y, x, c, m):
        self.y    = y
        self.x    = x
        self.next = 0
        self.map  = m
        self.dir  = int(c.translate(bytes.maketrans(b"^>v<",b"0123")))
        #print(c,self.dir)

    def move(self):
        if   self.dir == 0: self.y -= 1
        elif self.dir == 2: self.y += 1
        elif self.dir == 1: self.x += 1
        elif self.dir == 3: self.x -= 1

        current_tile = self.map[self.y][self.x] 

        if current_tile == "\\":
            self.dir = (3 - self.dir) % 4
        elif current_tile == "/":
            self.dir = (1 - self.dir) % 4
        elif current_tile == "+":
            self.turn()

    def turn(self):
        self.dir  = (self.dir + self.next - 1) % 4
        self.next = (self.next + 1) % 3

    def __lt__(self,c):
        if self.y == c.y:
            return self.x < c.x
        return self.y < c.y

class Sim:
    def __init__(self, lines):
        self.map=[]
        self.carts = []
        self.collisions=[]
        self.to_remove=set()
        for y,l in enumerate(lines):
            self.map.append([])
            for x,c in enumerate(l):
                if c in "<>^v":
                    self.carts.append(Cart(y,x,c,self.map))
                    if c in "<>":
                        c = '-'
                    elif c in "^v":
                        c = '|'
                self.map[y].append(c)

    def step(self):
        self.carts.sort()
        for c in self.carts:
            c.move()
            self.check_collisions(c)
        self.remove_collisions()

    def check_collisions(self,c1):
        for c2 in self.carts:
            if c1 is not c2 and c1.x == c2.x and c1.y == c2.y:
                self.collisions.append((c1.x,c1.y))
                self.to_remove.add(c1)
                self.to_remove.add(c2)

    def remove_collisions(self,):
        while self.to_remove:
            self.carts.remove(self.to_remove.pop())

s = Sim(sys.stdin.readlines())

while not s.collisions:
    s.step()

collision = s.collisions[0]

while len(s.carts) > 1:
    s.step()

print(collision)
print((s.carts[0].x,s.carts[0].y))
