from itertools import starmap

def vect(f,*args):
    return starmap(f, zip(*args))

def clamp(v,vmin,vmax):
    if v < vmin: return vmin
    return v if v < vmax else vmax
