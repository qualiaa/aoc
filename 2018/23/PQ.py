from collections import UserDict, OrderedDict
from operator import itemgetter

class PQ (UserDict):
    def _s(s, it): return sorted(it, key=itemgetter(1), reverse=s.reverse)
    def __init__(s, it, reverse=False):
        s.reverse = reverse
        s.data = OrderedDict(s._s(it))
    def __setitem__(s, k, v):
        s.data[k] = v
        s.data = OrderedDict(s._s(s.data.items()))
    def update(s, d):
        s.data.update(d)
        s.data = OrderedDict(s._s(s.data.items()))
    def popitem(s): return s.data.popitem()

