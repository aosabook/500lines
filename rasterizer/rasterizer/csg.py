from shape import Shape
from geometry import *

class CSG(Shape):
    def __init__(self, v1, v2, color=None):
        Shape.__init__(self, color or v1.color or v2.color)
        self.v1 = v1
        self.v2 = v2
    def transform(self, t):
        return self.__class__(self.v1.transform(t), self.v2.transform(t),
                              color=self.color)

class Union(CSG):
    def __init__(self, v1, v2, color=None):
        CSG.__init__(self, v1, v2, color=color)
        self.bound = AABox.from_vectors(v1.bound.low, v1.bound.high,
                                        v2.bound.low, v2.bound.high)
    def contains(self, p):
        return self.v1.contains(p) or self.v2.contains(p)
    def signed_distance_bound(self, p):
        b1 = self.v1.signed_distance_bound(p)
        b2 = self.v2.signed_distance_bound(p)
        return b1 if b1 > b2 else b2

class Intersection(CSG):
    def __init__(self, v1, v2, color=None):
        CSG.__init__(self, v1, v2, color=color)
        self.bound = v1.bound.intersection(v2.bound)
    def contains(self, p):
        return self.v1.contains(p) and self.v2.contains(p)
    def signed_distance_bound(self, p):
        b1 = self.v1.signed_distance_bound(p)
        b2 = self.v2.signed_distance_bound(p)
        return b1 if b1 < b2 else b2

class Subtraction(CSG):
    def __init__(self, v1, v2, color=None):
        CSG.__init__(self, v1, v2, color=color)
        self.bound = self.v1.bound
    def contains(self, p):
        return self.v1.contains(p) and not self.v2.contains(p)
    def signed_distance_bound(self, p):
        b1 = self.v1.signed_distance_bound(p)
        b2 = -self.v2.signed_distance_bound(p)
        return b1 if b1 < b2 else b2
