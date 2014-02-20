import random
import math
from itertools import product

def quadratic(a, b, c):
    d = (b * b - 4 * a * c) ** 0.5
    if b >= 0:
        return (-b - d) / (2 * a), (2 * c) / (-b - d)
    else:
        return (2 * c) / (-b + d), (-b + d) / (2 * a)

class Vector:
    def __init__(self, *args):
        self.x, self.y = args
    def __add__(self, o):
        return Vector(self.x + o.x, self.y + o.y)
    def __sub__(self, o):
        return Vector(self.x - o.x, self.y - o.y)
    def __mul__(self, k):
        return Vector(self.x * k, self.y * k)
    def dot(self, o):
        return self.x * o.x + self.y * o.y
    def min(self, o):
        return Vector(min(self.x, o.x), min(self.y, o.y))
    def max(self, o):
        return Vector(max(self.x, o.x), max(self.y, o.y))
    def union(*args):
        return AABox(reduce(Vector.min, args), reduce(Vector.max, args))
    def length(self):
        return (self.x * self.x + self.y * self.y) ** 0.5

class AABox:
    def __init__(self, p1, p2):
        self.low = p1.min(p2)
        self.high = p1.max(p2)
    def midpoint(self):
        return (self.low + self.high) * 0.5
    def size(self):
        return self.high - self.low
    def contains(self, p):
        return self.low.x <= p.x <= self.high.x and \
               self.low.y <= p.y <= self.high.y
    def overlaps(self, r):
        return not (r.low.x >= self.high.x or r.high.x <= self.low.x or
                    r.low.y >= self.high.y or r.high.y <= self.low.y)
    def intersection(self, other):
        return AABox(self.low.max(other.low), self.high.min(other.high))

class HalfPlane:
    def __init__(self, p1, p2):
        self.v = Vector(-p2.y + p1.y, p2.x - p1.x)
        l = self.v.length()
        self.c = -self.v.dot(p1) / l
        self.v = self.v * (1.0 / l)
    def signed_distance(self, p):
        return self.v.dot(p) + self.c

class Transform:
    def __init__(self, m11, m12, tx, m21, m22, ty):
        self.m = [[m11, m12, tx],
                  [m21, m22, ty],
                  [0, 0, 1]]
    def __mul__(self, other): # ugly
        if isinstance(other, Transform):
            t = [[0] * 3 for i in xrange(3)]
            for i, j, k in product(xrange(3), repeat=3):
                t[i][j] += self.m[i][k] * other.m[k][j]
            return Transform(t[0][0], t[0][1], t[0][2],
                             t[1][0], t[1][1], t[1][2])
        else:
            nx = self.m[0][0] * other.x + self.m[0][1] * other.y + self.m[0][2]
            ny = self.m[1][0] * other.x + self.m[1][1] * other.y + self.m[1][2]
            return Vector(nx, ny)
    def det(s):
        return s.m[0][0] * s.m[1][1] - s.m[0][1] * s.m[1][0]
    def inverse(self):
        d = 1.0 / self.det()
        return Transform(d * self.m[1][1], -d * self.m[0][1], -self.m[0][2],
                         -d * self.m[1][0], d * self.m[0][0], -self.m[1][2])
    def eigv(self): # power iteration, ignores translation, assumes SPD
        a = Vector(random.random(), random.random())
        last = Vector(0,0)
        t = Transform(self.m[0][0], self.m[0][1], 0,
                      self.m[1][0], self.m[1][1], 0)
        while (a - last).length() > 1e-6:
            last = a
            a = t * a
            a = a * (1.0 / a.length())
        return a, Vector(-a.y, a.x)

def identity():
    return Transform(1, 0, 0, 0, 1, 0)

def rotate(theta):
    s = math.sin(theta)
    c = math.cos(theta)
    return Transform(c, -s, 0, s, c, 0)

def translate(tx, ty):
    return Transform(1, 0, tx, 0, 1, ty)

def scale(x, y):
    return Transform(x, 0, 0, 0, y, 0)

def around(v, t):
    return translate(v.x, v.y) * t * translate(-v.x, -v.y)
