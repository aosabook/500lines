from shape import Shape
from poly import ConvexPoly
from geometry import Vector, Transform, quadratic, scale, translate, AABox, HalfPlane
from color import Color
import sys

class Ellipse(Shape):
    def __init__(self, a=1.0, b=1.0, c=0.0, d=0.0, e=0.0, f=-1.0, color=None):
        Shape.__init__(self, color)
        if c*c - 4*a*b >= 0:
            raise Exception("Not an ellipse")
        self.a = a
        self.b = b
        self.c = c
        self.d = d
        self.e = e
        self.f = f
        self.gradient = Transform(2*a, c, d, c, 2*b, e)
        self.center = self.gradient.inverse() * Vector(0, 0)
        y1, y2 = quadratic(b-c*c/4*a, e-c*d/2*a, f-d*d/4*a)
        x1, x2 = quadratic(a-c*c/4*b, d-c*e/2*b, f-e*e/4*b)
        self.bound = AABox.from_vectors(Vector(-(d + c*y1)/2*a, y1),
                                        Vector(-(d + c*y2)/2*a, y2),
                                        Vector(x1, -(e + c*x1)/2*b),
                                        Vector(x2, -(e + c*x2)/2*b))
        if not self.contains(self.center):
            raise Exception("Internal error, center not inside ellipse")
    def value(self, p):
        return self.a*p.x*p.x + self.b*p.y*p.y + self.c*p.x*p.y \
               + self.d*p.x + self.e*p.y + self.f
    def contains(self, p):
        return self.value(p) < 0
    def transform(self, transform):
        i = transform.inverse()
        ((m00, m01, m02), (m10, m11, m12),_) = i.m
        aa = self.a*m00*m00 + self.b*m10*m10 + self.c*m00*m10
        bb = self.a*m01*m01 + self.b*m11*m11 + self.c*m01*m11
        cc = 2*self.a*m00*m01 + 2*self.b*m10*m11 \
             + self.c*(m00*m11 + m01*m10)
        dd = 2*self.a*m00*m02 + 2*self.b*m10*m12 \
             + self.c*(m00*m12 + m02*m10) + self.d*m00 + self.e*m10
        ee = 2*self.a*m01*m02 + 2*self.b*m11*m12 \
             + self.c*(m01*m12 + m02*m11) + self.d*m01 + self.e*m11
        ff = self.a*m02*m02 + self.b*m12*m12 + self.c*m02*m12 \
             + self.d*m02 + self.e*m12 + self.f
        return Ellipse(aa, bb, cc, dd, ee, ff, color=self.color)
    def intersections(self, c, p):
        # returns the two intersections of the line through c and p
        # and the ellipse. Defining a line as a function of a single
        # parameter u, x(u) = c.x + u * (p.x - c.x), (and same for y)
        # this simply solves the quadratic equation f(x(u), y(u)) = 0
        pc = p - c
        u2 = self.a*pc.x**2 + self.b*pc.y**2 + self.c*pc.x*pc.y
        u1 = 2*self.a*c.x*pc.x + 2*self.b*c.y*pc.y \
             + self.c*c.y*pc.x +   self.c*c.x*pc.y + self.d*pc.x \
             + self.e*pc.y
        u0 = self.a*c.x**2 + self.b*c.y**2 + self.c*c.x*c.y \
             + self.d*c.x + self.e*c.y + self.f
        try:
            sols = quadratic(u2, u1, u0)
        except ValueError:
            raise Exception("Internal error, solutions be real numbers")
        return c+pc*sols[0], c+pc*sols[1]
    def signed_distance_bound(self, p):
        v = self.value(p)
        if v == 0:
            return 0
        elif v < 0:
            # if inside the ellipse, create an inscribed quadrilateral
            # that contains the given point and use the minimum distance
            # from the point to the quadrilateral as a bound. Since
            # the quadrilateral lies entirely inside the ellipse, the
            # distance from the point to the ellipse must be smaller.
            v0, v2 = self.intersections(p, p + Vector(1, 0))
            v1, v3 = self.intersections(p, p + Vector(0, 1))
            return abs(ConvexPoly([v0,v1,v2,v3]).signed_distance_bound(p))
        else:
            c = self.center
            crossings = self.intersections(c, p)
            # the surface point we want is the one closest to p
            if (p - crossings[0]).length() < (p - crossings[1]).length():
                surface_pt = crossings[0]
            else:
                surface_pt = crossings[1]
            # n is the normal at surface_pt
            n = self.gradient * surface_pt
            n = n * (1.0 / n.length())
            # returns the length of the projection of p - surface_pt
            # along the normal
            return -abs(n.dot(p - surface_pt))

def Circle(center, radius, color=None):
    return Ellipse(color=color).transform(
        scale(radius, radius)).transform(
        translate(center.x, center.y))
