from shape import Shape
from geometry import Vector, Transform, quadratic, scale, translate
from color import Color

class Ellipse(Shape):
    def __init__(self, a=1.0, b=1.0, c=0.0, d=0.0, e=0.0, f=-1.0, color=None):
        Shape.__init__(self, color)
        self.a = a
        self.b = b
        self.c = c
        self.d = d
        self.e = e
        self.f = f
        t = Transform(2 * a, c, 0, c, 2 * b, 0)
        self.center = t.inverse() * Vector(-d, -e)
        l1, l0 = quadratic(1, 2 * (-a - b), 4 * a * b - c * c)
        v = t.eigv()
        axes = [v[0] * ((l0 / 2) ** -0.5), v[1] * ((l1 / 2) ** -0.5)]
        self.bound = Vector.union(self.center - axes[0] - axes[1],
                                  self.center - axes[0] + axes[1],
                                  self.center + axes[0] - axes[1],
                                  self.center + axes[0] + axes[1])
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
        dd = 2*self.a*m00 * m02 + 2*self.b*m10*m12 \
             + self.c*(m00*m12 + m02*m10) + self.d*m00 + self.e*m10
        ee = 2*self.a*m10*m02 + 2*self.b*m11*m12 \
             + self.c*(m01*m12 + m02*m11) + self.d*m01 + self.e*m11
        ff = self.a*m02*m02 + self.b*m12*m12 + self.c*m02*m12 \
             + self.d*m02 + self.e*m12 + self.f
        return Ellipse(aa, bb, cc, dd, ee, ff, color=self.color)
    def signed_distance_bound(self, p):
        def sgn(x):
            return 0 if x == 0 else x / abs(x)
        v = -sgn(self.value(p))
        c = self.center
        pc = p - c
        u2 = self.a*pc.x**2 + self.b*pc.y**2 + self.c*pc.x*pc.y
        u1 = 2*self.a*c.x*pc.x + 2*self.b*c.y*pc.y \
             + self.c*c.y*pc.x + self.c*c.x*pc.y + self.d*pc.x \
             + self.e*pc.y
        u0 = self.a*c.x**2 + self.b*c.y**2 + self.c*c.x*c.y \
             + self.d*c.x + self.e*c.y + self.f
        sols = quadratic(u2, u1, u0)
        crossings = c+pc*sols[0], c+pc*sols[1]
        if (p - crossings[0]).length() < (p - crossings[1]).length():
            surface_pt = crossings[0]
        else:
            surface_pt = crossings[1]
        d = Vector(2*self.a*surface_pt.x + self.c*surface_pt.y + self.d,
                   2*self.b*surface_pt.y + self.c*surface_pt.x + self.e)
        return v * abs(d.dot(p - surface_pt) / d.length())

def Circle(center, radius, color=None):
    return Ellipse(color=color).transform(
        scale(radius, radius)).transform(
        translate(center.x, center.y))
