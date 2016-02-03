from shape import Shape
from geometry import HalfPlane, Vector, AABox

class ConvexPoly(Shape): # a *convex* poly, in ccw order, with no repeating vertices
    def __init__(self, ps, color=None):
        Shape.__init__(self, color)
        self.vs = ps
        self.bound = AABox.from_vectors(*self.vs)
        self.half_planes = []
        for i in xrange(len(self.vs)):
            h = HalfPlane(self.vs[i], self.vs[(i+1) % len(self.vs)])
            self.half_planes.append(h)
    def signed_distance_bound(self, p):
        plane = self.half_planes[0]
        min_inside = 1e30
        max_outside = -1e30
        for plane in self.half_planes:
            d = plane.signed_distance(p)
            if d <= 0 and d > max_outside:
                max_outside = d
            if d >= 0 and d < min_inside:
                min_inside = d
        return max_outside if max_outside <> -1e30 else min_inside
    def contains(self, p):
        for plane in self.half_planes:
            if plane.signed_distance(p) < 0:
                return False
        return True
    def transform(self, xform):
        return ConvexPoly(list(xform * v for v in self.vs), color=self.color)

Triangle, Quad = ConvexPoly, ConvexPoly

def Rectangle(v1, v2, color=None):
    return Quad([Vector(min(v1.x, v2.x), min(v1.y, v2.y)),
                 Vector(max(v1.x, v2.x), min(v1.y, v2.y)),
                 Vector(max(v1.x, v2.x), max(v1.y, v2.y)),
                 Vector(min(v1.x, v2.x), max(v1.y, v2.y))],
                color=color)

def LineSegment(v1, v2, thickness, color=None):
    d = v2 - v1
    d.x, d.y = -d.y, d.x
    d *= thickness / d.length() / 2
    return Quad([v1 + d, v1 - d, v2 - d, v2 + d], color=color)

# A simple polygon
# It will work by triangulating a polygon (via simple, slow ear-clipping)
# and then rendering it by a CSG union of the triangles
class Poly(Shape):
    def __init__(self, *args, **kwargs):
        raise Exception("Unimplemented")
