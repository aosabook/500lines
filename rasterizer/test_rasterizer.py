from rasterizer import *
import sys
import cProfile
import random
import math

def do_it():
    f = open(sys.argv[1], 'w')
    i = PPMImage(512, Color(1,1,1,1))
    s = Scene()
    s3 = Scene()
    s3.add(Union(Circle(Vector(0.3, 0.1), 0.1),
                 Circle(Vector(0.35, 0.1), 0.1),
                 Color(0,0,0,0.5)))
    s3.add(Intersection(Circle(Vector(0.3, 0.3), 0.1),
                        Circle(Vector(0.35, 0.3), 0.1),
                        Color(0,0.5,0,1)))
    s3.add(Subtraction(Circle(Vector(0.3, 0.5), 0.1),
                       Circle(Vector(0.35, 0.5), 0.1),
                       Color(0,0,0.5,1)))
    s3.add(Subtraction(Circle(Vector(0.35, 0.7), 0.1),
                       Circle(Vector(0.3, 0.7), 0.1),
                       Color(0,0.5,0.5,1)))
    s2 = Scene([
        LineSegment(Vector(0.0,0), Vector(0.0,1), 0.01, Color(1,0,0,1)),
        LineSegment(Vector(0.1,0), Vector(0.1,1), 0.01, Color(1,0,0,1)),
        LineSegment(Vector(0.2,0), Vector(0.2,1), 0.01, Color(1,0,0,1)),
        LineSegment(Vector(0.3,0), Vector(0.3,1), 0.01, Color(1,0,0,1)),
        LineSegment(Vector(0.4,0), Vector(0.4,1), 0.01, Color(1,0,0,1)),
        LineSegment(Vector(0.5,0), Vector(0.5,1), 0.01, Color(1,0,0,1)),
        LineSegment(Vector(0.6,0), Vector(0.6,1), 0.01, Color(1,0,0,1)),
        LineSegment(Vector(0.2,0.1), Vector(0.7, 0.4), 0.01, Color(0,0.5,0,1))])
    for shape in [
        Circle(Vector(0.0, 0.0), 0.8, Color(1,0.5,0,1)).transform(scale(0.05,1)),
        Triangle([Vector(0.2,0.1), Vector(0.9,0.3), Vector(0.1,0.4)],
                 Color(1,0,0,0.5)),
        Triangle([Vector(0.5,0.2), Vector(1,0.4), Vector(0.2,0.7)],
                 Color(0,1,0,0.5)),
        Rectangle(Vector(0.1,0.7), Vector(0.6,0.8),
                  Color(0,0.5,0.8,0.5)),
        Triangle([Vector(-1, 0.6), Vector(0.2, 0.8), Vector(-2, 0.7)],
                 Color(1,0,1,0.9)),
        Circle(Vector(0.5,0.9), 0.2, Color(0,0,0,1)),
        Circle(Vector(0.9, 0.5), 0.2,
               Color(0,1,1,0.5)).transform(around(Vector(0.9, 0.5), scale(1.0, 0.5))),
        s2,
        Scene([s2], around(Vector(0.5, 0.5), rotate(math.radians(90)))),
        Scene([s3], translate(0.5, 0))
        ]:
        s.add(shape)
    s.draw(i)
    i.write_ppm(f)
    f.close()

def test_quadratic():
    for i in xrange(1000):
        a = random.random()
        b = random.random()
        c = random.random()
        if b * b - 4 * a * c >= 0:
            v1, v2 = quadratic(a, b, c)
            if v1 * v1 * a + v1 * b + c > 1e-5:
                raise Exception("fail")
            if v2 * v2 * a + v2 * b + c > 1e-5:
                raise Exception("fail")
    for i in xrange(1000):
        a = 0
        b = random.random()
        c = random.random()
        if b * b - 4 * a * c >= 0:
            v1, v2 = quadratic(a, b, c)
            if v1 * v1 * a + v1 * b + c > 1e-5:
                raise Exception("fail")
            if v2 * v2 * a + v2 * b + c > 1e-5:
                raise Exception("fail")

def test_inverse():
    for i in xrange(10000):
        f = Transform(random.random(), random.random(), random.random(),
                      random.random(), random.random(), random.random())
        v = Vector(random.random(), random.random())
        m = f * f.inverse()
        if (m * v - v).length() > 1e-4:
            print >>sys.stderr, "inverse failed!"
            print f.m
            print m.m
            raise Exception("foo")

if __name__ == '__main__':
    test_inverse()
    test_quadratic()
    do_it()
