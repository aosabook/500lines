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
        Poly([Vector(0.2,0.1), Vector(0.9,0.3), Vector(0.1,0.4)],
             Color(1,0,0,0.5)),
        Poly([Vector(0.5,0.2), Vector(1,0.4), Vector(0.2,0.7)],
             Color(0,1,0,0.5)),
        Rectangle(Vector(0.1,0.7), Vector(0.6,0.8),
                  Color(0,0.5,0.8,0.5)),
        Poly([Vector(-1, 0.6), Vector(0.2, 0.8), Vector(-2, 0.7)],
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

def test_ellipse():
    Ellipse().transform(
        scale(1.5, 1)).transform(
        translate(0, 2)).transform(
        rotate(math.radians(45))).transform(
        scale(-1, 2))

def test_eigv():
    v = Transform(2, 0, 0, 0, 1, 0).eigv()
    assert(abs(v[0].dot(Vector(1,0))) > 0.9999)
    assert(abs(v[1].dot(Vector(0,1))) > 0.9999)

if __name__ == '__main__':
    test_ellipse()
    test_eigv()
    do_it()
