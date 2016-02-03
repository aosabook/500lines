from .. import *
import math

def run(image):
    s = Scene()
    radius = 0.02
    angle = 1
    distance = 0.02
    colors = [Color.hex("#1f77b4"), Color.hex("#ff7f0e"), Color.hex("#2ca02c"),
              Color.hex("#d62728"), Color.hex("#9467bd"), Color.hex("#8c564b")]
    for i in xrange(500):
        obj = Circle(Vector(0.5, 0.5), radius, colors[i % len(colors)])
        obj = obj.transform(translate(distance, 0))
        obj = obj.transform(around(Vector(0.5, 0.5), rotate(angle)))
        s.add(obj)
        step = 3 * radius / distance
        distance = distance + 0.009 * step
        angle = angle + step
    s.draw(image)
