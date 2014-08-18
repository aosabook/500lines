from color import Color
from itertools import product
from geometry import Vector
import random

class SceneObject:
    def draw(self, image):
        raise NotImplementedError("Undefined method")

class Shape(SceneObject):
    def __init__(self, color=None):
        self.color = color if color is not None else Color()
        self.bound = None
    def contains(self, p):
        raise NotImplementedError("Undefined method")
    def signed_distance_bound(self, p):
        raise NotImplementedError("Undefined method")
    def draw(self, image, super_sampling = 6):
        if not self.bound.overlaps(image.bounds()):
            return
        color = self.color
        r = float(image.resolution)
        jitter = [Vector((x + random.random()) / super_sampling / r,
                         (y + random.random()) / super_sampling / r)
                  for (x, y) in product(xrange(super_sampling), repeat=2)]
        lj = len(jitter)
        l_x = max(int(self.bound.low.x * r), 0)
        l_y = max(int(self.bound.low.y * r), 0)
        h_x = min(int(self.bound.high.x * r), r-1)
        h_y = min(int(self.bound.high.y * r), r-1)
        for y in xrange(l_y, int(h_y+1)):
            x = l_x
            while x <= h_x:
                corner = Vector(x / r, y / r)
                b = self.signed_distance_bound(corner)
                pixel_diameter = (2 ** 0.5) / r
                if b > pixel_diameter:
                    steps = int(r * (b - (pixel_diameter - 1.0/r)))
                    for x_ in xrange(x, min(x + steps, int(h_x+1))):
                        image.pixels[y][x_].draw(color)
                    x += steps
                elif b < -pixel_diameter:
                    steps = int(r * (-b - (pixel_diameter - 1.0/r)))
                    x += steps
                else:
                    coverage = 0
                    for j in jitter:
                        if self.contains(corner + j):
                            coverage += 1.0
                    image.pixels[y][x].draw(color.fainter(coverage / lj))
                    x += 1
