from color import Color
from geometry import AABox, Vector

class PPMImage:
    def __init__(self, resolution, bg=Color()):
        self.resolution = resolution
        self.pixels = []
        for i in xrange(self.resolution):
            lst = []
            for j in xrange(self.resolution):
                lst.append(Color(rgb=bg.rgb, a=bg.a))
            self.pixels.append(lst)
    def bounds(self):
        return AABox(Vector(0,0), Vector(1,1))
    def __getitem__(self, a):
        return self.pixels[a.y][a.x]
    def __setitem__(self, a, color):
        self.pixels[a.y][a.x] = color
    def write_ppm(self, out):
        n = self.resolution
        out.write("P6\n%s\n%s\n255\n" % (n,n))
        for y in xrange(n-1, -1, -1):
            for x in xrange(n):
                out.write(self.pixels[y][x].as_ppm())
