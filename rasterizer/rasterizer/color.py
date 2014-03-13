class Color:
    def __init__(self, r=0, g=0, b=0, a=1, rgb=None):
        self.rgb = rgb or (r, g, b)
        self.a = a
    def draw(self, o):
        if self.a == o.a == 0.0:
            return
        if o.a == 1.0:
            self.rgb = o.rgb
            self.a = 1
        else:
            u = 1.0 - o.a
            self.rgb = (u * self.rgb[0] + o.a * o.rgb[0],
                        u * self.rgb[1] + o.a * o.rgb[1],
                        u * self.rgb[2] + o.a * o.rgb[2])
            self.a = 1.0 - (1.0 - self.a) * (1.0 - o.a)
    def fainter(self, k):
        return Color(rgb=self.rgb, a=self.a*k)
    def as_ppm(self):
        def byte(v):
            return int(v ** (1.0 / 2.2) * 255)
        return "%c%c%c" % (byte(self.rgb[0] * self.a),
                           byte(self.rgb[1] * self.a),
                           byte(self.rgb[2] * self.a))
    def __repr__(self):
        return "[" + str(self.rgb) + "," + str(self.a) + "]"
    @staticmethod
    def hex(code, a=1):
        if len(code) == 4:
            return Color(int(code[1], 16) / 15.0,
                         int(code[2], 16) / 15.0,
                         int(code[3], 16) / 15.0, a)
        elif len(code) == 7:
            return Color(int(code[1:3], 16) / 255.0,
                         int(code[3:5], 16) / 255.0,
                         int(code[5:7], 16) / 255.0, a)
