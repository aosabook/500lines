#!/usr/bin/python
# -*- coding: utf-8 -*-
"""Draw SVG diagrams of skip lists and skip files to contrast.
"""
import sys

from xml.etree.ElementTree import ElementTree, TreeBuilder

font = "font-family: Palatino, \"URW Palladio R\", serif;"
width = 288
height = 144

def main():
    global builder

    builder = TreeBuilder()

    builder.start("svg", dict(xmlns="http://www.w3.org/2000/svg",
                              width=str(width),
                              height=str(height)))
    
    if len(sys.argv) != 2 or sys.argv[1] not in ['skiplist', 'skipfiles']:
        sys.stderr.write("Usage: %(me)s skiplist or %(me)s skipfiles\n" % 
                         dict(me=sys.argv[0]))
        return
    elif sys.argv[1] == 'skiplist':
        title("Skip list")
        fill_structure(Skiplist())
    else:
        title(u"Skip ﬁles")
        fill_structure(Btree())

    builder.end("svg")
    ElementTree(builder.close()).write(sys.stdout, encoding='utf-8')
    sys.stdout.write("\n")

def fill_structure(sl):
    sl.depth(3)
    sl.key(3, 3)
    sl.key(1, 5)
    sl.key(2, 6)
    sl.key(1, 10)
    sl.key(1, 11)
    sl.key(2, 15)
    sl.key(3, 16)
    sl.key(1, 22)
    sl.key(2, 25)
    sl.key(1, 25)

class Btree:
    "Draw a B+-tree."
    y_top = 50
    def depth(self, depth):
        self._depth = depth
        self.x = [20 + (depth-i) * 39 for i in range(depth)]

    def key(self, depth, key):
        width = height = 20
        padding = 3
        margin = 16
        prev = None
        yoff = 4
        for i in range(self._depth - depth, self._depth):
            y = self.y_top + i * (height + margin)
            box(self.x[i], y,
                self.x[i] + width, y + height,
                {'stroke': 'white', 'fill': '#ccc'})
            cx, cy = self.x[i] + width/2, y + height/2
            if prev is not None:
                px, py = prev
                arrow(px, py, cx, y)

            builder.start('text', dict(x=str(cx),
                                       y=str(y + height - padding - yoff),
                                       style="font-size: 12px; %s text-anchor: middle" % font))
            builder.data(str(key))
            builder.end('text')

            prev = cx, y + height - yoff
            self.x[i] += width

class Skiplist:
    "Draw a skip list."
    def __init__(self):
        self.pending = []
        self.x = 20

    def depth(self, depth):
        pass

    def key(self, node_height, key):
        height = width = 20
        y_base = 130
        padding = 3
        for i in range(node_height + 1):
            box(self.x, y_base - i * height,
                self.x + width, y_base - (i+1) * height,
                {'stroke': 'white', 'fill': '#ccc'})
            cx = self.x + width/2
            cy = y_base - (i+0.5) * height

            if i > 0 and i < len(self.pending):
                ocx, ocy = self.pending[i]
                arrow(ocx, ocy, self.x, ocy)
            elif i == len(self.pending):
                self.pending.append(None)
            self.pending[i] = cx, cy

        builder.start('text', dict(x=str(cx),
                                   y=str(y_base - padding),
                                   style="font-size: 12px; %s text-anchor: middle" % font))
        builder.data(str(key))
        builder.end('text')

        margin = 5
        self.x += width + margin

def title(s):
    builder.start("text", dict(style="font-size: 40px; text-anchor: middle; %s" % font, x=str(width/2), y="40"))
    builder.data(s)
    builder.end("text")

def arrow(x0, y0, x1, y1):
    direction = normalize(displacement((x0, y0), (x1, y1)))
    dx2, dy2 = rotate(direction, (-3.5, -2))
    dx3, dy3 = rotate(direction, (-3.5, 2))
    path(['M', x0, y0, 'L', x1, y1, 'm', dx2, dy2, 'l', -dx2, -dy2, 'l', dx3, dy3])

def displacement((x0, y0), (x1, y1)):
    return x1 - x0, y1 - y0

def normalize((x, y)):
    mag = (x**2 + y**2)**0.5
    return x/mag, y/mag

assert normalize((0, 1)) == (0, 1)
assert normalize((1, 0)) == (1, 0)
assert normalize((-1, 0)) == (-1, 0)

def rotate((cos, sin), (x, y)):
    return (cos * x - sin * y), (sin * x + cos * y)

assert rotate((1, 0), (.2, .3)) == (.2, .3)

def box(x0, y0, x1, y1, attrs={}):
    path(['M', x0, y0, 'L', x1, y0, 'L', x1, y1, 'L', x0, y1, 'Z'], attrs)

def path(d, attrs=dict(fill="none", stroke="#000")):
    attrs = attrs.copy()
    attrs['d'] = ' '.join(map(str, d))
    builder.start("path", attrs)
    builder.end("path")


if __name__ == '__main__': main()
