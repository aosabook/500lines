import destijl
from .. import *

def run(image):
    painting = destijl.painting()
    frame = Scene([Rectangle(Vector(-0.025,-0.025), Vector(1.025, 1.025), Color(0,0,0,1)),
                   Rectangle(Vector(0,0), Vector(1, 1), Color(1,1,1,1))])
    small_painting = Scene([frame, painting], scale(0.45, 0.45))
    p1 = Scene([small_painting], translate(0.025, 0.025))
    p2 = Scene([small_painting], translate(0.025, 0.525))
    p3 = Scene([small_painting], translate(0.525, 0.025))
    p4 = Scene([small_painting], translate(0.525, 0.525))
    s = Scene([p1, p2, p3, p4])
    s.draw(image)
   
