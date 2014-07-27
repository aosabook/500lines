from .. import *

def painting():
    scene = Scene()
    scene.add(Rectangle(Vector(0.95, 0.0), Vector(1.0, 0.175), Color(0.75, 0.55, 0, 1)))
    scene.add(Rectangle(Vector(0.0, 0.73), Vector(1.0, 0.76), Color(0,0,0,1)))
    scene.add(Rectangle(Vector(0.94, 0), Vector(0.96, 1), Color(0,0,0,1)))
    scene.add(Rectangle(Vector(0.95, 0.16), Vector(1, 0.19), Color(0,0,0,1)))
    scene.add(Rectangle(Vector(0.25, 0.35), Vector(1.0, 1.0), Color(0.75,0,0,1)))
    scene.add(Rectangle(Vector(0.0, 0.0), Vector(0.25, 0.35), Color(0.05,0,0.85,1)))
    scene.add(Rectangle(Vector(0.24, 0.0), Vector(0.26, 1.0), Color(0,0,0,1)))
    scene.add(Rectangle(Vector(0.0, 0.34), Vector(1.0, 0.36), Color(0,0,0,1)))
    return scene

def run(image):
    painting().draw(image)
