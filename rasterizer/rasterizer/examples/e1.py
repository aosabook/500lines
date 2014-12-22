from .. import *
def run(image):
    scene = Scene()
    scene.add(Triangle([Vector(0.5, 0.5), Vector(0.8, 0.5), Vector(0.5, 0.8)],
                       Color(1,0,0,1)))
    scene.draw(image)
