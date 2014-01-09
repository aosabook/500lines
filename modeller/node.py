from OpenGL.GL import *
from numpy import array
from primitive import G_OBJ_SPHERE, G_OBJ_CUBE, MakeCube

class Node(object):
    """ Base class for scene elements """
    def __init__(self):
        self.scale = array([1, 1, 1])
        self.call_list = None

    def render(self):
        """ renders the item to the screen """
        glPushMatrix()
        glScale(self.scale[0], self.scale[1], self.scale[2])
        glCallList(self.call_list)
        glPopMatrix()


class Sphere(Node):
    """ Sphere primitive """
    def __init__(self):
        super(Sphere, self).__init__()
        self.call_list = G_OBJ_SPHERE

class Cube(Node):
    """ Cube primitive """
    def __init__(self):
        super(Cube, self).__init__()
        self.call_list = G_OBJ_CUBE
        pass
