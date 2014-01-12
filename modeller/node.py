from OpenGL.GL import *
from numpy import array
from primitive import G_OBJ_SPHERE, G_OBJ_CUBE, MakeCube

class Node(object):
    """ Base class for scene elements """
    identifier = 0

    def __init__(self):
        self.scale = array([1, 1, 1])
        self.location = [0, 0, 0]
        self.color = [0, 0, 0]
        self.call_list = None

        self.identifier = Node.identifier
        Node.identifier += 1

        print self.identifier

    def render(self):
        """ renders the item to the screen """
        glPushMatrix()
        glScale(self.scale[0], self.scale[1], self.scale[2])
        glTranslated(self.location[0], self.location[1], self.location[2])
        glColor3f(self.color[0], self.color[1], self.color[2])
        glCallList(self.call_list)
        glPopMatrix()

    def color_from_id(self):
        denom = 1.0/256
        r = ((self.identifier & 0x000000FF) >>  0) * denom
        g = ((self.identifier & 0x0000FF00) >>  8) * denom
        b = ((self.identifier & 0x00FF0000) >> 16) * denom
        return [r, g, b]

    def render_picking(self):
        # set color to id
        main_color = self.color
        self.color = self.color_from_id()
        self.render()
        self.color = main_color

    def translate(self, x, y, z):
        self.location = [x, y, z]

    def set_color(self, r, g, b):
        self.color = [r, g, b]


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
