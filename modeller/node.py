import random
from OpenGL.GL import glCallList, glColor3f, glMaterialfv, glMultMatrixf, glPopMatrix, glPushMatrix, \
                      GL_EMISSION, GL_FRONT
import numpy

from primitive import G_OBJ_CUBE, G_OBJ_SPHERE
from aabb import AABB
from transformation import scaling, translation
import color


class Node(object):
    """ Base class for scene elements """
    def __init__(self):
        self.color_index = random.randint(color.MIN_COLOR, color.MAX_COLOR)
        self.aabb = AABB([0.0, 0.0, 0.0], [0.5, 0.5, 0.5])
        self.translation = numpy.identity(4)
        self.scalemat = numpy.identity(4)
        self.selected = False

    def render(self):
        """ renders the item to the screen """
        raise NotImplementedError("The Abstract Node Class doesn't define 'render'")

    def translate(self, x, y, z):
        self.translation = numpy.dot(self.translation, translation([x, y, z]))

    def rotate_color(self, forwards):
        self.color_index += 1 if forwards else -1
        if self.color_index > color.MAX_COLOR:
            self.color_index = color.MIN_COLOR
        if self.color_index < color.MIN_COLOR:
            self.color_index = color.MAX_COLOR

    def scale(self, up):
        s =  1.1 if up else 0.9
        self.scalemat = numpy.dot(self.scalemat, scaling([s, s, s]))
        self.aabb.scale(s)

    def pick(self, start, direction, mat):
        """ Return whether or not the ray hits the object
           Consume:  start, direction    the ray to check
                     mat                 the modelview matrix to transform the ray by """

        # transform the modelview matrix by the current translation
        newmat = numpy.dot(mat, self.translation)
        results = self.aabb.ray_hit(start, direction, newmat)
        return results

    def select(self, select=None):
        """ Toggles or sets selected state """
        if select is not None:
            self.selected = select
        else:
            self.selected = not self.selected

class Primitive(Node):
    def __init__(self):
        super(Primitive, self).__init__()
        self.call_list = None

    def render(self):
        glPushMatrix()
        glMultMatrixf(numpy.transpose(self.translation))
        glMultMatrixf(self.scalemat)
        cur_color = color.COLORS[self.color_index]
        glColor3f(cur_color[0], cur_color[1], cur_color[2])
        if self.selected:  # emit light if the node is selected
            glMaterialfv(GL_FRONT, GL_EMISSION, [0.3, 0.3, 0.3])
        glCallList(self.call_list)
        if self.selected:
            glMaterialfv(GL_FRONT, GL_EMISSION, [0.0, 0.0, 0.0])

        glPopMatrix()



class Sphere(Primitive):
    """ Sphere primitive """
    def __init__(self):
        super(Sphere, self).__init__()
        self.call_list = G_OBJ_SPHERE


class Cube(Primitive):
    """ Cube primitive """
    def __init__(self):
        super(Cube, self).__init__()
        self.call_list = G_OBJ_CUBE
