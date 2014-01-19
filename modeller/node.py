from OpenGL.GL import *
from numpy import array, matrix
import numpy
from primitive import G_OBJ_SPHERE, G_OBJ_CUBE, MakeCube
from aabb import AABB
from transformation import scaling, translation

class Node(object):
    """ Base class for scene elements """
    def __init__(self):
        self.scale = array([1, 1, 1])
        self.location = [0, 0, 0]
        self.color = [0, 0, 0]
        self.call_list = None
        self.aabb = None
        self.modelmatrix = numpy.identity(4)
        self.translation = numpy.identity(4)
        self.scalemat = numpy.identity(4)


    def render(self):
        """ renders the item to the screen """
        glPushMatrix()
        glScale(self.scale[0], self.scale[1], self.scale[2])
        glTranslated(self.location[0], self.location[1], self.location[2])
        glColor3f(self.color[0], self.color[1], self.color[2])
        glCallList(self.call_list)

        self.aabb.render()

        mat = glGetFloatv(GL_MODELVIEW_MATRIX)
        mat = matrix(mat)
        glPopMatrix()

    def translate(self, x, y, z):
        self.location = [a + b for a, b in zip(self.location, [x, y, z])]
        self.translation = numpy.dot(self.translation , translation([x, y, z]))

    def scale(self, x, y, z):
        self.scale = [a * b for a, b in zip(self.location, [x, y, z])]
        self.scalemat = numpy.dot(self.scalemat , scaling([x, y, z]))

    def set_color(self, r, g, b):
        self.color = [r, g, b]

    def picking(self, start, direction, mat):
        glPushMatrix()
        glScale(self.scale[0], self.scale[1], self.scale[2])
        glTranslated(self.location[0], self.location[1], self.location[2])
        glColor3f(self.color[0], self.color[1], self.color[2])
        glCallList(self.call_list)

        newmat = numpy.dot(self.translation, numpy.dot(mat, self.scalemat))
        results = self.aabb.ray_hit(start, direction, newmat)
        return results


class Sphere(Node):
    """ Sphere primitive """
    def __init__(self):
        super(Sphere, self).__init__()
        self.call_list = G_OBJ_SPHERE
        self.aabb = AABB([0.0, 0.0, 0.0], [0.5, 0.5, 0.5])

class Cube(Node):
    """ Cube primitive """
    def __init__(self):
        super(Cube, self).__init__()
        self.call_list = G_OBJ_CUBE
        self.aabb = AABB([0.5, 0.5, 0.5], [0.5, 0.5, 0.5])


