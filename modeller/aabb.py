from OpenGL.GL import *
from primitive import G_OBJ_CUBE
import numpy
import math
from transformation import scaling, translation


EPSILON = 0.000001

class AABB(object):

    def __init__(self, center, size):
        self.center = numpy.array(center)
        self.size = numpy.array(size)

    def intersects_aabb(self, other):
        diff = [a - b for a, b in zip(self.center, other.center)]

        if abs(diff[0]) > (size[0] + other.size[0]): return False
        if abs(diff[1]) > (size[1] + other.size[1]): return False
        if abs(diff[2]) > (size[2] + other.size[2]): return False

        return True

    def ray_hit(self, origin, direction, modelmatrix):
        aabb_min = self.center - self.size
        aabb_max = self.center + self.size
        tmin = 0.0
        tmax = 100000.0


        obb_pos_worldspace = numpy.array([modelmatrix[3, 0], modelmatrix[3, 1], modelmatrix[3, 2]])
        delta = (obb_pos_worldspace - origin)

        # test intersection with 2 planes perpendicular to OBB's x-axis
        xaxis = numpy.array((modelmatrix[0, 0], modelmatrix[1, 0], modelmatrix[2, 0]))

        e = numpy.dot(xaxis, delta)
        f = numpy.dot(direction, xaxis)
        if math.fabs(f) > 0.0 + EPSILON:
            t1 = (e + aabb_min[0])/f
            t2 = (e + aabb_max[0])/f
            if t1 > t2:
                t1, t2 = t2, t1
            if t2 < tmax:
                tmax = t2
            if t1 > tmin:
                tmin = t1
            if tmax < tmin:
                return (False, 0)
        else:
            if (-e + aabb_min[0] > 0.0 + EPSILON) or (-e+aabb_max[0] < 0.0 - EPSILON):
                return False, 0

        yaxis = numpy.array((modelmatrix[0, 1], modelmatrix[1, 1], modelmatrix[2, 1]))
        e = numpy.dot(yaxis, delta)
        f = numpy.dot(direction, yaxis)
        # intersection in y
        if math.fabs(f)> 0.0 + EPSILON:
            t1 = (e + aabb_min[1])/f
            t2 = (e + aabb_max[1])/f
            if t1 > t2:
                t1, t2 = t2, t1
            if t2 < tmax:
                tmax = t2
            if t1 > tmin:
                tmin = t1
            if tmax < tmin:
                return (False, 0)
        else:
            if (-e + aabb_min[1] > 0.0 + EPSILON) or (-e+aabb_max[1] < 0.0 - EPSILON):
                return False, 0

        # intersection in z
        zaxis = numpy.array((modelmatrix[0, 2], modelmatrix[1, 2], modelmatrix[2, 2]))
        e = numpy.dot(zaxis, delta)
        f = numpy.dot(direction, zaxis)
        if math.fabs(f)> 0.0 + EPSILON:
            t1 = (e + aabb_min[2])/f
            t2 = (e + aabb_max[2])/f
            if t1 > t2:
                t1, t2 = t2, t1
            if t2 < tmax:
                tmax = t2
            if t1 > tmin:
                tmin = t1
            if tmax < tmin:
                return (False, 0)
        else:
            if (-e + aabb_min[2] > 0.0 + EPSILON) or (-e+aabb_max[2] < 0.0 - EPSILON):
                return False, 0

        return True, tmin

    def render(self):
        glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
        glMatrixMode(GL_MODELVIEW);
        glPushMatrix();
        glTranslated(self.center[0] - self.size[0], self.center[1] - self.size[1], self.center[2] - self.size[2]);
        glScaled(2*self.size[0], 2*self.size[1], 2*self.size[2]);
        glCallList(G_OBJ_CUBE);
        glPopMatrix();
        glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
