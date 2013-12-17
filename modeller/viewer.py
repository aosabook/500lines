#! /usr/bin/env python
from OpenGLContext import testingcontext
from OpenGLContext import quaternion
from OpenGLContext.arrays import *
from OpenGLContext.scenegraph.basenodes import Sphere
BaseContext = testingcontext.getInteractive()

from OpenGL.GL import *
from OpenGL.GL import shaders
from OpenGL.GLU import *
from OpenGL.arrays import vbo
OBJ_PLANE = 1

class TestContext(BaseContext):
    def OnInit(self):
        self.MakePlane()
        self.rotation = quaternion.fromEuler()
        glPushMatrix()
        glLoadIdentity()
        glTranslatef(0, 0, -20)
        self.translation = glGetFloatv(GL_MODELVIEW_MATRIX)
        glPopMatrix()

    def Lights(self, mode = None):
        """ called by default rendering process iff there is no scenegraph present """
        glEnable(GL_LIGHTING)
        glEnable(GL_LIGHT0)
        print "LIGHTING"

    def MakePlane(self):
        glNewList(OBJ_PLANE, GL_COMPILE)
        glBegin(GL_LINES)
        glColor3d(0,0,0)
        for i in xrange(41):
            glVertex3f(-10.0 + 0.5 * i, 0, -10)
            glVertex3f(-10.0 + 0.5 * i, 0, 10)
            glVertex3f(-10.0 , 0, -10 + 0.5 * i)
            glVertex3f(10.0 , 0, -10 + 0.5 * i)

        # Axes
        glEnd()
        glLineWidth(5)

        glBegin(GL_LINES)
        glColor3f(0.5, 0.7, 0.5)
        glVertex3f(0.0, 0.0, 0.0)
        glVertex3f(5, 0.0, 0.0)
        glEnd()

        glBegin(GL_LINES)
        glColor3f(0.5, 0.7, 0.5)
        glVertex3f(0.0, 0.0, 0.0)
        glVertex3f(0.0, 5, 0.0)
        glEnd()

        glBegin(GL_LINES)
        glColor3f(0.5, 0.7, 0.5)
        glVertex3f(0.0, 0.0, 0.0)
        glVertex3f(0.0, 0.0, 5)
        glEnd()

        # Draw the Y.
        glBegin(GL_LINES)
        glColor3f(0.0, 0.0, 0.0)
        glVertex3f(0.0, 5.0, 0.0)
        glVertex3f(0.0, 5.5, 0.0)
        glVertex3f(0.0, 5.5, 0.0)
        glVertex3f(-0.5, 6.0, 0.0)
        glVertex3f(0.0, 5.5, 0.0)
        glVertex3f(0.5, 6.0, 0.0)

        # Draw the Z.
        glVertex3f(-0.5, 0.0, 5.0)
        glVertex3f(0.5, 0.0, 5.0)
        glVertex3f(0.5, 0.0, 5.0)
        glVertex3f(-0.5, 0.0, 6.0)
        glVertex3f(-0.5, 0.0, 6.0)
        glVertex3f(0.5, 0.0, 6.0)

        # Draw the X.
        glVertex3f(5.0, 0.0, 0.5)
        glVertex3f(6.0, 0.0, -0.5)
        glVertex3f(5.0, 0.0, -0.5)
        glVertex3f(6.0, 0.0, 0.5)

        glEnd()
        glLineWidth(1)
        glEndList()

    def Render(self, mode = None):
        BaseContext.Render(self, mode)

        glEnable(GL_LIGHTING)
        glEnable(GL_LIGHT0)
        glLightfv(GL_LIGHT0, GL_POSITION, GLfloat_4(0, 0, -15, 1))
        glLightfv(GL_LIGHT0, GL_SPOT_DIRECTION, GLfloat_3(0, 0, 1))

        glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE)
        glEnable( GL_COLOR_MATERIAL)

        glClearColor(0.4, 0.4, 0.4, 0.0);
        glClear(GL_COLOR_BUFFER_BIT)
        glMatrixMode(GL_MODELVIEW)
        glPushMatrix()
        glLoadIdentity()
        glMultMatrixf(self.translation)
        glMultMatrixf(self.rotation.matrix(inverse=True))

        glCallList(OBJ_PLANE)
#        self.MakePlane()
        glPopMatrix()

if __name__=="__main__":
    TestContext.ContextMainLoop()
