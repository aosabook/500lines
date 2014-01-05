#! /usr/bin/env python
from OpenGLContext import testingcontext
from OpenGLContext import quaternion
from OpenGLContext import contextdefinition
from OpenGLContext.arrays import *
from OpenGLContext.scenegraph.basenodes import Sphere
BaseContext = testingcontext.getInteractive()

from OpenGL.GL import *
from OpenGL.GL import shaders
from OpenGL.GLU import *
from OpenGL.GLUT import *
from OpenGL.arrays import vbo
OBJ_PLANE = 1

from mouse_interaction import MouseInteraction

class TestContext(BaseContext):
    def OnInit(self):
        self.MakePlane()
        self.mouse_interaction = MouseInteraction()
        self.mouse_interaction.register()
        glPushMatrix()
        glLoadIdentity()
        glTranslatef(0, 0, -10)

        glPopMatrix()

    def Lights(self, mode = None):
        """ called by default rendering process iff there is no scenegraph present """
        glEnable(GL_LIGHTING)
        glEnable(GL_LIGHT0)

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

        xSize, ySize = glutGet( GLUT_WINDOW_WIDTH ), glutGet( GLUT_WINDOW_HEIGHT )

        glMatrixMode(GL_PROJECTION);
        glLoadIdentity();
        glViewport(0, 0, xSize, ySize);
        gluPerspective(40.0, 1.0, 0.1, 1000.0);
        loc = self.mouse_interaction.camera_loc

        glTranslated(loc[0], loc[1], loc[2]);
        # use glMultMatrix


        glMatrixMode(GL_MODELVIEW);
        glEnable(GL_LIGHTING)
        glEnable(GL_LIGHT0)
        glLightfv(GL_LIGHT0, GL_POSITION, GLfloat_4(0, 0, -10, 1))
        glLightfv(GL_LIGHT0, GL_SPOT_DIRECTION, GLfloat_3(0, 0, 1))

        glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE)
        glEnable( GL_COLOR_MATERIAL)

        glClearColor(0.4, 0.4, 0.4, 0.0);
        glClear(GL_COLOR_BUFFER_BIT)
        glPushMatrix()
        glLoadIdentity()
        glMultMatrixf(self.mouse_interaction.rotation.matrix(inverse=False))

        glCallList(OBJ_PLANE)
        glPopMatrix()

if __name__=="__main__":
    TestContext.ContextMainLoop( definition = contextdefinition.ContextDefinition(size = (500,500),))
