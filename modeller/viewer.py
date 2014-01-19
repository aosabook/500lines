#! /usr/bin/env python
from OpenGLContext import testingcontext, quaternion, contextdefinition
BaseContext = testingcontext.getInteractive()

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from OpenGLContext.arrays import *
from numpy.linalg import norm
import numpy

from mouse_interaction import MouseInteraction
from primitive import InitPrimitives, G_OBJ_PLANE
from node import Sphere, Cube
from scene import Scene

class TestContext(BaseContext):
    def OnInit(self):
        InitPrimitives()
        self.mouse_interaction = MouseInteraction()
        self.mouse_interaction.register()
        self.scene = Scene()

        self.mouse_interaction.registerCallback('picking', self.picking)

        # TODO: remove
        self.InitDebug()

    def Render(self, mode = None):
        BaseContext.Render(self, mode)

        xSize, ySize = glutGet( GLUT_WINDOW_WIDTH ), glutGet( GLUT_WINDOW_HEIGHT )

        glMatrixMode(GL_PROJECTION);
        glLoadIdentity();
        glViewport(0, 0, xSize, ySize);
        gluPerspective(70.0, 1.0, 0.1, 1000.0);
        loc = self.mouse_interaction.camera_loc
        glTranslated(0, 0, -15);

        glMatrixMode(GL_MODELVIEW);
        glEnable(GL_LIGHTING)
        glEnable(GL_LIGHT0)
        glLightfv(GL_LIGHT0, GL_POSITION, GLfloat_4(0, 0, 1, 0))
        glLightfv(GL_LIGHT0, GL_SPOT_DIRECTION, GLfloat_3(0, 0, -1))

        glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE)
        glEnable( GL_COLOR_MATERIAL)

        glClearColor(0.4, 0.4, 0.4, 0.0);
        glClear(GL_COLOR_BUFFER_BIT)
        glPushMatrix()
        glLoadIdentity()
        glTranslated(-loc[0], -loc[1], -loc[2])
        glMultMatrixf(self.mouse_interaction.rotation.matrix(inverse=False))

        self.scene.render()

        glDisable(GL_LIGHTING)
        glCallList(G_OBJ_PLANE)
        glPopMatrix()

    def InitDebug(self):
        sphere_node = Sphere()
        sphere_node.set_color(0.5, 0.4, 0.2)
        self.scene.add_node(sphere_node)

        cube_node = Cube()
        cube_node.translate(2, 0, 2)
        cube_node.set_color(0.2, 0.6, 0.2)
        self.scene.add_node(cube_node)

    def picking(self, x, y):
        # render with each object having its own color
        # query screen to figure out what pixel is shown
        xSize, ySize = glutGet( GLUT_WINDOW_WIDTH ), glutGet( GLUT_WINDOW_HEIGHT )
        glMatrixMode(GL_PROJECTION);
        glLoadIdentity();
        glViewport(0, 0, xSize, ySize);
        gluPerspective(70.0, 1.0, 0.1, 1000.0);
        loc = self.mouse_interaction.camera_loc
        glTranslated(0, 0, -15);

        start = array(gluUnProject(x, y, 0.0))
        end = array(gluUnProject(x, y, 1.0))

        # reverse the y coordinate because of screen y direction differing from opengl y direction
        start[1] = -start[1]
        end[1] = -end[1]

        direction = end - start
        direction = direction / norm(direction)
        glMatrixMode(GL_MODELVIEW);
        glTranslated(-loc[0], -loc[1], -loc[2])
        glMultMatrixf(self.mouse_interaction.rotation.matrix(inverse=False))

        mat = numpy.array(glGetFloatv( GL_MODELVIEW_MATRIX ))
        self.scene.picking(start, direction, mat)




if __name__=="__main__":
    TestContext.ContextMainLoop( definition = contextdefinition.ContextDefinition(size = (500,500),))
