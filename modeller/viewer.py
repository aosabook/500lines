#! /usr/bin/env python
from OpenGLContext import testingcontext, quaternion, contextdefinition
BaseContext = testingcontext.getInteractive()

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *

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

        # TODO: remove
        self.InitDebug()

    def Render(self, mode = None):
        BaseContext.Render(self, mode)

        xSize, ySize = glutGet( GLUT_WINDOW_WIDTH ), glutGet( GLUT_WINDOW_HEIGHT )

        glMatrixMode(GL_PROJECTION);
        glLoadIdentity();
        glViewport(0, 0, xSize, ySize);
        gluPerspective(40.0, 1.0, 0.1, 1000.0);
        loc = self.mouse_interaction.camera_loc
        glTranslated(loc[0], loc[1], loc[2]);

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

        self.scene.render()
        glCallList(G_OBJ_PLANE)
        glPopMatrix()

    def InitDebug(self):
#        sphere_node = Sphere()
#        self.scene.add_node(sphere_node)

        cube_node = Cube()
        self.scene.add_node(cube_node)

if __name__=="__main__":
    TestContext.ContextMainLoop( definition = contextdefinition.ContextDefinition(size = (500,500),))
