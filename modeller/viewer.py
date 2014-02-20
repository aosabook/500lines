#! /usr/bin/env python
from OpenGLContext import testingcontext, quaternion, contextdefinition
BaseContext = testingcontext.getInteractive()

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from OpenGLContext.arrays import *
from numpy.linalg import norm
import numpy

from interaction import Interaction
from primitive import InitPrimitives, G_OBJ_PLANE
from node import Sphere, Cube
from scene import Scene
from transformation import make_perspective

class TestContext(BaseContext):
    def OnInit(self):
        """ Initialize the context. """
        InitPrimitives()
        self.interaction = Interaction()
        self.interaction.register()
        self.scene = Scene()

        self.interaction.registerCallback('picking', self.picking)
        self.interaction.registerCallback('move', self.move)
        self.interaction.registerCallback('place', self.place)

        self.InitialScene()

    def Render(self, mode = None):
        """ The render pass for the scene """
        BaseContext.Render(self, mode)


        self.initView()

        # Enable lighting and color
        glEnable(GL_LIGHTING)
        glEnable(GL_LIGHT0)
        glLightfv(GL_LIGHT0, GL_POSITION, GLfloat_4(0, 0, 1, 0))
        glLightfv(GL_LIGHT0, GL_SPOT_DIRECTION, GLfloat_3(0, 0, -1))

        glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE)
        glEnable( GL_COLOR_MATERIAL)

        glClearColor(0.4, 0.4, 0.4, 0.0);
        glClear(GL_COLOR_BUFFER_BIT)

        # Load the modelview matrix from the current state of the trackball
        glMatrixMode(GL_MODELVIEW);
        glPushMatrix()
        glLoadIdentity()
        loc = self.interaction.camera_loc
        glTranslated(-loc[0], -loc[1], -loc[2])
        glMultMatrixf(self.interaction.rotation.matrix(inverse=False))

        # Render the scene. This will call the render function for each object in the scene
        self.scene.render()

        # draw the grid
        glDisable(GL_LIGHTING)
        glCallList(G_OBJ_PLANE)
        glPopMatrix()

    def initView(self):
        xSize, ySize = glutGet( GLUT_WINDOW_WIDTH ), glutGet( GLUT_WINDOW_HEIGHT )
        aspect_rat = float(xSize) / float(ySize)

        # load the projection matrix. Always the same
        glMatrixMode(GL_PROJECTION);
        glLoadIdentity();

        glViewport(0, 0, xSize, ySize);
        gluPerspective(70, aspect_rat , 0.1, 1000.0)
        glTranslated(0, 0, -15);

    def InitialScene(self):
        sphere_node = Cube()
        sphere_node.set_color(0.5, 0.4, 0.2)
        self.scene.add_node(sphere_node)

        cube_node = Cube()
        cube_node.translate(2, 0, 2)
        cube_node.set_color(0.2, 0.6, 0.2)
        self.scene.add_node(cube_node)

        cube_node = Sphere()
        cube_node.translate(-2, 0, 2)
        cube_node.set_color(0.6, 0.2, 0.2)
        self.scene.add_node(cube_node)

        cube_node = Sphere()
        cube_node.translate(-2, 0, -2)
        cube_node.set_color(0.2, 0.2, 0.6)
        self.scene.add_node(cube_node)

    def getRay(self, x, y):
        """ Generate a ray beginning at the near plane, in the direction that the x, y coordinates are facing
            Consumes: x, y coordinates of mouse on screen
            Return: start, direction of the ray """
        self.initView()

        glMatrixMode(GL_MODELVIEW);
        glLoadIdentity()

        # get two points on the line.
        start = array(gluUnProject(x, y, 0.001))
        end = array(gluUnProject(x, y, 0.999))

        # convert those points into a ray
        direction = end - start
        direction = direction / norm(direction)

        return (start, direction)

    def getModelView(self):
        """ Fetches the current state of the modelview matrix """
        # TODO: store this as a member variable, and maybe its inverse too
        loc = self.interaction.camera_loc
        glMatrixMode(GL_MODELVIEW);
        glLoadIdentity()
        glTranslated(-loc[0], -loc[1], -loc[2])
        glMultMatrixf(self.interaction.rotation.matrix(inverse=False))

        mat = numpy.array(glGetFloatv( GL_MODELVIEW_MATRIX ))
        return mat

    def picking(self, x, y):
        """ Execute picking of an object. Selects an object in the scene.
            Consumes: x, y coordinates of the mouse on the screen """
        start, direction = self.getRay(x, y)
        mat = self.getModelView()
        self.scene.picking(start, direction, mat)

    def move(self, x, y):
        """ Execute a move command on the scene.
            Consumes: x, y coordinates of the mouse on the screen """
        start, direction = self.getRay(x, y)
        mat = self.getModelView()
        self.scene.move(start, direction, mat)

    def place(self, shape, x, y):
        """ Execute a placement of a new primitive into the scene.
            Consumes: x, y coordinates of the mouse on the screen """
        start, direction = self.getRay(x, y)
        mat = self.getModelView()
        self.scene.place(shape, start, direction, mat)


if __name__=="__main__":
    TestContext.ContextMainLoop( definition = contextdefinition.ContextDefinition(size = (500,500),))
