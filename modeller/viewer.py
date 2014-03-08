#! /usr/bin/env python
from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from numpy.linalg import norm
import numpy

from interaction import Interaction
from primitive import InitPrimitives, G_OBJ_PLANE
from node import Sphere, Cube
from scene import Scene
from transformation import make_perspective

class Viewer(object):
    def __init__(self):
        """ Initialize the context. """
        glutInit()
        glutInitWindowSize(640,480)
        glutCreateWindow("3D Modeller")
        glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB)
        glClearColor(1.0,1.0,1.0,0.0)
        glutDisplayFunc(self.Render)
        self.initialize()

    def initialize(self):
        InitPrimitives()
        self.interaction = Interaction()
        self.interaction.register()
        self.scene = Scene()

        self.interaction.registerCallback('picking', self.picking)
        self.interaction.registerCallback('move', self.move)
        self.interaction.registerCallback('place', self.place)
        self.interaction.registerCallback('color', self.color)
        self.interaction.registerCallback('scale', self.scale)

        self.InitialScene()
        self.inverseModelView = numpy.identity(4)
        self.modelView = numpy.identity(4)

        self.InitOpenGL()

    def InitOpenGL(self):
        glEnable(GL_CULL_FACE)
        glCullFace(GL_BACK)
        glEnable(GL_DEPTH_TEST)
        glDepthFunc(GL_LESS)

        glEnable(GL_LIGHT0)
        glLightfv(GL_LIGHT0, GL_POSITION, GLfloat_4(0, 0, 1, 0))
        glLightfv(GL_LIGHT0, GL_SPOT_DIRECTION, GLfloat_3(0, 0, -1))

        glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE)
        glEnable( GL_COLOR_MATERIAL)

    def MainLoop(self):
        glutMainLoop()

    def Render(self):
        """ The render pass for the scene """
        self.initView()

        # Enable lighting and color
        glEnable(GL_LIGHTING)

        glClearColor(0.4, 0.4, 0.4, 0.0);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

        # Load the modelview matrix from the current state of the trackball
        glMatrixMode(GL_MODELVIEW);
        glPushMatrix()
        glLoadIdentity()
        loc = self.interaction.camera_loc
        glTranslated(-loc[0], -loc[1], -loc[2])
        glMultMatrixf(self.interaction.trackball.matrix)

        # store the inverse of the current modelview.
        mat = numpy.array(glGetFloatv( GL_MODELVIEW_MATRIX ))
        self.inverseModelView = numpy.linalg.inv(numpy.transpose(mat))
        self.modelView = numpy.transpose(mat)

        # Render the scene. This will call the render function for each object in the scene
        self.scene.render()

        # draw the grid
        glDisable(GL_LIGHTING)
        glCallList(G_OBJ_PLANE)
        glPopMatrix()

        # flush the buffer to draw to the screen!
        glFlush()

    def initView(self):
        """ initialize the projection matrix """
        xSize, ySize = glutGet( GLUT_WINDOW_WIDTH ), glutGet( GLUT_WINDOW_HEIGHT )
        aspect_rat = float(xSize) / float(ySize)

        # load the projection matrix. Always the same
        glMatrixMode(GL_PROJECTION);
        glLoadIdentity();

        glViewport(0, 0, xSize, ySize);
        gluPerspective(70, aspect_rat , 0.1, 1000.0)
        glTranslated(0, 0, -15);

    def InitialScene(self):
        cube_node = Cube()
        cube_node.translate(2, 0, 2)
        cube_node.color_index = 2
        self.scene.add_node(cube_node)

        cube_node = Sphere()
        cube_node.translate(-2, 0, 2)
        cube_node.color_index = 3
        self.scene.add_node(cube_node)

        cube_node = Sphere()
        cube_node.translate(-2, 0, -2)
        cube_node.color_index = 1
        self.scene.add_node(cube_node)

    def getRay(self, x, y):
        """ Generate a ray beginning at the near plane, in the direction that the x, y coordinates are facing
            Consumes: x, y coordinates of mouse on screen
            Return: start, direction of the ray """
        self.initView()

        glMatrixMode(GL_MODELVIEW);
        glLoadIdentity()

        # get two points on the line.
        start = numpy.array(gluUnProject(x, y, 0.001))
        end = numpy.array(gluUnProject(x, y, 0.999))

        # convert those points into a ray
        direction = end - start
        direction = direction / norm(direction)

        return (start, direction)

    def picking(self, x, y):
        """ Execute picking of an object. Selects an object in the scene.
            Consumes: x, y coordinates of the mouse on the screen """
        start, direction = self.getRay(x, y)
        self.scene.picking(start, direction, self.modelView)

    def move(self, x, y):
        """ Execute a move command on the scene.
            Consumes: x, y coordinates of the mouse on the screen """
        start, direction = self.getRay(x, y)
        self.scene.move(start, direction, self.inverseModelView)

    def place(self, shape, x, y):
        """ Execute a placement of a new primitive into the scene.
            Consumes: x, y coordinates of the mouse on the screen """
        start, direction = self.getRay(x, y)
        self.scene.place(shape, start, direction, self.inverseModelView)
    
    def color(self, forward):
        """ Rotate the color of the selected Node. Boolean 'forward' indicates direction of rotation. """
        self.scene.rotate_color(forward)

    def scale(self, up):
        """ Scale the selected Node. Boolean up indicates scaling larger."""
        self.scene.scale(up)

if __name__=="__main__":
    viewer = Viewer()
    viewer.MainLoop()
