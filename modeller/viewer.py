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
        self.mouse_interaction.registerCallback('move', self.move)
        self.mouse_interaction.registerCallback('place', self.place)

        self.perspective = self.make_perspective(70, 1.0, 0.1, 1000.0)

        # TODO: remove
        self.InitDebug()


    def make_perspective(self, fov, aspect, near, far):
        fov = fov * math.pi / 180;
        f = 1/tan(fov/2);
        persp_mat = numpy.zeros((4, 4))
        persp_mat[0][0] = f/aspect;
        persp_mat[0][1] = 0;
        persp_mat[0][2] = 0;
        persp_mat[0][3] = 0;

        persp_mat[1][0] = 0;
        persp_mat[1][1] = f;
        persp_mat[1][2] = 0;
        persp_mat[1][3] = 0;

        persp_mat[2][0] = 0;
        persp_mat[2][1] = 0;
        persp_mat[2][2] = (far+near)/(near-far);
        persp_mat[2][3] = (2*far*near)/(near-far);

        persp_mat[3][0] = 0;
        persp_mat[3][1] = 0;
        persp_mat[3][2] = -1;
        persp_mat[3][3] = 0;

        return numpy.transpose(persp_mat)


    def Render(self, mode = None):
        BaseContext.Render(self, mode)

        xSize, ySize = glutGet( GLUT_WINDOW_WIDTH ), glutGet( GLUT_WINDOW_HEIGHT )

        glMatrixMode(GL_PROJECTION);
        glLoadIdentity();
        glViewport(0, 0, xSize, ySize);
        glMultMatrixf(self.perspective)
        glTranslated(0, 0, -15);
        loc = self.mouse_interaction.camera_loc

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

        cube_node = Sphere()
        cube_node.translate(2, 0, -2)
        cube_node.set_color(0.4, 0.4, 0.4)
        self.scene.add_node(cube_node)

    def getRay(self, x, y):
        xSize, ySize = glutGet( GLUT_WINDOW_WIDTH ), glutGet( GLUT_WINDOW_HEIGHT )
        glMatrixMode(GL_PROJECTION)
        glLoadIdentity()
        glViewport(0, 0, xSize, ySize)
        glMultMatrixf(self.perspective)
        glTranslated(0, 0, -15)

        glMatrixMode(GL_MODELVIEW)
        glLoadIdentity()

        start = array(gluUnProject(x, y, 0.001))
        end = array(gluUnProject(x, y, 0.999))

        # reverse the y coordinate because of screen y direction differing from opengl y direction
        start[1] = -start[1]
        end[1] = -end[1]

        direction = end - start
        direction = direction / norm(direction)

        print "RAY"
        print (start, direction)
        return (start, direction)

    def getModelView(self):
        loc = self.mouse_interaction.camera_loc
        glMatrixMode(GL_MODELVIEW);
        glLoadIdentity()
        glTranslated(-loc[0], -loc[1], -loc[2])
        glMultMatrixf(self.mouse_interaction.rotation.matrix(inverse=False))

        mat = numpy.array(glGetFloatv( GL_MODELVIEW_MATRIX ))
        return mat


    def picking(self, x, y):
        start, direction = self.getRay(x, y)
        mat = self.getModelView()
        self.scene.picking(start, direction, mat)

    def move(self, x, y):
        start, direction = self.getRay(x, y)
        mat = self.getModelView()
        self.scene.move(start, direction, mat)

    def place(self, shape, x, y):
        start, direction = self.getRay(x, y)
        mat = self.getModelView()
        self.scene.place(shape, start, direction, mat)





if __name__=="__main__":
    TestContext.ContextMainLoop( definition = contextdefinition.ContextDefinition(size = (500,500),))
