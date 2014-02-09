from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGLContext.scenegraph.basenodes import Sphere

G_OBJ_PLANE = 1
G_OBJ_SPHERE = 2
G_OBJ_CUBE = 3

def MakePlane():
    glNewList(G_OBJ_PLANE, GL_COMPILE)
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

def MakeSphere():
    glNewList(G_OBJ_SPHERE, GL_COMPILE)
    quad = gluNewQuadric();
    gluSphere(quad, 0.5, 30, 30);
    gluDeleteQuadric(quad);
    glEndList();

def MakeCube():
    glNewList(G_OBJ_CUBE, GL_COMPILE)

    glBegin(GL_QUADS);

    glNormal3d(-1.0, 0.0, 0.0)
    glVertex3d(-0.5, -0.5, -0.5)
    glVertex3d(-0.5, -0.5, 0.5)
    glVertex3d(-0.5, 0.5, 0.5)
    glVertex3d(-0.5, 0.5, -0.5)

    glNormal3d(0.0, 0.0, -1.0)
    glVertex3d(-0.5, -0.5, -0.5)
    glVertex3d(-0.5, 0.5, -0.5)
    glVertex3d(0.5, 0.5, -0.5)
    glVertex3d(0.5, -0.5, -0.5)

    glNormal3d(1.0, 0.0, 0.0)
    glVertex3d(0.5, -0.5, -0.5)
    glVertex3d(0.5, 0.5, -0.5)
    glVertex3d(0.5, 0.5, 0.5)
    glVertex3d(0.5, -0.5, 0.5)

    glNormal3d(0.0, 0.0, 1.0)
    glVertex3d(-0.5, -0.5, 0.5)
    glVertex3d(0.5, -0.5, 0.5)
    glVertex3d(0.5, 0.5, 0.5)
    glVertex3d(-0.5, 0.5, 0.5)

    glNormal3d(0.0, -1.0, 0.0)
    glVertex3d(-0.5, -0.5, 0.5)
    glVertex3d(-0.5, -0.5, -0.5)
    glVertex3d(0.5, -0.5, -0.5)
    glVertex3d(0.5, -0.5, 0.5)


    glNormal3d(0.0, 1.0, 0.0)
    glVertex3d(-0.5, 0.5, -0.5)
    glVertex3d(-0.5, 0.5, 0.5)
    glVertex3d(0.5, 0.5, 0.5)
    glVertex3d(0.5, 0.5, -0.5)


    glEnd()
    glEndList()



def InitPrimitives():
    MakePlane()
    MakeSphere()
    MakeCube()
