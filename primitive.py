from OpenGL.GL import glBegin, glColor3f, glEnd, glEndList, glLineWidth, glNewList, glNormal3f, glVertex3f, glRotatef, \
                      GL_COMPILE, GL_LINES, GL_QUADS, GL_TRIANGLES, glTranslate, glPushMatrix, glPopMatrix
from OpenGL.GLU import gluDeleteQuadric, gluNewQuadric, gluSphere, gluCylinder, gluDisk

G_OBJ_PLANE = 1
G_OBJ_SPHERE = 2
G_OBJ_CUBE = 3
G_OBJ_SQUAREPYRAMID = 4
G_OBJ_TRIANGLEPYRAMID = 5
G_OBJ_CYLINDER = 6
G_OBJ_CONE = 7
G_OBJ_REVERSECONE = 8


def make_plane():
    glNewList(G_OBJ_PLANE, GL_COMPILE)
    glBegin(GL_LINES)
    glColor3f(0, 0, 0)
    for i in xrange(41):
        glVertex3f(-10.0 + 0.5 * i, 0, -10)
        glVertex3f(-10.0 + 0.5 * i, 0, 10)
        glVertex3f(-10.0, 0, -10 + 0.5 * i)
        glVertex3f(10.0, 0, -10 + 0.5 * i)

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


def make_sphere():
    glNewList(G_OBJ_SPHERE, GL_COMPILE)
    quad = gluNewQuadric()
    gluSphere(quad, 0.5, 30, 30)
    gluDeleteQuadric(quad)
    glEndList()


def make_cube():
    glNewList(G_OBJ_CUBE, GL_COMPILE)
    vertices = [((-0.5, -0.5, -0.5), (-0.5, -0.5, 0.5), (-0.5, 0.5, 0.5), (-0.5, 0.5, -0.5)),
                ((-0.5, -0.5, -0.5), (-0.5, 0.5, -0.5), (0.5, 0.5, -0.5), (0.5, -0.5, -0.5)),
                ((0.5, -0.5, -0.5), (0.5, 0.5, -0.5), (0.5, 0.5, 0.5), (0.5, -0.5, 0.5)),
                ((-0.5, -0.5, 0.5), (0.5, -0.5, 0.5), (0.5, 0.5, 0.5), (-0.5, 0.5, 0.5)),
                ((-0.5, -0.5, 0.5), (-0.5, -0.5, -0.5), (0.5, -0.5, -0.5), (0.5, -0.5, 0.5)),
                ((-0.5, 0.5, -0.5), (-0.5, 0.5, 0.5), (0.5, 0.5, 0.5), (0.5, 0.5, -0.5))]
    normals = [(-1.0, 0.0, 0.0), (0.0, 0.0, -1.0), (1.0, 0.0, 0.0), (0.0, 0.0, 1.0), (0.0, -1.0, 0.0), (0.0, 1.0, 0.0)]

    glBegin(GL_QUADS)
    for i in xrange(6):
        glNormal3f(normals[i][0], normals[i][1], normals[i][2])
        for j in xrange(4):
            glVertex3f(vertices[i][j][0], vertices[i][j][1], vertices[i][j][2])
    glEnd()
    glEndList()

def make_squarePyramid():
    glNewList(G_OBJ_SQUAREPYRAMID, GL_COMPILE)
    vertices = [((0.0, 0.5, 0.0), (-0.5, -0.5, -0.5), (0.5, -0.5, -0.5), (0.0, 0.5, 0.0)),
                ((0.0, 0.5, 0.0), (-0.5, -0.5, 0.5), (-0.5, -0.5, -0.5), (0.0, 0.5, 0.0)),
                ((0.0, 0.5, 0.0), (0.5, -0.5, -0.5), (0.5, -0.5, 0.5), (0.0, 0.5, 0.0)), 
                ((0.0, 0.5, 0.0), (0.5, -0.5, 0.5), (-0.5, -0.5, 0.5), (0.0, 0.5, 0.0)),                       
                ((-0.5, -0.5, -0.5), (-0.5, -0.5, 0.5), (0.5, -0.5, -0.5), (0.5, -0.5, 0.5))]
    normals = [(0.0, 0.0, 1.0), (-1.0, 0.0, 0.0), (1.0, 0.0, 0.0), (0.0, 0.0, -1.0), (0.0, -1.0, 0.0)]
	
    glBegin(GL_TRIANGLES)
    for i in xrange(4):
	    glNormal3f(normals[i][0], normals[i][1], normals[i][2])
	    for j in xrange(3):
		    glVertex3f(vertices[i][j][0], vertices[i][j][1], vertices[i][j][2])
    glNormal3f(normals[4][0], normals[4][1], normals[4][2])
    for i in xrange(3):
    	glVertex3f(vertices[4][i][0], vertices[4][i][1], vertices[4][i][2])
    for i in xrange(3):
    	glVertex3f(vertices[4][i+1][0], vertices[4][i+1][1], vertices[4][i+1][2])
    glEnd()
    glEndList()

def make_trianglePyramid():
    glNewList(G_OBJ_TRIANGLEPYRAMID, GL_COMPILE)
    vertices = [((0.0, 0.5, 0.0), (-0.5, -0.5, -0.5), (0.5, -0.5, -0.5)),
                ((0.0, 0.5, 0.0), (-0.5, -0.5, 0.5), (-0.5, -0.5, -0.5)),
                ((0.0, 0.5, 0.0), (0.5, -0.5, -0.5), (-0.5, -0.5, 0.5)),                       
                ((-0.5, -0.5, -0.5), (-0.5, -0.5, 0.5), (0.5, -0.5, -0.5))]
    normals = [(0.0, 0.0, 1.0), (-1.0, 0.0, 0.0), (1.0, 0.0, 0.0), (0.0, -1.0, 0.0)]
	
    glBegin(GL_TRIANGLES)
    for i in xrange(4):
	    glNormal3f(normals[i][0], normals[i][1], normals[i][2])
	    for j in xrange(3):
		    glVertex3f(vertices[i][j][0], vertices[i][j][1], vertices[i][j][2])
    glEnd()
    glEndList()

def make_cylinder():
    glNewList(G_OBJ_CYLINDER, GL_COMPILE)
    quad = gluNewQuadric()
    glTranslate(0.0, -0.5, 0.0)
    glRotatef(90.0, -1.0, 0.0, 0.0)
    gluCylinder(quad, 0.5, 0.5, 1, 20, 30)
    glPushMatrix()   
    glTranslate(0.0, 0.0, 1)
    gluDisk(quad, 0.0, 0.5, 20, 30)
    glPopMatrix()
    glRotatef(180.0, 0.0, 0.0, 1.0)
    gluDisk(quad, 0.0, 0.5, 20, 30)
    gluDeleteQuadric(quad)
    glEndList()

def make_cone():
    glNewList(G_OBJ_CONE, GL_COMPILE)
    quad = gluNewQuadric()
    glTranslate(0.0, -0.5, 0.0)
    glRotatef(90.0, -1.0, 0.0, 0.0)
    gluCylinder(quad, 0.5, 0.0, 1, 20, 30)
    gluDisk(quad, 0.0, 0.5, 20, 30)
    gluDeleteQuadric(quad)
    glEndList()

def make_reverseCone():
    glNewList(G_OBJ_REVERSECONE, GL_COMPILE)
    quad = gluNewQuadric()
    glTranslate(0.0, -0.5, 0.0)
    glRotatef(90.0, -1.0, 0.0, 0.0)
    gluCylinder(quad, 0.0, 0.5, 1, 20, 30)
    gluDeleteQuadric(quad)
    glEndList()

def init_primitives():
    make_plane()
    make_sphere()
    make_cube()
    make_squarePyramid()
    make_trianglePyramid()
    make_cylinder()
    make_cone()
    make_reverseCone()
