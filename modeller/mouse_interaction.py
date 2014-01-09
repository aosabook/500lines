import sys
import math

from OpenGLContext import quaternion
from OpenGLContext.move import trackball
from OpenGLContext.arrays import *

try:
  from OpenGL.GLUT import *
  from OpenGL.GL import *
  from OpenGL.GLU import *
except:
  print ''' Error: PyOpenGL not installed properly !!'''
  sys.exit(  )


class MouseInteraction(object):

    def __init__(self):
        self.pressed = None
        self.rotation = quaternion.fromEuler()
        self.camera_loc = array( [0, 0, -15, 0], 'd')
        self.trackball = None
        self.mouse_loc = None


    def MouseButton(self, button, mode, x, y):
        xSize, ySize = glutGet( GLUT_WINDOW_WIDTH ), glutGet( GLUT_WINDOW_HEIGHT )
        self.mouse_loc = (x, y)
        if mode == GLUT_DOWN:
            print "DOWN" + str(button)
            self.pressed = button
            if button == GLUT_RIGHT_BUTTON:
                self.trackball = trackball.Trackball(self.camera_loc, self.rotation, (0,0,0), x, y, xSize, ySize)
            elif button == 3: # scroll up
                self.translate(0, 0, 1.0)
                glutPostRedisplay()
            elif button == 4: # scroll up
                self.translate(0, 0, -1.0)
                glutPostRedisplay()


        else:
            print "UP" + str(button)
            self.trackball = None
            self.pressed = None
        glutPostRedisplay()

    def translate(self, x, y, z):
       print x, y, z
       self.camera_loc[0] += x
       self.camera_loc[1] += y
       self.camera_loc[2] += z


    def MouseMove(self, x, y):
        xSize, ySize = glutGet( GLUT_WINDOW_WIDTH ), glutGet( GLUT_WINDOW_HEIGHT )
        y = ySize - y
        if self.pressed is not None:
            if self.pressed == GLUT_RIGHT_BUTTON and self.trackball is not None:
                _, self.rotation = self.trackball.update(x, y)
            elif self.pressed == GLUT_LEFT_BUTTON:
                dx = x - self.mouse_loc[0]
                dy = y - self.mouse_loc[1]
                self.translate(dx/60.0, dy/60.0, 0)
            elif self.pressed == GLUT_MIDDLE_BUTTON:
                dz = y - self.mouse_loc[1]
                self.translate(0, 0, dz/40.0)
            else:
                pass
            self.mouse_loc = (x, y)
            glutPostRedisplay()


    def register(self):
        glutMouseFunc(self.MouseButton)
        glutMotionFunc(self.MouseMove)
        glutPassiveMotionFunc(None)
