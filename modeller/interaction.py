import sys
import math
from collections import defaultdict

from OpenGLContext import quaternion
from OpenGLContext.move import trackball
from OpenGLContext.arrays import *

from OpenGL.GLUT import *
from OpenGL.GL import *
from OpenGL.GLU import *


class Interaction(object):

    def __init__(self):
        """ Handles user interaction """
        # currently pressed mouse button
        self.pressed = None
        # current rotation, stored as quaternion
        self.rotation = quaternion.fromEuler()
        self.camera_loc = array( [0, 0, 0, 0], 'd')
        # the trackball to calculate rotation
        self.trackball = None
        # the current mouse location
        self.mouse_loc = None
        # Unsophisticated callback mechanism
        self.callbacks = defaultdict(list)

    def registerCallback(self, name, func):
        """ registers a callback for a certain event """
        self.callbacks[name].append(func)

    def trigger(self, name, *args, **kwargs):
        """ calls a callback, forwards the args """
        for func in self.callbacks[name]:
            func(*args, **kwargs)

    def translate(self, x, y, z):
        """ translate the camera """
        self.camera_loc[0] += x
        self.camera_loc[1] += y
        self.camera_loc[2] += z

    def MouseButton(self, button, mode, x, y):
        """ Called when the mouse button is pressed or released """
        xSize, ySize = glutGet( GLUT_WINDOW_WIDTH ), glutGet( GLUT_WINDOW_HEIGHT )
        y = ySize - y # invert the y coordinate because OpenGL is inverted
        self.mouse_loc = (x, y)

        if mode == GLUT_DOWN:
            self.pressed = button
            if button == GLUT_RIGHT_BUTTON:
                self.trackball = trackball.Trackball(self.camera_loc, self.rotation, (0,0,0), x, y, xSize, ySize)
            elif button == GLUT_LEFT_BUTTON: # picking
                self.trigger('picking', x, y)
            elif button == 3: # scroll up
                self.translate(0, 0, -1.0)
            elif button == 4: # scroll up
                self.translate(0, 0, 1.0)
        else: # mouse button release
            self.trackball = None
            self.pressed = None
        glutPostRedisplay()

    def MouseMove(self, x, screen_y):
       """ Called when the mouse is moved """
       xSize, ySize = glutGet( GLUT_WINDOW_WIDTH ), glutGet( GLUT_WINDOW_HEIGHT )
       y = ySize - screen_y # invert the y coordinate because OpenGL is inverted
       if self.pressed is not None:
           if self.pressed == GLUT_RIGHT_BUTTON and self.trackball is not None:
               # ignore the updated camera loc because we want to always rotate around the origin
               _, self.rotation = self.trackball.update(x, y)
           elif self.pressed == GLUT_LEFT_BUTTON:
               self.trigger('move', x, y)
           elif self.pressed == GLUT_MIDDLE_BUTTON:
               dx = self.mouse_loc[0] - x
               dy = self.mouse_loc[1] - y
               self.translate(dx/60.0, dy/60.0, 0)
           else:
               pass
           glutPostRedisplay()
       self.mouse_loc = (x, y)

    def Keyboard(self, key, x, screen_y):
        """ Called on keyboard input from the user """
        xSize, ySize = glutGet( GLUT_WINDOW_WIDTH ), glutGet( GLUT_WINDOW_HEIGHT )
        y = ySize - screen_y
        if key == 's':
            self.trigger('place', 'sphere', x, y)
        elif key == 'c':
            self.trigger('place', 'cube', x, y)
        elif key == GLUT_KEY_UP:
            self.trigger('scale', up=True)
        elif key == GLUT_KEY_DOWN:
            self.trigger('scale', up=False)
        elif key == GLUT_KEY_LEFT:
            self.trigger('color', forward=True)
        elif key == GLUT_KEY_RIGHT:
            self.trigger('color', forward=False)
        glutPostRedisplay()

    def register(self):
        """ register callbacks with glut """
        glutMouseFunc(self.MouseButton)
        glutMotionFunc(self.MouseMove)
        glutKeyboardFunc(self.Keyboard)
        glutSpecialFunc(self.Keyboard)
        glutPassiveMotionFunc(None)
