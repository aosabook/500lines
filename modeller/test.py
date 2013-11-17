#! /usr/bin/env python

from OpenGLContext import testingcontext
BaseContext = testingcontext.getInteractive()


from OpenGL.GL import *
from OpenGL.arrays import vbo
from OpenGLContext.arrays import *

from OpenGL.GL import shaders

class TestContext(BaseContext):
    """ creates a simple vertex shader """

    def OnInit(self):
        VERTEX_SHADER = shaders.compileShader("""
        varying vec4 vertex_color;
        void main(){
            gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
            vertex_color = gl_Color;
        }""", GL_VERTEX_SHADER)

        FRAGMENT_SHADER = shaders.compileShader("""
        varying vec4 vertex_color;
        void main(){
            gl_FragColor = vertex_color;
        }""", GL_FRAGMENT_SHADER)

        self.shader = shaders.compileProgram(VERTEX_SHADER, FRAGMENT_SHADER)


        self.vbo = vbo.VBO(array([
                                [ 0,  1, 0,  0,1,0],
                                [-1, -1, 0,  1,1,0],
                                [ 1, -1, 0,  0,1,1],
                                [ 2, -1, 0,  1,0,0],
                                [ 4, -1, 0,  0,1,0],
                                [ 4,  1, 0,  0,0,1],
                                [ 2, -1, 0,  1,0,0],
                                [ 4,  1, 0,  0,0,1],
                                [ 2,  1, 0,  0,1,1],
                                ], 'f'))


    def Render(self, mode):
        """ Render geometry """
        shaders.glUseProgram(self.shader)
        try:
            self.vbo.bind()
            try:
                glEnableClientState(GL_VERTEX_ARRAY)
                glEnableClientState(GL_COLOR_ARRAY)

                glVertexPointer(3, GL_FLOAT, 24, self.vbo)
                glColorPointer(3, GL_FLOAT, 24, self.vbo+12)


                glDrawArrays(GL_TRIANGLES, 0, 9)

            finally:
                self.vbo.unbind()
                glDisableClientState(GL_VERTEX_ARRAY)
        finally:
            shaders.glUseProgram(0)

if __name__=="__main__":
    TestContext.ContextMainLoop()
