#! /usr/bin/env python

from OpenGLContext import testingcontext
from OpenGLContext.arrays import *
from OpenGLContext.events.timer import Timer
BaseContext = testingcontext.getInteractive()


from OpenGL.GL import *
from OpenGL.GL import shaders
from OpenGL.arrays import vbo

class TestContext(BaseContext):
    """ creates a simple vertex shader """

    def OnInit(self):
        VERTEX_SHADER = shaders.compileShader("""
        uniform float tween;
        attribute vec3 position;
        attribute vec3 tweened;
        attribute vec3 color;
        varying vec4 baseColor;
        void main(){
            gl_Position = gl_ModelViewProjectionMatrix * mix(
                vec4(position, 1.0),
                vec4(tweened, 1.0),
                tween);
            baseColor = vec4(color, 1.0);
        }""", GL_VERTEX_SHADER)

        FRAGMENT_SHADER = shaders.compileShader("""
        varying vec4 baseColor;
        void main(){
            gl_FragColor = baseColor;
        }""", GL_FRAGMENT_SHADER)

        self.shader = shaders.compileProgram(VERTEX_SHADER, FRAGMENT_SHADER)


        self.vbo = vbo.VBO(array([
                                [ 0,  1, 0,  1, 3, 0,  0,1,0],
                                [-1, -1, 0, -1,-1, 0,  1, 1,0],
                                [ 1, -1, 0,  1,-1, 0,  0,1,1],
                                [ 2, -1, 0,  2,-1, 0,  1,0,0],
                                [ 4, -1, 0,  4,-1, 0,  0,1,0],
                                [ 4,  1, 0,  4, 9, 0,  0,0,1],
                                [ 2, -1, 0,  2,-1, 0,  1,0,0],
                                [ 4,  1, 0,  1, 3, 0,  0,0,1],
                                [ 2,  1, 0,  1,-1, 0,  0,1,1],
                                ], 'f'))

        self.position_location = glGetAttribLocation(self.shader, 'position')
        self.tweened_location = glGetAttribLocation(self.shader, 'tweened')
        self.color_location = glGetAttribLocation(self.shader, 'color')
        self.tween_location = glGetUniformLocation(self.shader, 'tween')

        self.time = Timer(duration=2.0, repeating=1)
        self.time.addEventHandler("fraction", self.OnTimerFraction)
        self.time.register(self)
        self.time.start()
        self.tween_fraction = 0.0

    def Render(self, mode):
        """ Render geometry """
        BaseContext.Render(self, mode)
        glUseProgram(self.shader)
        glUniform1f(self.tween_location, self.tween_fraction)
        try:
            self.vbo.bind()
            try:
                glEnableVertexAttribArray(self.position_location)
                glEnableVertexAttribArray(self.tweened_location)
                glEnableVertexAttribArray(self.color_location)

                stride = 9*4
                glVertexAttribPointer( self.position_location, 3, GL_FLOAT, False, stride, self.vbo)
                glVertexAttribPointer( self.tweened_location, 3, GL_FLOAT, False, stride, self.vbo+12)
                glVertexAttribPointer( self.color_location, 3, GL_FLOAT, False, stride, self.vbo+24)

                glDrawArrays(GL_TRIANGLES, 0, 9)

            finally:
                self.vbo.unbind()
                glDisableClientState(GL_VERTEX_ARRAY)
                glDisableVertexAttribArray(self.position_location)
                glDisableVertexAttribArray(self.tweened_location)
                glDisableVertexAttribArray(self.color_location)
        finally:
            shaders.glUseProgram(0)

    def OnTimerFraction(self, event):
        frac = event.fraction()
        if frac > 0.5:
            frac = 1.0 - frac
        frac *= 2
        self.tween_fraction = frac
        self.triggerRedraw()

if __name__=="__main__":
    TestContext.ContextMainLoop()
