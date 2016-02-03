'use strict';

var ShaderSource = {
    kVertexShaderStr :
        "uniform mat3 u_mvpMatrix; \n" +
        "attribute vec2 a_vPosition; \n" +
        "attribute vec2 a_texCoord; \n" +
        "varying vec2 v_texCoord; \n" +
        "void main() { \n" +
        "   gl_Position = vec4(u_mvpMatrix * vec3(a_vPosition, 1), 1); \n" +
        "   v_texCoord = a_texCoord; \n" +
        "} \n",

    kFragmentShaderStr :
        "precision mediump float; \n" +
        "varying vec2 v_texCoord; \n" +
        "uniform sampler2D s_texture; \n" +
        "void main() { \n" +
        "   gl_FragColor = texture2D(s_texture, v_texCoord); \n" +
        "} \n"
};

var ShaderHandles = {
    u_mvpMatrix : null,
    a_vPosition : null,
    a_texCoord : null,
    s_texture : null
};

function Shader(gl, source, type) {
    this.shaderObj = gl.createShader(type);
    gl.shaderSource(this.shaderObj, source);
    gl.compileShader(this.shaderObj);
    if (!gl.getShaderParameter(this.shaderObj, gl.COMPILE_STATUS)) {
        logger.logError('Error in shader compile: ' + gl.getShaderInfoLog(this.shaderObj));
        gl.deleteShader(this.shaderObj);
        this.shaderObj = null;
    }
}
