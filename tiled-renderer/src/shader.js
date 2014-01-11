var ShaderSource = {
    kVertexShaderStr :
        "uniform mat4 u_mvpMatrix; \n" +
        "attribute vec2 a_vPosition; \n" +
        "attribute vec2 a_texCoord; \n" +
        "varying vec2 v_texCoord; \n" +
        "void main() { \n" +
        "   gl_Position = u_mvpMatrix * vec4(a_vPosition, 0.0, 1.0); \n" +
        "   v_texCoord = a_texCoord; \n" +
        "} \n",

    kFragmentShaderStr :
        "precision mediump float; \n" +
        "varying vec2 v_texCoord; \n" +
        "uniform float u_alpha; \n" +
        "uniform sampler2D s_texture; \n" +
        "void main() { \n" +
        "   gl_FragColor = texture2D(s_texture, v_texCoord); \n" +
        "   gl_FragColor *= u_alpha; \n" +
        "} \n",
};

var ShaderHandles = {
    u_mvpMatrix : null,
    a_vPosition : null,
    a_texCoord : null,
    u_alpha : null,
    s_texture : null
};

function Shader(gl, source, type) {
    this.shaderObj = gl.createShader(type);
    gl.shaderSource(this.shaderObj, source);
    gl.compileShader(this.shaderObj);
    if (!gl.getShaderParameter(this.shaderObj, gl.COMPILE_STATUS)) {
        logger.log('Error in shader compile: ' + gl.getShaderInfoLog(this.shaderObj));
        gl.deleteShader(this.shaderObj);
        this.shaderObj = null;
    }
}
