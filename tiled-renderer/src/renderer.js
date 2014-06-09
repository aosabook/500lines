function Rect(x, y, width, height) {
    this.x = x;
    this.y = y;
    this.width = width;
    this.height = height;
    this.contains = function(rect) {
        return this.x <= rect.x && (this.x + this.width) >= (rect.x + rect.width)
            && this.x <= rect.x && (this.x + this.width) >= (rect.x + rect.width);
    };

    this.intersects = function(rect) {
        return this.x < (rect.x + rect.width) && rect.x < (this.x + this.width)
            && this.y < (rect.y + rect.height) && rect.y < (this.y + this.height);
    };
}

function Tile(x, y, width, height, texture) {
    this.rect = new Rect(x, y, width, height);
    this.texture = texture;
    this.zoomFactor = 1;
}

function ImageTexture(gl, src, loadCallback) {
    this.gl = gl;
    this.loadImage = function() {
        this.image = new Image();
        var self = this;
        this.image.onload = function() {
            self.width = this.width;
            self.height = this.height;
            self.createImageTexture(this);
            loadCallback(self);
        };
        this.image.src = src;
    };

    this.createImageTexture = function(image) {
        try {
            this.texture = renderer.createAndSetupTexture();
            this.gl.texImage2D(this.gl.TEXTURE_2D, 0, this.gl.RGBA, this.gl.RGBA, this.gl.UNSIGNED_BYTE, image);
            logger.logGLStatus(this.gl, 'loading image ' + image.src + '...');
        } catch (e) { // Notify user of SecurityErrors need to run with --disable-web-security on Chrome
            renderer.canvas.style.display = 'none';
            logger.logError(e);
            throw e;
        }
    };
    this.loadImage();
}

var renderer = new function() {
    this.TILE_SIZE = 128;

    this.initGL = function() {
        this.canvas = document.getElementById('glCanvas');
        this.gl = this.canvas.getContext('webgl');
        this.viewportRect = new Rect(0, 0, this.canvas.width, this.canvas.height);
        this.gl.clearColor(1.0, 0.0, 0.0, 1.0);
        this.gl.viewport(0, 0, this.canvas.width, this.canvas.height);
        this.gl.enable(this.gl.BLEND);
        this.gl.blendFunc(this.gl.ONE, this.gl.ONE_MINUS_SRC_ALPHA);
        this.initShader();
        this.initProgram();
        this.setupGeometry();
        this.createCheckerboardTexture();
        this.setupCanvasEventListeners();
        this.loadImagesAndDrawScene();
    };

    this.loadImagesAndDrawScene = function() {
        this.tileOutline = new ImageTexture(this.gl, 'images/tile_outline.png', function() {});
        this.backingImage = new ImageTexture(this.gl, 'images/A_Song_of_Ice_and_Fire.jpg', function(imageTexture) {
            this.createTiles(imageTexture);
            requestAnimationFrame(this.drawScene.bind(this));
        }.bind(this));
    };

    this.initShader = function() {
        this.vertexShader = new Shader(this.gl, ShaderSource.kVertexShaderStr, this.gl.VERTEX_SHADER);
        this.fragmentShader = new Shader(this.gl, ShaderSource.kFragmentShaderStr, this.gl.FRAGMENT_SHADER);
    };

    this.initProgram = function() {
        this.program = this.gl.createProgram();
        this.gl.attachShader(this.program, this.vertexShader.shaderObj);
        this.gl.attachShader(this.program, this.fragmentShader.shaderObj);
        this.gl.linkProgram(this.program);
        if (!this.gl.getProgramParameter(this.program, this.gl.LINK_STATUS))
            logger.logError('Error linking program');
        else
            logger.log('Completed linking program');

        this.gl.useProgram(this.program);
        ShaderHandles.u_mvpMatrix = this.gl.getUniformLocation(this.program, 'u_mvpMatrix');
        ShaderHandles.a_vPosition = this.gl.getAttribLocation(this.program, 'a_vPosition');
        ShaderHandles.a_texCoord = this.gl.getAttribLocation(this.program, 'a_texCoord');
        ShaderHandles.s_texture = this.gl.getUniformLocation(this.program, 's_texture');
        logger.logGLStatus(this.gl, 'getting locations of attributes and uniforms');
    };

    this.createAndSetupTexture = function() {
        var texture = this.gl.createTexture();
        this.gl.bindTexture(this.gl.TEXTURE_2D, texture);
        this.gl.pixelStorei(this.gl.UNPACK_FLIP_Y_WEBGL, true);
        this.gl.texParameteri(this.gl.TEXTURE_2D, this.gl.TEXTURE_WRAP_S, this.gl.CLAMP_TO_EDGE);
        this.gl.texParameteri(this.gl.TEXTURE_2D, this.gl.TEXTURE_WRAP_T, this.gl.CLAMP_TO_EDGE);
        this.gl.texParameteri(this.gl.TEXTURE_2D, this.gl.TEXTURE_MIN_FILTER, this.gl.NEAREST);
        this.gl.texParameteri(this.gl.TEXTURE_2D, this.gl.TEXTURE_MAG_FILTER, this.gl.NEAREST);
        return texture;
    };

    this.setupGeometry = function() {
        this.clipspaceBuffer = this.gl.createBuffer();
        this.gl.bindBuffer(this.gl.ARRAY_BUFFER, this.clipspaceBuffer);
        this.gl.bufferData(this.gl.ARRAY_BUFFER,
            new Float32Array([
                -1.0, -1.0, 0.0, 0.0,
                 1.0, -1.0, 1.0, 0.0,
                -1.0,  1.0, 0.0, 1.0,
                 1.0,  1.0, 1.0, 1.0]), this.gl.STATIC_DRAW);
        this.gl.enableVertexAttribArray(ShaderHandles.a_vPosition);
        this.gl.vertexAttribPointer(ShaderHandles.a_vPosition, 2, this.gl.FLOAT, false, 16, 0);
        this.gl.enableVertexAttribArray(ShaderHandles.a_texCoord);
        this.gl.vertexAttribPointer(ShaderHandles.a_texCoord, 2, this.gl.FLOAT, false, 16, 8);
        this.gl.uniform1i(ShaderHandles.s_texture, 0);
        logger.logGLStatus(this.gl, 'uploading vertex, texture buffer data, attributes and uniforms');
    };

    this.drawScene = function(timestamp) {
        requestAnimationFrame(this.drawScene.bind(this));
        if (this.backingImage.texture == null || this.tileOutline.texture == null || !this.needsUpdate) return;
        timer.tick(timestamp);
        this.gl.clear(this.gl.COLOR_BUFFER_BIT);
        this.renderTiles();
    };

    this.createCheckerboardTexture = function() {
        this.checkerboardTexture = this.createAndSetupTexture();
        var colorBuffer = [
            200, 200, 200,
            255, 255, 255,
            255, 255, 255,
            200, 200, 200 ];
        var colorArray = new Uint8Array(colorBuffer);
        this.gl.pixelStorei(this.gl.UNPACK_ALIGNMENT, 1);
        this.gl.texImage2D(this.gl.TEXTURE_2D, 0, this.gl.RGB, 2, 2, 0, this.gl.RGB, this.gl.UNSIGNED_BYTE, colorArray);
    };

    this.setupCanvasEventListeners = function() {
        this.mouseDown = false;
        var mouseUp = function(event) { this.mouseDown = false; }.bind(this);
        this.canvas.addEventListener('mousedown',
            function(event) {
                this.mouseDown = true;
                this.mousePressRect = new Rect(
                    event.offsetX == undefined ? event.layerX : event.offsetX,
                    event.offsetY == undefined ? event.layerY : event.offsetY,
                    this.viewportRect.width, this.viewportRect.height);
            }.bind(this));
        this.canvas.addEventListener('mouseup', mouseUp);
        this.canvas.addEventListener('mouseout', mouseUp);
        this.canvas.addEventListener('mousemove',
            function(event) {
                if (!this.mouseDown) return;
                var offsetX = event.offsetX == undefined ? event.layerX : event.offsetX;
                var offsetY = event.offsetY == undefined ? event.layerY : event.offsetY;
                this.viewportRect.x -= (offsetX - this.mousePressRect.x);
                this.viewportRect.x = Math.max(0, Math.min(this.viewportRect.x, this.backingImage.width - this.viewportRect.width));
                this.viewportRect.y -= (offsetY - this.mousePressRect.y);
                this.viewportRect.y = Math.max(0, Math.min(this.viewportRect.y, this.backingImage.height - this.viewportRect.height));
                this.mousePressRect.x = offsetX;
                this.mousePressRect.y = offsetY;
                this.needsUpdate = true;
            }.bind(this));
        this.canvas.addEventListener('dblclick',
            function(event) {
                // TODO
            }.bind(this));
    };

    this.drawQuad = function(texture, x, y, width, height, viewportWidth, viewportHeight) {
        x += width / 2; // The center of the quad needs to move from (0,0)
        y += height / 2;

        if (this.mvpMatrix == null)
            this.mvpMatrix = mat3.create();
        else
            mat3.identity(this.mvpMatrix);

        var scaleX = width / viewportWidth;
        var scaleY = height / viewportHeight;
        var translateX = (2 * x / viewportWidth) - 1; // Map from [0, viewportWidth] to [-1, 1]
        var translateY = (2 * y / viewportHeight) - 1; // Map from [0, viewportHeight] to [-1, 1]
        mat3.translate(this.mvpMatrix, this.mvpMatrix, vec2.fromValues(translateX, -translateY)); // top left coordinates
        mat3.scale(this.mvpMatrix, this.mvpMatrix, vec2.fromValues(scaleX, scaleY));
        this.gl.uniformMatrix3fv(ShaderHandles.u_mvpMatrix, this.gl.FALSE, this.mvpMatrix);
        this.gl.bindTexture(this.gl.TEXTURE_2D, texture);
        this.gl.drawArrays(this.gl.TRIANGLE_STRIP, 0, 4);
    };

    this.createTiles = function(imageTexture) {
        this.backingStore = [];
        var rows = Math.ceil(imageTexture.height / this.TILE_SIZE);
        var columns = Math.ceil(imageTexture.width / this.TILE_SIZE);
        this.gl.viewport(0, 0, this.TILE_SIZE, this.TILE_SIZE);
        for (var i = 0; i < rows; ++i)
            for (var j = 0; j < columns; ++j)
                this.createTile(imageTexture, i, j, columns);

        logger.logGLStatus(this.gl,'creating ' + rows * columns + ' ' + this.TILE_SIZE + 'x' + this.TILE_SIZE
            + ' tiles for ' + imageTexture.width + 'x' + imageTexture.height + ' image');
        this.gl.bindFramebuffer(this.gl.FRAMEBUFFER, null);
        this.gl.viewport(0, 0, this.canvas.width, this.canvas.height);
        this.setDefaultTextureCoordinates();
        this.needsUpdate = true;
    };

    this.createTile = function(imageTexture, i, j, stride) {
        var texture = this.createAndSetupTexture();
        this.backingStore[i * stride + j] = new Tile(j * this.TILE_SIZE, i * this.TILE_SIZE,
            this.TILE_SIZE, this.TILE_SIZE, texture);
        this.attachTextureToFBO(texture);
        this.setupTextureCropCoordinates(j * this.TILE_SIZE, i * this.TILE_SIZE,
            this.TILE_SIZE, this.TILE_SIZE, imageTexture.width, imageTexture.height);
        this.drawQuad(imageTexture.texture, 0, 0, this.TILE_SIZE,
            this.TILE_SIZE, this.TILE_SIZE, this.TILE_SIZE);
    };

    this.setDefaultTextureCoordinates = function() {
        if (this.clipspaceBuffer == null)
            logger.logError('Error: Geometry has not been set up');

        this.gl.bindBuffer(this.gl.ARRAY_BUFFER, this.clipspaceBuffer);
        this.gl.enableVertexAttribArray(ShaderHandles.a_texCoord);
        this.gl.vertexAttribPointer(ShaderHandles.a_texCoord, 2, this.gl.FLOAT, false, 16, 8);
    };

    this.setupTextureCropCoordinates = function(x, y, cropWidth, cropHeight, viewportWidth, viewportHeight) {
        if (this.textureCropBuffer == null)
            this.textureCropBuffer = this.gl.createBuffer();

        this.gl.bindBuffer(this.gl.ARRAY_BUFFER, this.textureCropBuffer);
        var scaledWidth = cropWidth / viewportWidth;
        var scaledHeight = cropHeight / viewportHeight;
        var offsetX = x / viewportWidth;
        var offsetY = 1 - y / viewportHeight - scaledHeight; // First tile should be top left of the input texture.
        this.gl.bufferData(this.gl.ARRAY_BUFFER,
            new Float32Array([
                offsetX, offsetY,
                offsetX + scaledWidth, offsetY,
                offsetX, offsetY + scaledHeight,
                offsetX + scaledWidth, offsetY + scaledHeight]), this.gl.STATIC_DRAW);
        this.gl.enableVertexAttribArray(ShaderHandles.a_texCoord);
        this.gl.vertexAttribPointer(ShaderHandles.a_texCoord, 2, this.gl.FLOAT, false, 0, 0);
    };

    this.attachTextureToFBO = function(texture) {
        if (this.fbo == null)
            this.fbo = this.gl.createFramebuffer();

        this.gl.bindTexture(this.gl.TEXTURE_2D, texture);
        this.gl.texImage2D(this.gl.TEXTURE_2D, 0, this.gl.RGBA, this.TILE_SIZE,
            this.TILE_SIZE, 0, this.gl.RGBA, this.gl.UNSIGNED_BYTE, null);

        this.gl.bindFramebuffer(this.gl.FRAMEBUFFER, this.fbo);
        this.gl.framebufferTexture2D(this.gl.FRAMEBUFFER, this.gl.COLOR_ATTACHMENT0, this.gl.TEXTURE_2D, texture, 0);
        if (this.gl.checkFramebufferStatus(this.gl.FRAMEBUFFER) != this.gl.FRAMEBUFFER_COMPLETE) {
            logger.logError('Error: FBO is not complete');
            this.gl.deleteFramebuffer(this.fbo);
        }
        this.gl.clear(this.gl.COLOR_BUFFER_BIT);
    };

    this.renderTiles = function() {
        var rows = Math.ceil(this.backingImage.height / this.TILE_SIZE);
        var columns = Math.ceil(this.backingImage.width / this.TILE_SIZE);
        this.tileCount = 0;
        for (var i = 0; i < rows; ++i)
            for (var j = 0; j < columns; ++j)
                this.renderTile(i, j, columns);

        logger.logDynamicLine('tileCount', 'Rendered ' + this.tileCount + ' tiles for frame #'
            + timer.totalFrames + ' at ' + parseInt(timer.fps()) + ' fps');
        this.needsUpdate = false;
    };

    this.renderTile = function(i, j, stride) {
        var tile = this.backingStore[i * stride + j];
        if (!this.viewportRect.intersects(tile.rect)) return;
        this.drawQuad(tile.texture, tile.rect.x - this.viewportRect.x, tile.rect.y - this.viewportRect.y,
                tile.rect.width, tile.rect.height, this.canvas.width, this.canvas.height);
        this.drawQuad(this.tileOutline.texture, tile.rect.x - this.viewportRect.x, tile.rect.y - this.viewportRect.y,
                tile.rect.width, tile.rect.height, this.canvas.width, this.canvas.height);
        ++this.tileCount;
    };
};
