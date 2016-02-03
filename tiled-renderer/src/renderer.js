'use strict';

function Rect(x, y, width, height) {
    this.x = x;
    this.y = y;
    this.width = width;
    this.height = height;
}

Rect.prototype.intersects = function(rect) {
    return this.x < (rect.x + rect.width) && rect.x < (this.x + this.width) &&
        this.y < (rect.y + rect.height) && rect.y < (this.y + this.height);
};

function Tile(x, y, width, height, texture) {
    this.rect = new Rect(x, y, width, height);
    this.texture = texture;
}

function ImageTexture(gl, src, loadCallback) {
    this.gl = gl;
    this.loadImage(src, loadCallback);
}

ImageTexture.prototype.loadImage = function(src, loadCallback) {
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

ImageTexture.prototype.createImageTexture = function(image) {
    try {
        this.texture = renderer.createAndSetupTexture();
        this.gl.texImage2D(this.gl.TEXTURE_2D, 0, this.gl.RGBA, this.gl.RGBA, this.gl.UNSIGNED_BYTE, image);
        logger.logGLStatus(this.gl, 'loading image ' + image.src + '...');
    } catch (e) { // Notify user of SecurityErrors need to run with --disable-web-security on Chrome
        renderer.hideCanvas();
        logger.logError(e);
        throw e;
    }
};

var renderer = (function() {
    var instance = {};
    var kTileSize = 128;

    instance.initGL = function() {
        this.canvas = document.getElementById('glCanvas');
        this.gl = this.canvas.getContext('webgl');
        this.viewportRect = new Rect(0, 0, this.canvas.width, this.canvas.height);
        this.gl.clearColor(1.0, 0.0, 0.0, 1.0);
        this.gl.enable(this.gl.BLEND);
        this.gl.blendFunc(this.gl.ONE, this.gl.ONE_MINUS_SRC_ALPHA);
        this.initShader();
        this.initProgram();
        this.setupGeometry();
        this.setupCanvasEventListeners();
        this.loadImagesAndDrawScene();
    };

    instance.hideCanvas = function() { this.canvas.style.display = 'none'; };

    instance.loadImagesAndDrawScene = function() {
        this.tileOutline = new ImageTexture(this.gl, 'images/Tile_Outline.png', function() {});
        this.backingImage = new ImageTexture(this.gl, 'images/A_Song_of_Ice_and_Fire.jpg', function(imageTexture) {
            this.createTiles(imageTexture);
            requestAnimationFrame(this.drawScene.bind(this));
        }.bind(this));
    };

    instance.initShader = function() {
        this.vertexShader = new Shader(this.gl, ShaderSource.kVertexShaderStr, this.gl.VERTEX_SHADER);
        this.fragmentShader = new Shader(this.gl, ShaderSource.kFragmentShaderStr, this.gl.FRAGMENT_SHADER);
    };

    instance.initProgram = function() {
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

    instance.createAndSetupTexture = function() {
        var texture = this.gl.createTexture();
        this.gl.bindTexture(this.gl.TEXTURE_2D, texture);
        this.gl.pixelStorei(this.gl.UNPACK_FLIP_Y_WEBGL, true);
        this.gl.texParameteri(this.gl.TEXTURE_2D, this.gl.TEXTURE_WRAP_S, this.gl.CLAMP_TO_EDGE);
        this.gl.texParameteri(this.gl.TEXTURE_2D, this.gl.TEXTURE_WRAP_T, this.gl.CLAMP_TO_EDGE);
        this.gl.texParameteri(this.gl.TEXTURE_2D, this.gl.TEXTURE_MIN_FILTER, this.gl.NEAREST);
        this.gl.texParameteri(this.gl.TEXTURE_2D, this.gl.TEXTURE_MAG_FILTER, this.gl.NEAREST);
        return texture;
    };

    instance.setupGeometry = function() {
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

    instance.drawScene = function(timestamp) {
        requestAnimationFrame(this.drawScene.bind(this));
        if (!this.backingImage.texture || !this.tileOutline.texture || !this.needsUpdate) return;
        timer.tick(timestamp);
        this.gl.clear(this.gl.COLOR_BUFFER_BIT);
        this.renderTiles();
    };

    instance.setupCanvasEventListeners = function() {
        this.mouseDown = false;
        var mouseUp = function(event) { this.mouseDown = false; }.bind(this);
        this.canvas.addEventListener('mousedown',
            function(event) {
                this.mouseDown = true;
                this.mousePressRect = new Rect(
                    event.offsetX === undefined ? event.layerX : event.offsetX,
                    event.offsetY === undefined ? event.layerY : event.offsetY,
                    this.viewportRect.width, this.viewportRect.height);
            }.bind(this));
        this.canvas.addEventListener('mouseup', mouseUp);
        this.canvas.addEventListener('mouseout', mouseUp);
        this.canvas.addEventListener('mousemove',
            function(event) {
                if (!this.mouseDown) return;
                var offsetX = event.offsetX === undefined ? event.layerX : event.offsetX;
                var offsetY = event.offsetY === undefined ? event.layerY : event.offsetY;
                this.viewportRect.x -= (offsetX - this.mousePressRect.x);
                this.viewportRect.x = Math.max(0, Math.min(this.viewportRect.x, this.backingImage.width - this.viewportRect.width));
                this.viewportRect.y -= (offsetY - this.mousePressRect.y);
                this.viewportRect.y = Math.max(0, Math.min(this.viewportRect.y, this.backingImage.height - this.viewportRect.height));
                this.mousePressRect.x = offsetX;
                this.mousePressRect.y = offsetY;
                this.needsUpdate = true;
            }.bind(this));
    };

    instance.drawQuad = function(texture, x, y, width, height, viewportWidth, viewportHeight) {
        x += width / 2; // The center of the quad needs to move from (0,0)
        y += height / 2;

        if (!this.mvpMatrix)
            this.mvpMatrix = new Matrix3();
        else
            this.mvpMatrix.identity();

        var scaleX = width / viewportWidth;
        var scaleY = height / viewportHeight;
        var translateX = (2 * x / viewportWidth) - 1; // Map from [0, viewportWidth] to [-1, 1]
        var translateY = (2 * y / viewportHeight) - 1; // Map from [0, viewportHeight] to [-1, 1]
        this.mvpMatrix.translate(translateX, -translateY); // top left coordinates
        this.mvpMatrix.scale(scaleX, scaleY);
        this.gl.uniformMatrix3fv(ShaderHandles.u_mvpMatrix, this.gl.FALSE, this.mvpMatrix.values);
        this.gl.bindTexture(this.gl.TEXTURE_2D, texture);
        this.gl.drawArrays(this.gl.TRIANGLE_STRIP, 0, 4);
    };

    instance.createTiles = function(imageTexture) {
        this.backingStore = [];
        var rows = Math.ceil(imageTexture.height / kTileSize);
        var columns = Math.ceil(imageTexture.width / kTileSize);
        this.gl.viewport(0, 0, kTileSize, kTileSize);
        for (var i = 0; i < rows; ++i) {
            for (var j = 0; j < columns; ++j)
                this.createTile(imageTexture, i, j, columns);
        }

        logger.logGLStatus(this.gl,'creating ' + rows * columns + ' ' + kTileSize + 'x' + kTileSize +
                ' tiles for ' + imageTexture.width + 'x' + imageTexture.height + ' image');
        this.gl.bindFramebuffer(this.gl.FRAMEBUFFER, null);
        this.gl.viewport(0, 0, this.canvas.width, this.canvas.height);
        this.setDefaultTextureCoordinates();
        this.needsUpdate = true;
    };

    instance.createTile = function(imageTexture, i, j, stride) {
        var texture = this.createAndSetupTexture();
        this.backingStore[i * stride + j] = new Tile(j * kTileSize, i * kTileSize,
            kTileSize, kTileSize, texture);
        this.attachTextureToFBO(texture);
        this.setupTextureCropCoordinates(j * kTileSize, i * kTileSize,
            kTileSize, kTileSize, imageTexture.width, imageTexture.height);
        this.drawQuad(imageTexture.texture, 0, 0, kTileSize,
            kTileSize, kTileSize, kTileSize);
    };

    instance.setDefaultTextureCoordinates = function() {
        if (!this.clipspaceBuffer)
            logger.logError('Error: Geometry has not been set up');

        this.gl.bindBuffer(this.gl.ARRAY_BUFFER, this.clipspaceBuffer);
        this.gl.enableVertexAttribArray(ShaderHandles.a_texCoord);
        this.gl.vertexAttribPointer(ShaderHandles.a_texCoord, 2, this.gl.FLOAT, false, 16, 8);
    };

    instance.setupTextureCropCoordinates = function(x, y, cropWidth, cropHeight, imageWidth, imageHeight) {
        if (!this.textureCropBuffer)
            this.textureCropBuffer = this.gl.createBuffer();

        this.gl.bindBuffer(this.gl.ARRAY_BUFFER, this.textureCropBuffer);
        var scaledCropWidth = cropWidth / imageWidth;
        var scaledCropHeight = cropHeight / imageHeight;
        var offsetX = x / imageWidth;
        var offsetY = 1 - y / imageHeight - scaledCropHeight; // First tile should be top left of the input texture.
        this.gl.bufferData(this.gl.ARRAY_BUFFER,
            new Float32Array([
                offsetX, offsetY,
                offsetX + scaledCropWidth, offsetY,
                offsetX, offsetY + scaledCropHeight,
                offsetX + scaledCropWidth, offsetY + scaledCropHeight]), this.gl.STATIC_DRAW);
        this.gl.enableVertexAttribArray(ShaderHandles.a_texCoord);
        this.gl.vertexAttribPointer(ShaderHandles.a_texCoord, 2, this.gl.FLOAT, false, 0, 0);
    };

    instance.attachTextureToFBO = function(texture) {
        if (!this.fbo)
            this.fbo = this.gl.createFramebuffer();

        this.gl.bindTexture(this.gl.TEXTURE_2D, texture);
        this.gl.texImage2D(this.gl.TEXTURE_2D, 0, this.gl.RGBA, kTileSize,
            kTileSize, 0, this.gl.RGBA, this.gl.UNSIGNED_BYTE, null);

        this.gl.bindFramebuffer(this.gl.FRAMEBUFFER, this.fbo);
        this.gl.framebufferTexture2D(this.gl.FRAMEBUFFER, this.gl.COLOR_ATTACHMENT0, this.gl.TEXTURE_2D, texture, 0);
        if (this.gl.checkFramebufferStatus(this.gl.FRAMEBUFFER) != this.gl.FRAMEBUFFER_COMPLETE) {
            logger.logError('Error: FBO is not complete');
            this.gl.deleteFramebuffer(this.fbo);
        }
        this.gl.clear(this.gl.COLOR_BUFFER_BIT);
    };

    instance.renderTiles = function() {
        var rows = Math.ceil(this.backingImage.height / kTileSize);
        var columns = Math.ceil(this.backingImage.width / kTileSize);
        this.tileCount = 0;
        for (var i = 0; i < rows; ++i) {
            for (var j = 0; j < columns; ++j)
                this.renderTile(i, j, columns);
        }
        logger.logDynamicLine('tileCount', 'Rendered ' + this.tileCount + ' tiles for frame #' +
                timer.totalFrames + ' at ' + parseInt(timer.fps(), 10) + ' fps');
        this.needsUpdate = false;
    };

    instance.renderTile = function(i, j, stride) {
        var tile = this.backingStore[i * stride + j];
        if (!this.viewportRect.intersects(tile.rect)) return;

        this.drawQuad(tile.texture, tile.rect.x - this.viewportRect.x, tile.rect.y - this.viewportRect.y,
                tile.rect.width, tile.rect.height, this.canvas.width, this.canvas.height);
        this.drawQuad(this.tileOutline.texture, tile.rect.x - this.viewportRect.x, tile.rect.y - this.viewportRect.y,
                tile.rect.width, tile.rect.height, this.canvas.width, this.canvas.height);
        ++this.tileCount;
    };

    return instance;
})();
