(function(){
    'use strict';

    var PIXEL_RATIO = window.devicePixelRatio || 1;
    var canvasPlaceholder = document.querySelector('.canvas-placeholder');
    var canvas = document.querySelector('.canvas');
    var script = document.querySelector('.script');
    var ctx = canvas.getContext('2d');
    var cos = Math.cos, sin = Math.sin, atan2 = Math.atan2, sqrt = Math.sqrt, floor = Math.floor, PI = Math.PI;
    var DEGREE = PI / 180;
    var WIDTH, HEIGHT, position, direction, visible, pen, color, center;

    // Tiny vector library

    // Create a vector from an angle in degrees and a magnitude (length)
    function vector(degrees, magnitude){
        var radians = deg2rad(degrees);
        return {
            x: cos(radians) * magnitude,
            y: sin(radians) * magnitude,
            mag: magnitude,
            rad: radians
        };
    }

    function vectorAtPoint(x,y){
        return {
            x: x,
            y: y,
            mag: sqrt(x*x + y*y),
            rad: atan2(y,x)
        };
    }

    // convert degrees to radians
    function deg2rad(deg){
        return deg * DEGREE;
    }

    // replace JavaScript % operator because of sign conversion
    function mod(a,b){
        return a - floor(a/b) * b
    }

    // angle between two vectors in radians
    function angle(v1, v2){
        var diff = v1.rad - v2.rad;
        return atan2(sin(diff), cos(diff));
    }

    // add two vectors together to get a new vector
    function addv(v1, v2){
        var x = v1.x + v2.x;
        var y = v1.y + v2.y;
        var rad = atan2(y,x);
        return {
            x: x,
            y: y,
            mag: sqrt(x*x + y*y),
            rad: rad
        };
    }

    // subtract one vector from another, giving a new vector
    function subv(v1, v2){
        return addv(v1, multv(v2, -1));
    }

    // multiply a vector by a scalar, giving a new vector
    function multv(vec, scal){
        var mag = vec.mag * scal;
        return {
            x: cos(vec.rad) * mag,
            y: sin(vec.rad) * mag,
            mag: mag,
            rad: vec.rad
        };
    }

    // Make magnitude equal to 1
    function normalizev(vec){
        if (vec.mag !== 0){
            return mult(vec, 1 / vec.mag);
        }
        return vec;
    }

    function rotatev(vec, deg){
        var rad = vec.rad + deg2rad(deg);
        return {
            x: cos(rad) * vec.mag,
            y: sin(rad) * vec.mag,
            mag: vec.mag,
            rad: rad
        };
    }

    // dot product of two vectors, giving a scalar
    function dotv(v1, v2){
        return v1.mag * v2.mag * cos(v1.rad - v2.rad);
    }

    // cross product of two vectors, giving a new vector
    // only relevant for 3D vectors
    // function crossv(v1, v2){
    //     var mag = v1.mag * v2.mag * sin(v1.rad - v2.rad);
    // }

    // Tiny Sprite library

    function Sprite(color){
        this.color = color;
        this.position = vector(0,0);
        this.facing = vector(-PI/2,0.1);
        this.velocity = vector(0,0.1);
    }

    Sprite.prototype.accelerate = function(speed){
        this.velocity = addv(this.velocity, multv(this.facing, speed));
    }

    Sprite.prototype.applyForce = function(vec){
        this.velocity = addv(this.velocity, vec);
    }

    Sprite.prototype.rotate = function(r){
        this.facing = rotatev(this.facing, r);
    }

    Sprite.prototype.move = function(){
        this.position = addv(this.position, this.velocity);
    }

    Sprite.prototype.draw = function(){
        ctx.beginPath();
        ctx.lineStyle = this.color;
        ctx.lineWidth = 3;
        ctx.moveTo(this.position.x, this.position.y);
        ctx.lineTo(cos(this.facing - PI/8) * 30 + this.position.x,
                   sin(this.facing - PI/8) * 30 + this.position.y);
        ctx.moveTo(this.position.x, this.position.y);
        ctx.lineTo(cos(this.facing + PI/8) * 30 + this.position.x,
                   sin(this.facing + PI/8) * 30 + this.position.y);
        ctx.closePath();
        ctx.stroke();
        ctx.fillRect(this.position.x, this.position.y, 50, 50);
    }

    var keys = {};
    var everyKeyFunction = [];
    function onKeydown(evt){
        console.log(evt.keyCode);
        keys[evt.keyCode] = true;
    }
    function onKeyup(evt){
        keys[evt.keyCode] = false;
    }



    function onResize(evt){
        WIDTH = canvasPlaceholder.getBoundingClientRect().width * PIXEL_RATIO;
        HEIGHT = canvasPlaceholder.getBoundingClientRect().height * PIXEL_RATIO;
        center = vectorAtPoint(WIDTH / 2, HEIGHT / 2);
        canvas.setAttribute('width', WIDTH);
        canvas.setAttribute('height', HEIGHT);
        canvas.style.top = canvasPlaceholder.getBoundingClientRect().top + "px";
        canvas.style.left = canvasPlaceholder.getBoundingClientRect().left + "px";
        canvas.style.width = (WIDTH / PIXEL_RATIO) + "px"
        canvas.style.height = (HEIGHT / PIXEL_RATIO) + "px"
        if (evt){
            Menu.runSoon();
        }
    }

    var sprite1 = new Sprite('#00FF00');

    function setWrap(){ /* set some global for wrapping */ }
    function setBounce(){ /* set some global for bouncing */ }
    function setHardEdges(){ /* set some global for stopping at the edge */ }
    /* Move in the direction the sprite is facing */
    function accelerate(block){ sprite1.accelerate(Block.value(block)); }
    function rotate(block){ sprite1.rotate(Block.value(block)); }
    function moveToX(block){ sprite1.position = vectorAtPoint(Block.value(block), sprite1.position.y); }
    function moveToY(block){ sprite1.position = vectorAtPoint(sprite1.position.x, Block.value(block)); }
    function onKey(block){
        var key = Block.value(block);
        var children = Block.contents(block);
        everyKeyFunction.push([key, function(){
            Block.run(children);
        }]);
    }
    /* move in the direction of current momentum */
    function move(){ sprite1.move(); }
    function draw(){ sprite1.draw(); }
    function eachFrame(block){
        var children = Block.contents(block);
        everyFrameFunction.push(function(){
            Block.run(children);
        });
    }

    var everyFrameFunction = [];
    function everyFrame(){
        clear();
        for (var i = 0; i < everyKeyFunction.length; i++){
            var key = everyKeyFunction[i][0];
            if (keys[key]){
                everyKeyFunction[i][1]();
            }
        }
        for (var i = 0; i < everyFrameFunction.length; i++){
            everyFrameFunction[i]();
        }
    };


    function beforeRun(){
        clear();
        sprite1 = new Sprite('#00FF00');
        everyFrameFunction = [];
        keys = {};
        everyKeyFunction = {};
    }

    function clear(){
        ctx.save();
        ctx.fillStyle = 'white';
        ctx.fillRect(0,0,WIDTH,HEIGHT);
        ctx.restore();
    }

    onResize();
    clear();

    Menu.item('set screen to wrap-around', setWrap);
    Menu.item('set screen to bounce', setBounce);
    Menu.item('accelerate', accelerate, 1, 'unit');
    Menu.item('rotate by', rotate, 1, 'degrees');
    Menu.item('move x to', moveToX, 20);
    Menu.item('move y to', moveToY, 20);
    Menu.item('while key down', onKey, 132, []);
    Menu.item('each frame', eachFrame, null, []);
    Menu.item('move', move);
    Menu.item('draw', draw);

    script.addEventListener('beforeRun', beforeRun, false); // always clear canvas first
    window.addEventListener('resize', onResize, false);
    canvas.addEventListener('keydown', onKeydown, false);
    canvas.addEventListener('keyup', onKeyup, false);
    script.addEventListener('everyFrame', everyFrame, false);

})(window);
