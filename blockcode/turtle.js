(function(global){

	var canvas = document.querySelector('.turtle-canvas');
	canvas.setAttribute('width', canvas.scrollWidth);
	canvas.setAttribute('height', canvas.scrollHeight);
	var ctx = canvas.getContext('2d');
	var WIDTH = canvas.scrollWidth;
	var HEIGHT = canvas.scrollHeight;
	var cos = Math.cos, sin = Math.sin, sqrt = Math.sqrt, PI = Math.PI;
	var DEGREE = PI / 180;

	function deg2rad(degrees){
		return DEGREE * degrees;
	}

	function rad2deg(radians){
		return radians / DEGREE;
	}

	function Turtle(){
		this.reset();
	}

	Turtle.prototype.reset = function reset(){
		this.position = {x: WIDTH/2, y: HEIGHT/2};
		this.direction = deg2rad(90);
		this.visible = true;
		this.pen = true;
		this.color = 'black';
	}

	Turtle.prototype.getDirection = function getDirection(){
		return rad2deg(this.direction);
	}

	Turtle.prototype.draw = function draw(){
		// draw the turtle. Default turtle is a triangle
	}

	Turtle.prototype.penup = function penup(){
		this.pen = false;
	}

	Turtle.prototype.pendown = function pendown(){
		this.pen = true;
	}

	Turtle.prototype.forward = function(distance){
		var start = this.position;
		this.position = {
			x: cos(this.direction) * distance + start.x,
			y: -sin(this.direction) * distance + start.y
		}
		if (this.pen){
			ctx.lineStyle = this.color;
			ctx.beginPath();
			ctx.moveTo(start.x, start.y);
			ctx.lineTo(this.position.x, this.position.y);
			ctx.stroke();
			console.log('lineTo(%s, %s)', this.position.x, this.position.y);
		}
	}

	Turtle.prototype.left = function left(degrees){
		this.direction += deg2rad(degrees);
	}

	function clear(){
		ctx.save();
		ctx.fillStyle = 'white';
		ctx.fill(0,0,WIDTH,HEIGHT);
		ctx.restore();
		ctx.moveTo(turtle.position.x, turtle.position.y);
		turtle.reset();
		turtle.draw();
	}
	// initialization
	global.clear = clear;
	global.turtle = new Turtle();
	global.ctx = ctx;
	clear();
})(window);