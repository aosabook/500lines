(function(){
	'use strict';

	//
	// This is the implmentation of the turtle block language. It exposes no globals of its own
	// 

	// Variables for managing the canvas itself
	//
	var canvas = document.querySelector('.canvas');
	var script = document.querySelector('.script');
	var ctx = canvas.getContext('2d');
	canvas.setAttribute('width', canvas.scrollWidth);
	canvas.setAttribute('height', canvas.scrollHeight);
	var WIDTH = canvas.scrollWidth;
	var HEIGHT = canvas.scrollHeight;

	function onResize(evt){
		canvas.setAttribute('width', canvas.scrollWidth);
		canvas.setAttribute('height', canvas.scrollHeight);
		WIDTH = canvas.scrollWidth;
		HEIGHT = canvas.scrollHeight;
		menu.runSoon();
	}

	// Save some handier references to frequently-used parts of Math
	var cos = Math.cos, sin = Math.sin, sqrt = Math.sqrt, PI = Math.PI;
	var DEGREE = PI / 180;

	// State (we could embed this in an object if we wanted to support more than one turtle)
	var position, direction, visible, pen, color;
	function reset(){
		recenter();
		direction = deg2rad(90); // facing "up"		
		visible = true;
		pen = true; // when pen is true we draw, otherwise we move without drawing
		color = 'black';
	}

	function recenter(){
		position = {x: WIDTH/2, y: HEIGHT/2};
	}

	// Utility so we can work with degrees, but draw in radians
	function deg2rad(degrees){
		return DEGREE * degrees;
	}

	function drawTurtle(){
		// draw the turtle. Default turtle is a triangle
		var userPen = pen; // save pen state
		if (visible){
			penUp();
			moveForward(5);
			penDown();
			turn(-150);
			moveForward(12);
			turn(-120);
			moveForward(12);
			turn(-120);
			moveForward(12);
			turn(30);
			penUp();
			moveForward(-5);
			if (userPen){
				penDown(); // restore pen state
			}
		}
	}

	function penUp(){
		pen = false;
	}

	function penDown(){
		pen = true;
	}

	function hideTurtle(){
		visible = false;
	}

	function showTurtle(){
		visible = true;
	}

	// internal function
	function moveForward(distance){
		var start = position;
		position = {
			x: cos(direction) * distance + start.x,
			y: -sin(direction) * distance + start.y
		}
		if (pen){
			ctx.lineStyle = color;
			ctx.beginPath();
			ctx.moveTo(start.x, start.y);
			ctx.lineTo(position.x, position.y);
			ctx.stroke();
		}
	}

	function forward(block){
		moveForward(Block.value(block));
	}

	function back(block){
		moveForward(-Block.value(block));
	}

	// internal function for turning
	function turn(degrees){
		direction += deg2rad(degrees);
	}

	function left(block){
		turn(Block.value(block));
	}

	function right(block){
		turn(-Block.value(block));
	}

	function clear(){
		ctx.save();
		ctx.fillStyle = 'white';
		ctx.fillRect(0,0,WIDTH,HEIGHT);
		ctx.restore();
		reset();
		ctx.moveTo(position.x, position.y);
	}

	// initialization
	clear();
	drawTurtle();

	// Set up menu for the turtle language
	//
	menu.item('Left', left, 5);
	menu.item('Right', right, 5);
	menu.item('Forward', forward, 10);
	menu.item('Back', back, 10);
	menu.item('Pen up', penUp);
	menu.item('Pen down', penDown);
	menu.item('Back to center', recenter);
	menu.item('Hide turtle', hideTurtle);
	menu.item('Show turtle', showTurtle);

	// Handler to run before script runs, always clear canvas first
	//
	script.addEventListener('beforeRun', clear, false);
	// Handler to run after script runs, show turtle if visible
	script.addEventListener('afterRun', drawTurtle, false);
	window.addEventListener('resize', onResize, false);

})(window);