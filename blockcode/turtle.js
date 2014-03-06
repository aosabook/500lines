(function(){
	'use strict';

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

	var cos = Math.cos, sin = Math.sin, sqrt = Math.sqrt, PI = Math.PI;
	var DEGREE = PI / 180;

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

	function deg2rad(degrees){
		return DEGREE * degrees;
	}

	function drawTurtle(){
		var userPen = pen; // save pen state
		if (visible){
			penUp();
			_moveForward(5);
			penDown();
			_turn(-150);
			_moveForward(12);
			_turn(-120);
			_moveForward(12);
			_turn(-120);
			_moveForward(12);
			_turn(30);
			penUp();
			_moveForward(-5);
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

	function _moveForward(distance){
		var start = position;
		position = {
			x: cos(direction) * distance + start.x,
			y: -sin(direction) * distance + start.y
		};
		if (pen){
			ctx.lineStyle = color;
			ctx.beginPath();
			ctx.moveTo(start.x, start.y);
			ctx.lineTo(position.x, position.y);
			ctx.stroke();
		}
	}

	function forward(block){
		_moveForward(Block.value(block));
	}

	function back(block){
		_moveForward(-Block.value(block));
	}

	function _turn(degrees){
		direction += deg2rad(degrees);
	}

	function left(block){
		_turn(Block.value(block));
	}

	function right(block){
		_turn(-Block.value(block));
	}

	function clear(){
		ctx.save();
		ctx.fillStyle = 'white';
		ctx.fillRect(0,0,WIDTH,HEIGHT);
		ctx.restore();
		reset();
		ctx.moveTo(position.x, position.y);
	}

	clear();
	drawTurtle();

	menu.item('Left', left, 5);
	menu.item('Right', right, 5);
	menu.item('Forward', forward, 10);
	menu.item('Back', back, 10);
	menu.item('Pen up', penUp);
	menu.item('Pen down', penDown);
	menu.item('Back to center', recenter);
	menu.item('Hide turtle', hideTurtle);
	menu.item('Show turtle', showTurtle);

	script.addEventListener('beforeRun', clear, false); // always clear canvas first
	script.addEventListener('afterRun', drawTurtle, false); // show turtle if visible
	window.addEventListener('resize', onResize, false);

})(window);