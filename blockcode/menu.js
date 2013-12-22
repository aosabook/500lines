(function(global){
	var menu = document.querySelector('.menu');
	var scriptRegistry = {};
	function menuItem(name, fn, value, contents){
		var item = Block.create(name, value, contents);
		scriptRegistry[name] = fn;
		menu.appendChild(item);
		return item;
	}

	function forward(block){
		turtle.forward(Block.value(block));
	}

	function back(block){
		turtle.forward(-Block.value(block));
	}

	function left(block){
		turtle.left(Block.value(block));
	}

	function right(block){
		turtle.left(-Block.value(block));
	}

	function penUp(){
		turtle.penUp();
	}

	function penDown(){
		turtle.penDown();
	}

	function hideTurtle(){
		turtle.hide();
	}

	function showTurtle(){
		turtle.show();
	}

	function repeat(block){
		var count = Block.value(block);
		var children = Block.contents(block);
		for (var i = 0; i < count; i++){
			Block.run(children);
		}
	}
	function run(){
		// debounce
		if (scriptDirty){
			scriptDirty = false;
			clear();
			var blocks = [].slice.call(document.querySelectorAll('.script > .block'));
			Block.run(blocks);
			turtle.draw();
		}
		requestAnimationFrame(run);
	}
	requestAnimationFrame(run);

	function runEach(evt){
		var elem = evt.target;
		if (!matches(elem, '.script .block')) return;
		elem.classList.add('running');
		scriptRegistry[elem.dataset.name](elem);
		elem.classList.remove('running');
	}

	menuItem('Forward', forward, 10);
	menuItem('Back', back, 10);
	menuItem('Left', left, 5);
	menuItem('Right', right, 5);
	menuItem('Pen up', penUp);
	menuItem('Pen down', penDown);
	menuItem('Hide turtle', hideTurtle);
	menuItem('Show turtle', showTurtle);
	menuItem('Repeat', repeat, 10, []);

	var scriptDirty = false;
	function runSoon(){
		console.log('runSoon: %o', runSoon.caller);
		scriptDirty = true;
	}

	document.addEventListener('drop', runSoon, false);
	var script = document.querySelector('.script');
	script.addEventListener('run', runEach, false);
	script.addEventListener('change', runSoon, false);
	script.addEventListener('keyup', runSoon, false);
})(window);