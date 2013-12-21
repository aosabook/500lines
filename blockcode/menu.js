(function(global){
	var menu = document.querySelector('.menu');
	var scriptRegistry = {};
	function menuItem(name, fn, value){
		var item = document.createElement('div');
		item.className = 'block';
		item.setAttribute('draggable', 'true');
		item.textContent = name;
		item.dataset.name = name;
		if (value !== undefined){
			var val = document.createElement('input');
			val.setAttribute('type', typeof value);
			val.value = value;
			item.appendChild(val);
		}
		scriptRegistry[name] = fn;
		menu.appendChild(item);
		return item;
	}

	function value(block){
		return parseInt(block.querySelector('input').value, 10);
	}

	function forward(block){
		global.turtle.forward(value(block));
	}

	function back(block){
		global.turtle.forward(-value(block));
	}

	function left(block){
		global.turtle.left(value(block));
	}

	function right(block){
		global.turtle.left(-value(block));
	}

	function penUp(){
		global.turtle.penUp();
	}

	function penDown(){
		global.turtle.penDown();
	}

	function hideTurtle(){
		global.turtle.hide();
	}

	function showTurtle(){
		global.turtle.show();
	}

	function repeat(block){
		var count = value(block);
		var children = [].slice.call(block.querySelector('.container').children);
		for (var i = 0; i < count; i++){
			runBlocks(children);
		}
	}

	function runBlocks(blocks){
		var evt = new CustomEvent('run', {bubbles: true, cancelable: false});
		blocks.forEach(function(block){
			block.dispatchEvent(evt);
		});
	}

	function run(){
		// debounce
		if (scriptDirty){
			scriptDirty = false;
			clear();
			var blocks = [].slice.call(document.querySelectorAll('.script > .block'));
			runBlocks(blocks);
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
	var repeatItem = menuItem('Repeat', repeat, 10);
	var container = document.createElement('div');
	container.className = 'container';
	repeatItem.appendChild(container.cloneNode(true));

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