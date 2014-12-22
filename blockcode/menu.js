(function(global){
	'use strict';

	var menu = document.querySelector('.menu');
	var script = document.querySelector('.script');
	var scriptRegistry = {};
	var scriptDirty = false;

	function runSoon(){ scriptDirty = true; }

	function menuItem(name, fn, value, units){
		var item = Block.create(name, value, units);
		scriptRegistry[name] = fn;
		menu.appendChild(item);
		return item;
	}

	function run(){
		if (scriptDirty){
			scriptDirty = false;
			Block.trigger('beforeRun', script);
			var blocks = [].slice.call(document.querySelectorAll('.script > .block'));
			Block.run(blocks);
			Block.trigger('afterRun', script);
		}else{
            Block.trigger('everyFrame', script);
        }
		requestAnimationFrame(run);
	}
	requestAnimationFrame(run);

	function runEach(evt){
		var elem = evt.target;
		if (!matches(elem, '.script .block')) return;
		if (elem.dataset.name === 'Define block') return;
		elem.classList.add('running');
		scriptRegistry[elem.dataset.name](elem);
		elem.classList.remove('running');
	}

	function repeat(block){
		var count = Block.value(block);
		var children = Block.contents(block);
		for (var i = 0; i < count; i++){
			Block.run(children);
		}
	}
	menuItem('Repeat', repeat, 10, []);


	global.Menu = {
		runSoon: runSoon,
		item: menuItem
	};

	document.addEventListener('drop', runSoon, false);
	script.addEventListener('run', runEach, false);
	script.addEventListener('change', runSoon, false);
	script.addEventListener('keyup', runSoon, false);
})(window);
