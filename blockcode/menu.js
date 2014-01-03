(function(global){

	// We use these a lot, keep references around
	var menu = document.querySelector('.menu');
	var script = document.querySelector('.script');

	// This is where we will store the scripts of blocks in the menu
	// We use a very simple name -> script mapping, so it does not support
	// either multiple menu blocks with the same name, or renaming blocks
	//
	var scriptRegistry = {};

	// Keep track of whether the script has been modified since the last time
	// it was run, so we don't keep trying to run it constantly
	//
	var scriptDirty = false;

	// Flag the system that we should run the assembled script during the next 
	// frame handler
	function runSoon(){
		scriptDirty = true;
	}

	function menuItem(name, fn, value, contents){
		var item = Block.create(name, value, contents);
		scriptRegistry[name] = fn;
		menu.appendChild(item);
		return item;
	}

	function run(){
		// debounce
		if (scriptDirty){
			scriptDirty = false;
			Block.trigger('beforeRun', script);
			var blocks = [].slice.call(document.querySelectorAll('.script > .block'));
			Block.run(blocks);
			Block.trigger('afterRun', script);
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

	// Create a new type of block, don't respond to run() normally
	function defineBlock(block){

	}
	// menuItem('Define block', function(){}, 'New block', []);


	global.menu = {
		runSoon: runSoon,
		item: menuItem
	}

	document.addEventListener('drop', runSoon, false);
	script.addEventListener('run', runEach, false);
	script.addEventListener('change', runSoon, false);
	script.addEventListener('keyup', runSoon, false);
})(window);