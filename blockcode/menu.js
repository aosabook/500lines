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

	// A menu block is just a normal block that is associated with a function
	// And it lives in the menu column.
	function menuItem(name, fn, value, contents){
		var item = Block.create(name, value, contents);
		scriptRegistry[name] = fn;
		menu.appendChild(item);
		return item;
	}

	// Run all the script blocks, let the specific language handle any tasks it needs
	// before and after the script is run
	//
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

	// As each block is run, set a class on it, then find and execute its associated
	// function. If we slow things down, you should be able to watch the code execute
	// as each block highlights to show when it is running.
	//
	function runEach(evt){
		var elem = evt.target;
		if (!matches(elem, '.script .block')) return;
		if (elem.dataset.name === 'Define block') return;
		elem.classList.add('running');
		scriptRegistry[elem.dataset.name](elem);
		elem.classList.remove('running');
	}

	// One of the default menu blocks reused in each language
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