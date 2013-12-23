(function(global){

	// Shorthand function to create elements easily
	// While also setting their attributes and adding child elements
	//
	function elem(name, attrs, children){
		children = children || [];
		var e = document.createElement(name);
		Object.keys(attrs).forEach(function(attr){
			e.setAttribute(attr, attrs[attr]);
		});
		children.forEach(function(child){
			e.appendChild(child);
		})
		return e;
	}

	// This can be used to create blocks for the menu, or
	// for restoring blocks saved in files or localStorage
	//
	function createBlock(name, value, contents){
		var item = elem('div', {'class': 'block', draggable: true})
		item.textContent = name;
		item.dataset.name = name;
		if (value !== undefined){
			item.appendChild(elem('input', {type: 'number', value: value}));
		}
		if (Array.isArray(contents)){
			item.appendChild(elem('div', {'class': 'container'}, contents.map(function(block){
				return createBlock.apply(null, block);
			})));
		}
		return item;		
	}

	// Simply retrieve the child blocks of a container block. Always returns a list
	// if called on a container block, always returns null on a simple block
	//
	function blockContents(block){
		var container = block.querySelector('.container');
		if (container){
			return [].slice.call(container.children);
		}else{
			return null;
		}
	}

	// Return the numerical value of the input on a block, if the block has
	// an input field of type number, or string for other input type, null if there
	// is no input element for the block.
	//
	function blockValue(block){
		var input = block.querySelector('input');
		if (input){
			if (input.type === 'number'){
				return Number(input.value);
			}else{
				return input.value;
			}
		}else{
			return null;
		}
	}

	// Returns the script of a block as a structure suitable for stringifying with JSON.
	// Used for saving blocks in a form they can easily be restored from
	//
	function blockScript(block){
		var script = [block.dataset.name];
		var value = blockValue(block);
		if (value !== null){
			script.push(value);
		}
		var contents = blockContents(block);
		if (contents !== null){
			script.push(contents.map(blockScript));
		}
		return script;
	}

	// Handler to save the current script in localStorage on page refresh
	//
	function saveLocal(){
		var blocks = [].slice.call(document.querySelectorAll('.script > .block'));
		var script = blocks.map(blockScript);
		localStorage._blockCode = JSON.stringify(script);
	}

	// Handler to restore the current script on page refresh
	//
	function restoreLocal(){
		if (!localStorage['_blockCode']) return;
		var scriptElem = document.querySelector('.script');
		JSON.parse(localStorage._blockCode).forEach(function(block){
			scriptElem.appendChild(createBlock.apply(null, block));
		});
		menu.runSoon();
	}

	// Send a custom event to an element
	function trigger(name, target){
		target.dispatchEvent(new CustomEvent(name, {bubbles: true, cancelable: false}));
	}

	// Handler to run an array of blocks by sending each block the "run" event
	//
	function runBlocks(blocks){
		blocks.forEach(function(block){
			trigger('run', block);
		});
	}

	// Expose some functions outside this file. 
	global.Block = {
		create: createBlock,
		value: blockValue,
		contents: blockContents,
		script: blockScript,
		run: runBlocks,
		trigger: trigger
	}

	window.addEventListener('unload', saveLocal, false);
	window.addEventListener('load', restoreLocal, false);

})(window);