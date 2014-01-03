(function(global){


	function createBlock(name, value, contents){
		var item = elem('div', {'class': 'block', draggable: true, 'data-name': name}, [name])
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

	function blockContents(block){
		var container = block.querySelector('.container');
		if (container){
			return [].slice.call(container.children);
		}else{
			return null;
		}
	}

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

	window.addEventListener('unload', file.saveLocal, false);
	window.addEventListener('load', file.restoreLocal, false);

})(window);