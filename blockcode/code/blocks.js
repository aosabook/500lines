(function(global){
	'use strict';

	function createBlock(name, value, contents){
		var item = elem('div', {'class': 'block', draggable: true, 'data-name': name}, [name]);
		if (value !== undefined && value !== null){
			item.appendChild(elem('input', {type: 'number', value: value}));
		}
		if (Array.isArray(contents)){
			item.appendChild(elem('div', {'class': 'container'}, contents.map(function(block){
				return createBlock.apply(null, block);
			})));
		}else if (typeof contents === 'string'){ // Add units specifier
			item.appendChild(document.createTextNode(' ' + contents));
		}
		return item;
	}

	function blockContents(block){
		var container = block.querySelector('.container');
		return container ? [].slice.call(container.children) : null;
	}

	function blockValue(block){
		var input = block.querySelector('input');
		return input ? Number(input.value) : null;
	}

	function blockUnits(block){
		if (block.children.length > 1 && block.lastChild.nodeType === Node.TEXT_NODE && block.lastChild.textContent){
			return block.lastChild.textContent.slice(1);
		}
	}

	function blockScript(block){
		var script = [block.dataset.name];
        var value = blockValue(block);
        if (value !== null){
    		script.push(blockValue(block));
        }
		var contents = blockContents(block);
		var units = blockUnits(block);
		if (contents){script.push(contents.map(blockScript));}
		if (units){script.push(units);}
		return script.filter(function(notNull){ return notNull !== null; });
	}

	function runBlocks(blocks){
		blocks.forEach(function(block){ trigger('run', block); });
	}

	global.Block = {
		create: createBlock,
		value: blockValue,
		contents: blockContents,
		script: blockScript,
		run: runBlocks,
		trigger: trigger
	};

	window.addEventListener('unload', file.saveLocal, false);
	window.addEventListener('load', file.restoreLocal, false);
})(window);
