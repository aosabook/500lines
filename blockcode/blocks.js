(function(global){

	function createBlock(name, value, contents){
		var item = document.createElement('div');
		item.className = 'block';
		item.setAttribute('draggable', 'true');
		item.textContent = name;
		item.dataset.name = name;
		if (value !== undefined){
			var val = document.createElement('input');
			val.setAttribute('type', 'number');
			val.value = value;
			item.appendChild(val);
		}
		if (Array.isArray(contents)){
			var container = document.createElement('div');
			container.className = 'container';
			item.appendChild(container);
			// FIXME, where should we convert from script to DOM?
			contents.forEach(function(block){
				container.appendChild(block);
			});
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
		var input = block.querySelector('input[type=number]');
		if (input){
			return Number(input.value);
		}else{
			return null;
		}
	}

	function blockScript(block, indent){
		if (indent === undefined){
			indent = '';
		}
		var script = [
			indent,
			block.dataset.name
		];
		var value = blockValue(block);
		if (value !== null){
			script.push(' ' + value);
		}
		var contents = blockContents(block);
		if (contents !== null){
			script.push('[')
			extendedScript = contents.map(function(child){
				return blockScript(child, indent+'  ');
			});
			extendedScript.unshift(script.join(''));
			extendedScript.push(indent + ']');
		}else{
			return script.join('');
		}
	}

	function runBlocks(blocks){
		var evt = new CustomEvent('run', {bubbles: true, cancelable: false});
		blocks.forEach(function(block){
			block.dispatchEvent(evt);
		});
	}


	global.Block = {
		create: createBlock,
		value: blockValue,
		contents: blockContents,
		script: blockScript,
		run: runBlocks
	}

})(window);