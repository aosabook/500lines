(function(global){

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

	function blockFromScript(script){
		return createBlock.apply(null, script);
	}

	function saveLocal(){
		var blocks = [].slice.call(document.querySelectorAll('.script > .block'));
		var script = blocks.map(blockScript);
		localStorage._blockCode = JSON.stringify(script);
	}

	function restoreLocal(){
		if (!localStorage['_blockCode']) return;
		var scriptElem = document.querySelector('.script');
		JSON.parse(localStorage._blockCode).forEach(function(block){
			scriptElem.appendChild(createBlock.apply(null, block));
		});
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
		run: runBlocks,
		fromScript: blockFromScript,
		saveLocal: saveLocal,
		restoreLocal: restoreLocal
	}

	window.addEventListener('unload', saveLocal, false);
	window.addEventListener('load', restoreLocal, false);

})(window);