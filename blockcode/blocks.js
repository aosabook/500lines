(function(global){


	// This can be used to create blocks for the menu, or
	// for restoring blocks saved in files or localStorage
	//
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
		localStorage._blockCode = scriptToJson();
	}

	// Utility for converting scripts to JSON
	//
	function scriptToJson(){
		var blocks = [].slice.call(document.querySelectorAll('.script > .block'));
		return JSON.stringify(blocks.map(blockScript));		
	}

	// Utility for converting JSON to scripts
	//
	function jsonToScript(json){
		var scriptElem = document.querySelector('.script');
		clearScript();
		JSON.parse(json).forEach(function(block){
			scriptElem.appendChild(createBlock.apply(null, block));
		});
		menu.runSoon();
	}

	// Handler to restore the current script on page refresh
	//
	function restoreLocal(){
		jsonToScript(localStorage._blockCode || '[]');
	}

	// Handler to clear the current script
	//
	function clearScript(){
		[].slice.call(document.querySelectorAll('.script > .block')).forEach(function(block){
			block.parentElement.removeChild(block);
		});
		menu.runSoon();
	}

	// Handler to save to a local file
	//
	function saveFile(evt){
	    var title = prompt("Save file as: ");
	    if (!title) return;
		var file = new Blob([scriptToJson()], {type: 'application/json'});
		var reader = new FileReader();
		var a = document.createElement('a');
		reader.onloadend = function(){
			var a = elem('a', {'href': reader.result, 'download': title + '.json'});
			a.click();
		};
		reader.readAsDataURL(file);
	}

	// Handler to load from a local file
	function readFile(file){
		fileName = file.name;
		if (fileName.indexOf('.json', fileName.length - 5) === -1) {
			alert('Not a JSON file');
			return;
		}
		var reader = new FileReader();
		reader.readAsText( file );
		reader.onload = function (evt){
			jsonToScript(evt.target.result);
		};
	}

	function loadFile(){
		var input = elem('input', {'type': 'file', 'accept': 'application/json'});
		if (!input) return;
		input.addEventListener('change', function(evt){
			readFile(input.files[0]);
		});
		input.click();
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

	document.querySelector('.clear-action').addEventListener('click', clearScript, false);
	document.querySelector('.save-action').addEventListener('click', saveFile, false);
	document.querySelector('.load-action').addEventListener('click', loadFile, false);

	window.addEventListener('unload', saveLocal, false);
	window.addEventListener('load', restoreLocal, false);

})(window);