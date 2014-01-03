(function(global){
	// Handler to save the current script in localStorage on page refresh
	//
	function saveLocal(){
		localStorage._blockCode = scriptToJson();
	}

	// Utility for converting scripts to JSON
	//
	function scriptToJson(){
		var blocks = [].slice.call(document.querySelectorAll('.script > .block'));
		return JSON.stringify(blocks.map(Block.script));		
	}

	// Utility for converting JSON to scripts
	//
	function jsonToScript(json){
		var scriptElem = document.querySelector('.script');
		clearScript();
		JSON.parse(json).forEach(function(block){
			scriptElem.appendChild(Block.create.apply(null, block));
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

	global.file = {
		saveLocal: saveLocal,
		restoreLocal: restoreLocal
	};

	document.querySelector('.clear-action').addEventListener('click', clearScript, false);
	document.querySelector('.save-action').addEventListener('click', saveFile, false);
	document.querySelector('.load-action').addEventListener('click', loadFile, false);

})(window);