(function(global){

	function saveLocal(){
		localStorage._blockCode = scriptToJson();
	}

	function scriptToJson(){
		var blocks = [].slice.call(document.querySelectorAll('.script > .block'));
		return JSON.stringify(blocks.map(Block.script));		
	}

	function jsonToScript(json){
		var scriptElem = document.querySelector('.script');
		clearScript();
		JSON.parse(json).forEach(function(block){
			scriptElem.appendChild(Block.create.apply(null, block));
		});
		menu.runSoon();
	}

	function restoreLocal(){
		jsonToScript(localStorage._blockCode || '[]');
	}

	function clearScript(){
		[].slice.call(document.querySelectorAll('.script > .block')).forEach(function(block){
			block.parentElement.removeChild(block);
		});
		menu.runSoon();
	}

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