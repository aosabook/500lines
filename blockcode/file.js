(function(global){
    'use strict';

    var scriptElem = document.querySelector('.script');
    var title = '__' + document.querySelector('title').textContent.toLowerCase().replace(' ', '_');

    function saveLocal(){
        var script = scriptToJson();
        if (script){
            localStorage[title] = script;
        }else{
            delete localStorage[title];
        }
    }

    function scriptToJson(){
        var blocks = [].slice.call(document.querySelectorAll('.script > .block'));
        return blocks.length ? JSON.stringify(blocks.map(Block.script)) : null;
    }

    function jsonToScript(json){
        clearScript();
        JSON.parse(json).forEach(function(block){
            scriptElem.appendChild(Block.create.apply(null, block));
        });
        Menu.runSoon();
    }

    function restoreLocal(){ jsonToScript(localStorage[title] || '[]' ); }

    function clearScript(){
        [].slice.call(document.querySelectorAll('.script > .block')).forEach(function(block){
            block.parentElement.removeChild(block);
        });
        Menu.runSoon();
    }

    function saveFile(evt){
        var title = prompt("Save file as: ");
        if (!title){ return; }
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
        var fileName = file.name;
        if (fileName.indexOf('.json', fileName.length - 5) === -1) {
            return alert('Not a JSON file');
        }
        var reader = new FileReader();
        reader.readAsText( file );
        reader.onload = function (evt){ jsonToScript(evt.target.result); };
    }

    function loadFile(){
        var input = elem('input', {'type': 'file', 'accept': 'application/json'});
        if (!input){ return; }
        input.addEventListener('change', function(evt){ readFile(input.files[0]); });
        input.click();
    }

    function loadExample(evt){
        var exampleName = evt.target.value;
        if (exampleName === ''){ return; }
        clearScript();
        file.examples[exampleName].forEach(function(block){
            scriptElem.appendChild(Block.create.apply(null, block));
        });
        Menu.runSoon();
    }

    global.file = {
        saveLocal: saveLocal,
        restoreLocal: restoreLocal,
        examples: {}
    };

    document.querySelector('.clear-action').addEventListener('click', clearScript, false);
    document.querySelector('.save-action').addEventListener('click', saveFile, false);
    document.querySelector('.load-action').addEventListener('click', loadFile, false);
    document.querySelector('.choose-example').addEventListener('change', loadExample, false);

})(window);
