(function(global){
	'use strict';
	// Remove namespace for matches
    if (document.body.matches){
        global.matches = function matches(elem, selector){ return elem.matches(selector); };
    }else if(document.body.mozMatchesSelector){
        global.matches = function matches(elem, selector){ return elem.mozMatchesSelector(selector); };
    }else if (document.body.webkitMatchesSelector){
        global.matches = function matches(elem, selector){ return elem.webkitMatchesSelector(selector); };
    }else if (document.body.msMatchesSelector){
        global.matches = function matches(elem, selector){ return elem.msMatchesSelector(selector); };
    }else if(document.body.oMatchesSelector){
        global.matches = function matches(elem, selector){ return elem.oMatchesSelector(selector); };
    }

    global.closest = function closest(elem, selector){
        while(elem){
            if (matches(elem, selector)) return elem;
            elem = elem.parentElement;
        }
        return null;
    };

    global.requestAnimationFrame = global.requestAnimationFrame || global.mozRequestAnimationFrame || global.msRequestAnimationFrame || global.webkitRequestAnimationFrame || function(fn){
    	setTimeout(fn, 20);
    };

	var dragTarget = null;
	var dragType = null;
	var scriptBlocks = [];

	function dragStart(evt){
		if (!matches(evt.target, '.block')) return;
		if (matches(evt.target, '.menu .block')){
			dragType = 'menu';
		}else{
			dragType = 'script';
		}
		evt.target.classList.add('dragging');
		dragTarget = evt.target;
		scriptBlocks = [].slice.call(document.querySelectorAll('.script .block:not(.dragging)'));
		scriptBlocks.sort(function(a,b){
			return a.getBoundingClientRect().top - b.getBoundingClientRect().top;
		});
		// For dragging to take place in Firefox, we have to set this, even if we don't use it
		evt.dataTransfer.setData('text/html', evt.target.outerHTML);
		if (matches(evt.target, '.menu .block')){
			evt.dataTransfer.effectAllowed = 'copy';
		}else{
			evt.dataTransfer.effectAllowed = 'move';
		}
		// evlog(evt);
	}
	document.addEventListener('dragstart', dragStart, false);

	// Block we'll be inserting before
	var nextBlock = null;

	function drag(evt){
		// Find which block we should insert the dragged block before
		var prevBlock = nextBlock;
		nextBlock = null;
		// x, pageX, clientX, layerX, screenX 
		var x = evt.clientX;
		var y = evt.clientY;
		var offset = 15; // pixels cursor can overlap a block by
		for (var i = 0; i < scriptBlocks.length; i++){
			var block = scriptBlocks[i];
			var rect = block.getBoundingClientRect();
			if (y < (rect.top + offset)){
				nextBlock = block;
				break;
			}
		}
		if (prevBlock !== nextBlock){
			if (prevBlock){
				prevBlock.classList.remove('next');
			}
			if (nextBlock){
				nextBlock.classList.add('next');
			}
		}
	}
	document.addEventListener('drag', drag, false);

	function dragEnter(evt){
		// evlog(evt);
		if (matches(evt.target, '.menu, .script')){
			evt.target.classList.add('over');
		}else{
			if (!matches(evt.target, '.menu *, .script *')){
				var over = document.querySelector('.over');
				if (over){
					over.classList.remove('over');
				}
				evt.target.classList.remove('over');
			}
		}
		if (evt.preventDefault) {
			evt.preventDefault(); // Necessary. Allows us to drop.
			}
		return false;
	}
	document.addEventListener('dragenter', dragEnter, false);

	function dragOver(evt){
		// evlog(evt);
		if (!matches(evt.target, '.menu, .menu *, .script, .script *')) return;
		if (evt.preventDefault) {
			evt.preventDefault(); // Necessary. Allows us to drop.
			}
			if (dragType === 'menu'){
			evt.dataTransfer.dropEffect = 'copy';  // See the section on the DataTransfer object.
		}else{
			evt.dataTransfer.dropEffect = 'move';
		}
		return false;
	}
	document.addEventListener('dragover', dragOver, false);

	function drop(evt){
		if (!matches(evt.target, '.menu, .menu *, .script, .script *')) return;
		var dropTarget = closest(evt.target, '.menu, .script');
		var dropType = 'script';
		if (matches(dropTarget, '.menu')){
			dropType = 'menu';
		}
		if (evt.stopPropagation) {
		    evt.stopPropagation(); // stops the browser from redirecting.
		}
		if (dragType === 'script' && dropType === 'menu'){
			// If dragging from script to menu, delete dragTarget
			dragTarget.parentElement.removeChild(dragTarget);
		}else if (dragType ==='script' && dropType === 'script'){
  			// If dragging from script to script, move dragTarget
  			dropTarget.insertBefore(dragTarget, nextBlock);
		}else if (dragType === 'menu' && dropType === 'script'){
			// If dragging from menu to script, copy dragTarget
			var newNode = dragTarget.cloneNode(true);
			newNode.classList.remove('dragging');
			dropTarget.insertBefore(newNode, nextBlock);
		}else{
  			// If dragging from menu to menu, do nothing
		}
		// evlog(evt);
	};
	document.addEventListener('drop', drop, false);

	function dragEnd(evt){
		// clean up dragging styles
		// this looks like a goog place for a helper class
		// evt.target.style.opacity = '1.0';
		var dragging = document.querySelector('.dragging');
		if (dragging){
			dragging.classList.remove('dragging');
		}
		var over = document.querySelector('.over');
		if (over){
			over.classList.remove('over');
		}
		var next = document.querySelector('.next');
		if (next){
			next.classList.remove('next');
		}
		// evlog(evt);
	}
	document.addEventListener('dragend', dragEnd, false);
})(window);