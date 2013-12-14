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

	var dragTarget = null;
	var dragType = null;

	function dragStart(evt){
		if (!matches(evt.target, '.block')) return;
		if (matches(evt.target, '.menu .block')){
			dragType = 'menu';
		}else{
			dragType = 'script';
		}
		evt.target.style.opacity = '0.4';
		dragTarget = evt.target;
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

	function drag(evt){
		// this is where we can set the cursor to show where the element will end up
		// evlog(evt);
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
				// console.log('left all drag targets');
			}else{
				// console.log('mysterious');
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
  			// simulate proper moving with appendChild for now
  			dropTarget.appendChild(dragTarget);
		}else if (dragType === 'menu' && dropType === 'script'){
			// If dragging from menu to script, copy dragTarget
			var newNode = dragTarget.cloneNode(true);
			newNode.removeAttribute('style');
			dropTarget.appendChild(newNode);
		}else{
  			// If dragging from menu to menu, do nothing
		}
		// evlog(evt);
	};
	document.addEventListener('drop', drop, false);

	function dragEnd(evt){
		evt.target.style.opacity = '1.0';
		// evlog(evt);
	}
	document.addEventListener('dragend', dragEnd, false);
})(window);