(function(global){
    'use strict';

    var dragTarget = null; // Block we're dragging
    var dragType = null; // Are we dragging from the menu or from the script?
    var scriptBlocks = []; // Blocks in the script, sorted by position

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
        // For dragging to take place in Firefox, we have to set this, even if we don't use it
        evt.dataTransfer.setData('text/html', evt.target.outerHTML);
        if (matches(evt.target, '.menu .block')){
            evt.dataTransfer.effectAllowed = 'copy';
        }else{
            evt.dataTransfer.effectAllowed = 'move';
        }
    }


    function dragEnter(evt){
        if (matches(evt.target, '.menu, .script, .content')){
            evt.target.classList.add('over');
            if (evt.preventDefault) { evt.preventDefault(); }// Necessary. Allows us to drop.
        }else{
            if (!matches(evt.target, '.menu *, .script *')){
                _findAndRemoveClass('over');
                evt.target.classList.remove('over');
            }
        }
        return false;
    }

    function dragOver(evt){
        if (!matches(evt.target, '.menu, .menu *, .script, .script *, .content')) return;
        if (evt.preventDefault) { evt.preventDefault(); } // Necessary. Allows us to drop.
        if (dragType === 'menu'){
            evt.dataTransfer.dropEffect = 'copy';  // See the section on the DataTransfer object.
        }else{
            evt.dataTransfer.dropEffect = 'move';
        }
        return false;
    }

    function drop(evt){
        if (!matches(evt.target, '.menu, .menu *, .script, .script *')) return;
        var dropTarget = closest(evt.target, '.script .container, .script .block, .menu, .script');
        var dropType = 'script';
        if (matches(dropTarget, '.menu')){ dropType = 'menu'; }
        if (evt.stopPropagation) { evt.stopPropagation(); } // stops the browser from redirecting.
        if (dragType === 'script' && dropType === 'menu'){
            trigger('blockRemoved', dragTarget.parentElement, dragTarget);
            dragTarget.parentElement.removeChild(dragTarget);
        }else if (dragType ==='script' && dropType === 'script'){
            if (matches(dropTarget, '.block')){
                dropTarget.parentElement.insertBefore(dragTarget, dropTarget.nextSibling);
            }else{
                dropTarget.insertBefore(dragTarget, dropTarget.firstChildElement);
            }
            trigger('blockMoved', dropTarget, dragTarget);
        }else if (dragType === 'menu' && dropType === 'script'){
            var newNode = dragTarget.cloneNode(true);
            newNode.classList.remove('dragging');
            if (matches(dropTarget, '.block')){
                dropTarget.parentElement.insertBefore(newNode, dropTarget.nextSibling);
            }else{
                dropTarget.insertBefore(newNode, dropTarget.firstChildElement);
            }
            trigger('blockAdded', dropTarget, newNode);
        }
    }

    function _findAndRemoveClass(klass){
        var elem = document.querySelector('.' + klass);
        if (elem){ elem.classList.remove(klass); }
    }

    function dragEnd(evt){
        _findAndRemoveClass('dragging');
        _findAndRemoveClass('over');
        _findAndRemoveClass('next');
    }

    document.addEventListener('dragstart', dragStart, false);
    document.addEventListener('dragenter', dragEnter, false);
    document.addEventListener('dragover', dragOver, false);
    document.addEventListener('drag', function(){}, false);
    document.addEventListener('drop', drop, false);
    document.addEventListener('dragend', dragEnd, false);
})(window);
