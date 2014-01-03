(function(global){
	'use strict';

    // FIXME: Put all these routines in util namespace

    // Shorthand function to create elements easily
    // While also setting their attributes and adding child elements
    //
    global.elem = function elem(name, attrs, children){
        attrs = attrs || {};
        children = children || [];
        var e = document.createElement(name);
        Object.keys(attrs).forEach(function(key){
            e.setAttribute(key, attrs[key]);
        });
        children.forEach(function(child){
            e.appendChild(child);
        })
        return e;
    }

	// Remove namespace for matches
	// This is a lot of code just to get functionality that is already built-in
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

    // Emulate one of the handiest methods in all of jQuery
    // that isn't already built in to the browser yet
    //
    global.closest = function closest(elem, selector){
        while(elem){
            if (matches(elem, selector)) return elem;
            elem = elem.parentElement;
        }
        return null;
    };


    // Another polyfill for built-in functionality, just to get rid of namespaces in older
    // browsers, or to emulate it for browsers that don't have requestAnimationFrame yet
    global.requestAnimationFrame = global.requestAnimationFrame || global.mozRequestAnimationFrame || global.msRequestAnimationFrame || global.webkitRequestAnimationFrame || function(fn){
    	setTimeout(fn, 20);
    };

    // Send a custom event to an element
    global.trigger = function trigger(name, target){
        target.dispatchEvent(new CustomEvent(name, {bubbles: true, cancelable: false}));
    }

})(window);