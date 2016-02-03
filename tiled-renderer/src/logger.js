var logger = (function() {
    'use strict';
    var instance = {};

    instance.log = function(msg) {
        var p = document.createElement('p');
        p.appendChild(document.createTextNode(msg));
        document.getElementById('log').appendChild(p);
    };

    instance.logError = function(msg) {
        var p = document.createElement('p');
        p.appendChild(document.createTextNode(msg));
        p.style.color = 'red';
        p.style.marginTop = '10px';
        document.getElementById('log').appendChild(p);
    };

    instance.logDynamicLine = function(id, msg) {
        var p = document.getElementById(id);
        if (p === null) {
            p = document.createElement('p');
            p.id = id;
            document.getElementById('log').appendChild(p);
        } else {
            p.removeChild(p.firstChild);
        }
        p.appendChild(document.createTextNode(msg));
    };

    instance.logGLStatus = function(gl, msg) {
        if (gl.getError() != gl.NO_ERROR)
            this.logError('Error in ' + msg);
        else
            this.log('Completed '  + msg);
    };

    return instance;
})();

