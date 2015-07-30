var sheet,
    errs,
    vals;
self.onmessage = (function($__0) {
  var $__2;
  var data = $__0.data;
  ($__2 = [data, {}, {}], sheet = $__2[0], errs = $__2[1], vals = $__2[2], $__2);
  var $__3 = function(coord) {
    ['', '$'].map((function(p) {
      return [coord, coord.toLowerCase()].map((function(c) {
        var name = p + c;
        if ((Object.getOwnPropertyDescriptor(self, name) || {}).get) {
          return;
        }
        Object.defineProperty(self, name, {get: function() {
            var match;
            if (coord in vals) {
              return vals[coord];
            }
            vals[coord] = NaN;
            var x = +sheet[coord];
            if (sheet[coord] !== x.toString()) {
              x = sheet[coord];
            }
            try {
              vals[coord] = (('=' === x[0]) ? eval.call(null, x.slice(1)) : x);
            } catch (e) {
              match = /\$?[A-Za-z]+[1-9][0-9]*\b/.exec(e);
              if (match && !(match[0] in self)) {
                self[match[0]] = 0;
                delete vals[coord];
                return self[coord];
              }
              errs[coord] = e.toString();
            }
            switch (typeof vals[coord]) {
              case 'function':
              case 'object':
                vals[coord] += '';
            }
            return vals[coord];
          }});
      }));
    }));
  };
  for (var coord in sheet) {
    $__3(coord);
  }
  for (var coord$__4 in sheet) {
    self[coord$__4];
  }
  postMessage([errs, vals]);
});

//# sourceMappingURL=worker.map
