if (self.importScripts) { importScripts("../node_modules/traceur/bin/traceur-runtime.js");
  try {
    throw undefined;
  } catch (vals) {
    try {
      throw undefined;
    } catch (errs) {
      try {
        throw undefined;
      } catch (sheet) {
        {
          ;
          self.onmessage = (function($__6) {
            var $__7;
            var data = $traceurRuntime.assertObject($__6).data;
            ($__7 = [data, {}, {}], sheet = $__7[0], errs = $__7[1], vals = $__7[2], $__7);
            for (var $coord in sheet) {
              try {
                throw undefined;
              } catch (coord) {
                {
                  coord = $coord;
                  for (var $__2 = (function() {
                    var $__0 = 0,
                        $__1 = [];
                    for (var $__4 = ['', '$'][Symbol.iterator](),
                        $__5; !($__5 = $__4.next()).done; ) {
                      try {
                        throw undefined;
                      } catch (p) {
                        {
                          p = $__5.value;
                          for (var $__2 = [coord, coord.toLowerCase()][Symbol.iterator](),
                              $__3; !($__3 = $__2.next()).done; ) {
                            try {
                              throw undefined;
                            } catch (c) {
                              {
                                c = $__3.value;
                                $__1[$__0++] = p + c;
                              }
                            }
                          }
                        }
                      }
                    }
                    return $__1;
                  }())[Symbol.iterator](),
                      $__3; !($__3 = $__2.next()).done; ) {
                    try {
                      throw undefined;
                    } catch (name) {
                      {
                        name = $__3.value;
                        {
                          if ((Object.getOwnPropertyDescriptor(self, name) || {}).get) {
                            continue;
                          }
                          Object.defineProperty(self, name, {get: function() {
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
                                try {
                                  throw undefined;
                                } catch (match) {
                                  {
                                    match = /\$?[A-Za-z]+[1-9][0-9]*\b/.exec(e);
                                    if (match && !(match[0] in self)) {
                                      self[match[0]] = 0;
                                      delete vals[coord];
                                      return self[coord];
                                    }
                                    errs[coord] = e.toString();
                                  }
                                }
                              }
                              return ((typeof vals[coord] === 'number') ? vals[coord] : vals[coord] += '');
                            }});
                        }
                      }
                    }
                  }
                }
              }
            }
            for (var $coord in sheet) {
              try {
                throw undefined;
              } catch (coord) {
                {
                  coord = $coord;
                  self[coord];
                }
              }
            }
            postMessage([errs, vals]);
          });
        }
      }
    }
  }
}

//# sourceMappingURL=worker.map
