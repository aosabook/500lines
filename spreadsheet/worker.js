if (this.importScripts) {
  // Fast eval without the "this" object; available as execScript in IE
  const globalEval = self.execScript || ( (js)=>eval.call( null, js ) );

  // Forward the incoming messages to calc(), and post its result back
  self.onmessage = (event)=>{ self.postMessage( calc( event ) ) };

  function calc({data: sheet}) {
    let cache = {}, errs = {};

    for (const coord in sheet) {
      // Four variable names pointing to the same coordinate: A1, a1, $A1, $a1
      for (const name of [ for (p of [ '', '$' ]) for (c of [ coord, coord.toLowerCase() ]) p+c ]) {
        // Worker is reused across calculations, so only define each variable once
        if (( Object.getOwnPropertyDescriptor( self, name ) || {} ).get) { continue; }

        // Define self['A1'], which is the same thing as the global variable A1
        Object.defineProperty( self, name, { get() {
          if (coord in cache) { return cache[coord]; }
          cache[coord] = NaN;

          // Convert numeric-looking strings into numbers so =A1+C1 works when both are numbers
          let val = +sheet[coord];
          if (sheet[coord] != val.toString()) { val = sheet[coord]; }

          // Evaluate formula cells that begin with =
          try { cache[coord] = ( ('=' === val[0]) ? globalEval( val.slice( 1 ) ) : val ); }
          catch (e) {
            const match = /\$?[A-Za-z]+[1-9][0-9]*\b/.exec( e );
            if (match && !( match[0] in self )) {
              // The formula refers to a uninitialized cell; set it to 0 and retry
              self[match[0]] = 0;
              delete cache[coord];
              return self[coord];
            }
            // Otherwise, stringify the caught exception in the errs object
            errs[coord] = e.toString();
          }
          return cache[coord];
        } } )
      }
    }

    // For each coordinate in the sheet, call the property getter defined above
    let vals = {};
    for (const coord in sheet) { vals[coord] = self[coord]; }
    return [ errs, vals ];
  }
}
