'use strict'
{
  const globalEval = self.execScript || ( (js)=>eval.call( null, js ) )
  self.onmessage = (event)=>{ self.postMessage( calc( event ) ) }
  let cache, errs, sheet
  function calc(event) {
    [cache, errs, sheet] = [ {}, {}, event.data ]
    for (const coord in sheet) {
      for (const pre of [ '', '$' ]) {
        for (const cr of [ coord, coord.toLowerCase() ]) {
          if (( Object.getOwnPropertyDescriptor( self, pre+cr ) || {} ).get) { continue }
          Object.defineProperty( self, pre+cr, { get: ()=>{
            if (coord in cache) { return cache[coord] }
            cache[coord] = NaN
            let val = +sheet[coord]
            if (sheet[coord] != val) { val = sheet[coord] }
            try { cache[coord] = ( ( '=' === val[0] ) ? globalEval( val.slice(1) ) : val ) }
            catch (e) {
              const match = /\$?[A-Za-z]+[1-9][0-9]*\b/.exec( e )
              if (match && !( match[0] in self )) {
                self[match[0]] = 0
                delete cache[coord]
                return self[coord]
              }
              errs[coord] = e.toString()
            }
            return cache[coord]
          } } )
        }
      }
    }
    const vals = {}
    for (const coord in sheet) { vals[coord] = self[coord] }
    return [ errs, vals ]
  }
}
