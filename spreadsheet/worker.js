{
  const globalEval = self.execScript || ( (js)=>eval.call( null, js ) )
  self.onmessage = (event)=>{ self.postMessage( calc( event ) ) }
  let cache, errs, sheet
  function calc({data}) {
    [cache, errs, sheet] = [ {}, {}, data ]
    for (const coord in sheet) {
      for (const name of [ for (p of [ '', '$' ]) for (c of [ coord, coord.toLowerCase() ]) p+c ]) {
        if (( Object.getOwnPropertyDescriptor( self, name ) || {} ).get) { continue }
        Object.defineProperty( self, name, { get: ()=>{
          if (coord in cache) { return cache[coord] }
          cache[coord] = NaN
          let val = +sheet[coord]
          if (sheet[coord] != val.toString()) { val = sheet[coord] }
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
    const vals = {}
    for (const coord in sheet) { vals[coord] = self[coord] }
    return [ errs, vals ]
  }
}
