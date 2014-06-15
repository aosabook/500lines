calc = -> jThread(worker, -> console.log JSON.stringify it) it
calc A1: '=-B1-B2' A2: '=$B1*-$A1' B1: 123 B2: 456
calc A1: '=A2+1' A2: '=A1+2'
calc A1: '=$A2'

function worker (sheet)
  eval-global = self.execScript ? -> eval.call null, it
  cache = {}
  for let coord of sheet
    for pre in ['', '$'] => for cr in [coord, coord.toLowerCase!]
      Object.defineProperty self, "#pre#cr", get: ->
        if coord of cache then cache[coord] else cache[coord] = try
          cache[coord] = NaN
          cell = sheet[coord]
          if cell is /^=/ then eval-global cell.slice(1) else cell
        catch => if e is /Error: (\$?[A-Za-z]+[1-9][0-9]*) is not defined/
          and RegExp.$1 not of self then
            self[RegExp.$1] = 0
            delete cache[coord]
            eval-global cell.slice(1)
          else NaN
  return { [coord, self[coord]] for coord of sheet }

function jThread (fun, done)
  return that(fun, done) if window?jThread
  thread = require \webworker-threads .create!
  return (arg) -> thread.eval "(function(self){ return JSON.stringify((#fun)(#{
    JSON.stringify arg
  })) })(this)" (,it) -> thread.destroy!; done JSON.parse it
