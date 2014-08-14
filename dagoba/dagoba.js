/*
     ____  _____ _____ _____ _____ _____ 
    |    \|  _  |   __|     | __  |  _  |
    |  |  |     |  |  |  |  | __ -|     |
    |____/|__|__|_____|_____|_____|__|__|
    
    dagoba: a tiny in-memory graph database

    ex: 
    V = [ {name: 'alice'}                                         // alice gets auto-_id (prolly 1)
        , {_id: 10, name: 'bob', hobbies: ['asdf', {x:3}]}] 
    E = [ {_out: 1, _in: 10, _label: 'knows'} ]
    g = Dagoba.graph(V, E)
    
    g.addVertex({name: 'charlie', _id: 'charlie'})                // string ids are fine
    g.addVertex({name: 'delta', _id: '30'})                       // actually they're all strings

    g.addEdge({_out: 10, _in: 30, _label: 'parent'})
    g.addEdge({_out: 10, _in: 'charlie', _label: 'knows'})

    g.v(1).out('knows').out().run()                               // returns [charlie, delta]
    
    q = g.v(1).out('knows').out().take(1)
    q.run()                                                       // returns [charlie]
    q.run()                                                       // returns [delta]    (but don't rely on result order!)
    q.run()                                                       // returns []
*/


Dagoba = {}

Dagoba.G = {}                                                     // the prototype

Dagoba.graph = function(V, E) {                                   // the factory
  var graph = Object.create( Dagoba.G )
  graph.vertices = []                                             // fresh copies so they're not shared
  graph.edges = []
  graph.vertexIndex = {}
  if(V && Array.isArray(V)) graph.addVertices(V)                  // arrays only: singular V and E don't fly
  if(E && Array.isArray(E)) graph.addEdges(E)
  return graph
}

Dagoba.G.v = function() {                                         // a query initializer: g.v() -> query
  var query = Dagoba.query(this)
  query.add(['vertex'].concat( [].slice.call(arguments) ))
  return query
}

Dagoba.G.addVertex = function(vertex) {
  if(!vertex._id) 
    vertex._id = this.vertices.length+1
  // TODO: ensure unique _id
  this.vertices.push(vertex) // THINK: the user may retain a pointer to vertex, which they might mutate later >.<
  // can take away user's ability to set _id and lose the index cache hash, because building it causes big rebalancing slowdowns and runs the GC hard. (or does it?) [this was with a million items, indexed by consecutive ints. generally we need settable _id because we need to grab vertices quickly by external key]
  this.vertexIndex[vertex._id] = vertex
  vertex._out = []; vertex._in = []
}

Dagoba.G.addEdge = function(edge) {
  if(!edge._label) return false
  edge._in  = this.findVertexById(edge._in)
  edge._out = this.findVertexById(edge._out)
  if(!(edge._in && edge._out)) return false
  edge._out._out.push(edge)
  edge._in._in.push(edge)
  this.edges.push(edge)
}

Dagoba.G.addVertices = function(vertices) { vertices.forEach(this.addVertex.bind(this)) }
Dagoba.G.addEdges    = function(edges)    { edges   .forEach(this.addEdge  .bind(this)) }

Dagoba.G.findVertexById = function(vertex_id) {
  return this.vertexIndex[vertex_id] }

Dagoba.G.findVerticesByIds = function(ids) {
  return ids.length == 1 ? [].concat( this.findVertexById(ids[0]) || [] )
       : ids.map( this.findVertexById.bind(this) ).filter(Boolean) }

Dagoba.G.findVertices = function(ids) {
  return typeof ids[0] == 'object' ? this.searchVertices(ids[0])
       : ids.length == 0 ? this.vertices.slice()                  // OPT: do we need the slice?
       : this.findVerticesByIds(ids) }

Dagoba.G.searchVertices = function(obj) {
  return this.vertices.filter(
    function(vertex) {
      return Object.keys(obj).reduce(
        function(acc, key) {
          return acc && obj[key] == vertex[key] }, true ) } ) }

Dagoba.G.findEdgeById = function(edge_id) {
  return Dagoba.find(this.edges, function(edge) {return edge._id == edge_id} ) }

Dagoba.G.findOutEdges = function(vertex) { return vertex._out; }
Dagoba.G.findInEdges  = function(vertex) { return vertex._in;  }

Dagoba.G.toString = function() {                                  // kids, don't hand code JSON
  return '{"V":' + JSON.stringify(this.vertices, Dagoba.cleanvertex)
       + ',"E":' + JSON.stringify(this.edges,    Dagoba.cleanedge) 
       + '}' }

Dagoba.fromString = function(str) {                               // another graph constructor
  var obj = JSON.parse(str)
  return Dagoba.graph(obj.V, obj.E) 
}



Dagoba.Query = {}                                                 // prototype

Dagoba.query = function(graph) {                                  // factory (only called by a graph's query initializers)
  var query = Object.create(Dagoba.Query)
  
  query.   graph = graph                                          // the graph itself
  query.   state = []                                             // state for each step
  query. program = []                                             // list of steps to take  
  query.gremlins = []                                             // gremlins for each step
  
  return query
}

Dagoba.Query.run = function() {                                   // the magic lives here
  
  var graph = this.graph                                          // these are closed over in the helpers
  var state = this.state                                          // so we give them a spot in the frame
  var program  = this.program
  var gremlins = this.gremlins

  var max = program.length-1                                      // work backwards
  var pc = max                                                    // program counter
  var done = -1                                                   // behindwhich things have finished
  var results = []                                                // results for this run
  var maybe_gremlin = false                                       // a mythical beast

  if(!program.length) return []                                   // don't bother
  
  
  // driver loop
  while(done < max) {
    maybe_gremlin = try_step(pc, maybe_gremlin)                   // maybe_gremlin is a gremlin or (string | false)
    
    if(maybe_gremlin == 'pull') {
      maybe_gremlin = false
      if(pc-1 > done) {
        pc--
        continue
      } else {
        done = pc
      }
    }
    
    if(maybe_gremlin == 'done') {
      done = pc
      maybe_gremlin = false
    }
    
    pc++
    
    if(pc > max) {                                                // a gremlin is popping out of the pipeline. catch it!
      if(maybe_gremlin)
        results.push(maybe_gremlin)
      maybe_gremlin = false
      pc--
    }
  }

  // TODO: deal with gremlin paths / history and gremlin "collisions"
  
  results = results.map(function(gremlin) {                       // make this a query component (or posthook)
    return gremlin.result ? gremlin.result : gremlin.vertex } )

  results = Dagoba.firehooks('postquery', this, results)[0] 
  
  return results
  
  // NAMED HELPERS
  
  function try_step(pc, maybe_gremlin) {
    var step = program[pc]
    var my_state = (state[pc] = state[pc] || {})
    if(!Dagoba.QFuns[step[0]]) return Dagoba.onError('Unrecognized function call: ' + step[0]) || maybe_gremlin || 'pull'
    return Dagoba.QFuns[step[0]](graph, step.slice(1) || {}, maybe_gremlin, my_state)
  }
    
  function gremlin_boxer(step_index) { return function(gremlin) { return [step_index, gremlin] } }
  
  function stepper(step_index, gremlin) {
    var step = program[step_index]
    if(!Dagoba.QFuns[step[0]]) return Dagoba.onError('Unrecognized function call: ' + step[0]) || {}
    return Dagoba.QFuns[step[0]](graph, step.slice(1) || {}, gremlin || {}, state[step_index] || {})
  }
  
  function eat_gremlins(gremlins, step_index, result) {
    return gremlins.concat( (result.stay || []).map(gremlin_boxer(step_index))   )
                   .concat( (result.go   || []).map(gremlin_boxer(step_index+1)) ) }
  
  function setbang_gremlins(step_index, result) {gremlins = eat_gremlins(gremlins, step_index, result)}
}


Dagoba.Query.add = function(list) {                               // add a new traversal to the query
  this.program.push(list)
  return this
}

Dagoba.addQFun = function(name, fun) {                            // add a new traversal type
  Dagoba.QFuns[name] = fun
  Dagoba.Query[name] = function() { return this.add([name].concat([].slice.apply(arguments))) } 
  // TODO: accept string fun and allow extra params, for building quick aliases like
  //       Dagoba.addQFun('children', 'out') <-- if all out edges are kids
  //       Dagoba.addQFun('nthGGP', 'inN', 'parent')
  // var methods = ['out', 'in', 'take', 'property', 'outAllN', 'inAllN', 'unique', 'filter', 'outV', 'outE', 'inV', 'inE', 'both', 'bothV', 'bothE']
}


Dagoba.QFuns = {}                                                 // all traversal types

Dagoba.addQFun('vertex', function(graph, args, gremlin, state) {
  if(!state.vertices) state.vertices = graph.findVertices(args)
  if(!state.vertices.length) return 'done'
  var vertex = state.vertices.pop() 
  return Dagoba.make_gremlin(vertex)
})
  
Dagoba.addQFun('out', function(graph, args, gremlin, state) {
  if(!gremlin && (!state.edges || !state.edges.length)) return 'pull'
  if(!state.edges || !state.edges.length) 
    state.edges = graph.findOutEdges(gremlin.vertex).filter(Dagoba.filterThings(args[0]))
  if(!state.edges.length) return 'pull'
  var vertex = state.edges.pop()._in // what?
  var clone = Dagoba.make_gremlin(vertex) // we lose history here: use clone_gremlin(gremlin).goto(vertex) instead
  return clone
})

Dagoba.addQFun('outAllN', function(graph, args, gremlin, state) {
  var filter = args[0]
  var limit = args[1]-1
  
  if(!state.edgeList) { // initialize
    if(!gremlin) return 'pull'
    state.edgeList = []
    state.current = 0
    state.edgeList[0] = graph.findOutEdges(gremlin.vertex).filter(Dagoba.filterThings(filter))
  }
  
  if(!state.edgeList[state.current].length) { // finished this round
    if(state.current >= limit || !state.edgeList[state.current+1]   // totally done, or the next round has no items
                              || !state.edgeList[state.current+1].length) {
      state.edgeList = false
      return 'pull'
    }
    state.current++ // go to next round
    state.edgeList[state.current+1] = [] 
  }
  
  var vertex = state.edgeList[state.current].pop()._in
  
  if(state.current < limit) { // add all our matching edges to the next level
    if(!state.edgeList[state.current+1]) state.edgeList[state.current+1] = []
    state.edgeList[state.current+1] = state.edgeList[state.current+1].concat(
      graph.findOutEdges(vertex).filter(Dagoba.filterThings(filter))
    )
  }
  
  var clone = Dagoba.make_gremlin(vertex) // we lose history here: use clone_gremlin(gremlin).goto(vertex) instead
  return clone
})
  
Dagoba.addQFun('inAllN', function(graph, args, gremlin, state) {
  var filter = args[0]
  var limit = args[1]-1
  
  if(!state.edgeList) {                                           // initialize
    if(!gremlin) return 'pull'
    state.edgeList = []
    state.current = 0
    state.edgeList[0] = graph.findInEdges(gremlin.vertex).filter(Dagoba.filterThings(filter))
  }
  
  if(!state.edgeList[state.current].length) {                     // finished this round
    if(state.current >= limit || !state.edgeList[state.current+1] // totally done, or the next round has no items
                              || !state.edgeList[state.current+1].length) {
      state.edgeList = false
      return 'pull'
    }
    state.current++                                               // go to next round
    state.edgeList[state.current+1] = [] 
  }
  
  var vertex = state.edgeList[state.current].pop()._out
  
  if(state.current < limit) {                                     // add all our matching edges to the next level
    if(!state.edgeList[state.current+1]) state.edgeList[state.current+1] = []
    state.edgeList[state.current+1] = state.edgeList[state.current+1].concat(
      graph.findInEdges(vertex).filter(Dagoba.filterThings(filter))
    )
  }
  
  var clone = Dagoba.make_gremlin(vertex) // we lose history here: use clone_gremlin(gremlin).goto(vertex) instead
  return clone
})
  
Dagoba.addQFun('in', function(graph, args, gremlin, state) {
  if(!gremlin && (!state.edges || !state.edges.length)) return 'pull'
  if(!state.edges || !state.edges.length) 
    state.edges = graph.findInEdges(gremlin.vertex).filter(Dagoba.filterThings(args[0]))
  if(!state.edges.length) return 'pull'
  var vertex = state.edges.pop()._out // what? // also, abstract this...
  var clone = Dagoba.make_gremlin(vertex) // we lose history here: use clone_gremlin(gremlin).goto(vertex) instead
  return clone
})
  
Dagoba.addQFun('property', function(graph, args, gremlin, state) {
  if(!gremlin) return 'pull'
  gremlin.result = gremlin.vertex[args[0]]
  return gremlin
})
  
Dagoba.addQFun('unique', function(graph, args, gremlin, state) {
  if(!gremlin) return 'pull'
  if(state[gremlin.vertex._id]) return 'pull'                     // we've seen this gremlin, so get another instead
  state[gremlin.vertex._id] = true
  return gremlin
})
  
Dagoba.addQFun('filter', function(graph, args, gremlin, state) {
  if(!gremlin) return 'pull'
  if(typeof args[0] != 'function') return Dagoba.onError('Filter arg is not a function: ' + args[0]) || gremlin
  if(!args[0](gremlin.vertex)) return 'pull'                      // gremlin fails filter function 
  // THINK: would we ever want to filter by other parts of the gremlin?
  return gremlin
})
  
Dagoba.addQFun('take', function(graph, args, gremlin, state) {
  state.taken = state.taken ? state.taken : 0
  if(state.taken == args[0]) {
    state.taken = 0
    return 'done'
  }
  if(!gremlin) return 'pull'
  state.taken++ // FIXME: mutating! ugh!
  return gremlin
})



// hi! 
// - tune gremlins (collisions, history, etc)
// - interface: show query pieces and params,
// - interface: resumable queries
// - generational queries
// - intersections
// - adverbs
// - you are great!



Dagoba.hooks = {}

Dagoba.addhook = function(type, callback) {
  if(!Dagoba.hooks[type]) Dagoba.hooks[type] = []
  Dagoba.hooks[type].push(callback)
}

Dagoba.firehooks = function(type, query) {
  var args = [].slice.call(arguments, 2)
  return ((Dagoba.hooks || {})[type] || []).reduce(function(acc, callback) {return callback.apply(query, acc)}, args)
}

Dagoba.make_gremlin = function(vertex, state) { return {vertex: vertex, state: state} }

Dagoba.filterThings = function(arg) {
  return function(thing) {
    return !arg ? true                                                                           // nothing is true
         : arg+'' === arg ? thing._label == arg                                                  // check the label
         : Array.isArray(arg) ? !!~arg.indexOf(thing._label) : Dagoba.objFilter(thing, arg) } }  // or a list of labels

Dagoba.objFilter = function(thing, obj) {
  for(var key in obj)
    if(thing[key] != obj[key])
      return false; return true }

Dagoba.find = function(arr, fun) {
  for (var i = 0, len = arr.length; i < len; i++)
    if(fun(arr[i], i, arr))
      return arr[i] }

Dagoba.cleanvertex = function(key, value) {return (key == '_in' || key == '_out') ? undefined : value} // for JSON.stringify
Dagoba.cleanedge   = function(key, value) {return key == '_in' ? value._id : key == '_out' ? value._id : value}

Dagoba.uniqueify = function (results) { // OPT: do this in the query via gremlin collision counting
  return [results.filter(function(item, index, array) {return array.indexOf(item) == index})]}

Dagoba.cleanclone = function (results) { // remove all _-prefixed properties
 return [results.map(function(item) {return JSON.parse(JSON.stringify(item, function(key, value) {return key[0]=='_' ? undefined : value}))})]}

// NOTE: add these hooks if you need them. (our vertex payloads are immutable, and we uniqueify prior to taking.)

// Dagoba.addhook('postquery', Dagoba.uniqueify)
// Dagoba.addhook('postquery', Dagoba.cleanclone)

// THINK: the uniquify hook happens after the take component so it smushes results down, possibly returning fewer than you wanted...
  
Dagoba.onError = function(msg) {
  console.log(msg)
  return false 
}