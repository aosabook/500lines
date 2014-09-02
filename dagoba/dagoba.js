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
    g.addVertex({name: 'delta', _id: '30'})                       // in fact they're all strings

    g.addEdge({_out: 10, _in: 30, _label: 'parent'})
    g.addEdge({_out: 10, _in: 'charlie', _label: 'knows'})

    g.v(1).out('knows').out().run()                               // returns [charlie, delta]
    
    q = g.v(1).out('knows').out().take(1)
    q.run()                                                       // returns [charlie]
    q.run()                                                       // returns [delta]    (but don't rely on result order!)
    q.run()                                                       // returns []

    Dagoba consists of two parts: graphs and queries.
    A graph contains vertices and edges, and provides access to query initializers like g.v()
    A query contains pipes, which make up a pipeline.
    There are also a few helper functions.
    That's all.
*/


Dagoba = {}                                                       // the namespace

Dagoba.G = {}                                                     // the prototype

Dagoba.graph = function(V, E) {                                   // the factory
  var graph = Object.create( Dagoba.G )
  graph.vertices = []                                             // fresh copies so they're not shared
  graph.edges    = []
  graph.vertexIndex = {}
  if(V && Array.isArray(V)) graph.addVertices(V)                  // arrays only, because you wouldn't
  if(E && Array.isArray(E)) graph.addEdges(E)                     // call this with singular V and E
  return graph
}

Dagoba.G.v = function() {                                         // a query initializer: g.v() -> query
  var query = Dagoba.query(this)
  query.add(['vertex'].concat( [].slice.call(arguments) ))        // add vertex as first query pipe
  return query
}

Dagoba.G.addVertex = function(vertex) {
  if(!vertex._id)                                                 // TODO: ensure unique _id
    vertex._id = this.vertices.length+1
  
  this.vertices.push(vertex)
  this.vertexIndex[vertex._id] = vertex
  vertex._out = []; vertex._in = []                               // placeholders for edge pointers
  return vertex._id
}

Dagoba.G.addEdge = function(edge) {
  if(!edge._label) return false                                   // all edges must be labeled // THINK: why?
  edge._in  = this.findVertexById(edge._in)
  edge._out = this.findVertexById(edge._out)
  if(!(edge._in && edge._out)) return false
  edge._out._out.push(edge)                                       // add edge to the edge's out vertex's out edges
  edge._in._in.push(edge)                                         // vice versa
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

Dagoba.G.searchVertices = function(obj) {                         // find vertices that match obj's key-value pairs
  return this.vertices.filter(
    function(vertex) {
      return Object.keys(obj).reduce(
        function(acc, key) {
          return acc && obj[key] == vertex[key] }, true ) } ) }

Dagoba.G.findEdgeById = function(edge_id) {
  return Dagoba.find(this.edges, function(edge) {
      return edge._id == edge_id} ) }

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



Dagoba.Q = {}                                                     // prototype

Dagoba.query = function(graph) {                                  // factory (only called by a graph's query initializers)
  var query = Object.create( Dagoba.Q )
  
  query.   graph = graph                                          // the graph itself
  query.   state = []                                             // state for each step
  query. program = []                                             // list of steps to take  
  query.gremlins = []                                             // gremlins for each step

  return query
}

Dagoba.Q.run = function() {                                       // the magic lives here
  
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
    maybe_gremlin = try_step(pc, maybe_gremlin)                   // maybe_gremlin is a gremlin, a primitive, or false
    
    if(maybe_gremlin == 'pull') {                                 // 'pull' is an out-of-band signal,
      maybe_gremlin = false                                       // telling us the pipe wants further input
      if(pc-1 > done) {
        pc--
        continue
      } else {
        done = pc
      }
    }
    
    if(maybe_gremlin == 'done') {                                 // 'done' is on out-of-band signal,
      done = pc                                                   // telling us the pipe is finished
      maybe_gremlin = false
    }
    
    pc++
    
    if(pc > max) {
      if(maybe_gremlin)
        results.push(maybe_gremlin)                               // a gremlin popped out the end of the pipeline
      maybe_gremlin = false
      pc--
    }
  }

  // TODO: deal with gremlin paths / history and gremlin "collisions"
  
  results = results.map(function(gremlin) {                       // THINK: make this a pipe type (or posthook)
    return gremlin.result != null ? gremlin.result : gremlin.vertex } )

  results = Dagoba.fireHooks('postquery', this, results)[0] 
  
  return results
  
  // NOTE: these helpers are inside our closure
  
  function try_step(pc, maybe_gremlin) {
    var step = program[pc]
    var my_state = (state[pc] = state[pc] || {})
    if(!Dagoba.PipeTypes[step[0]]) 
        return Dagoba.onError('Unrecognized pipe type: ' + step[0]) || maybe_gremlin || 'pull'
    return Dagoba.PipeTypes[step[0]](graph, step.slice(1) || {}, maybe_gremlin, my_state)
  }
    
  function gremlin_boxer(step_index) { return function(gremlin) { return [step_index, gremlin] } }
  
  function stepper(step_index, gremlin) {
    var step = program[step_index]
    if(!Dagoba.PipeTypes[step[0]]) 
        return Dagoba.onError('Unrecognized pipe type: ' + step[0]) || {}
    return Dagoba.PipeTypes[step[0]](graph, step.slice(1) || {}, gremlin || {}, state[step_index] || {})
  }
  
  function eat_gremlins(gremlins, step_index, result) {
    return gremlins.concat( (result.stay || []).map(gremlin_boxer(step_index))   )
                   .concat( (result.go   || []).map(gremlin_boxer(step_index+1)) ) }
  
  function setbang_gremlins(step_index, result) {gremlins = eat_gremlins(gremlins, step_index, result)}
}


Dagoba.Q.add = function(list) {                                   // add a new pipe to the query
  this.program.push(list)
  return this
}

Dagoba.PipeTypes = {}                                             // every pipe has a type

Dagoba.addPipeType = function(name, fun) {
  Dagoba.PipeTypes[name] = fun
  Dagoba.Q[name] = function() { return this.add([name].concat([].slice.apply(arguments))) } 
}


// BUILT-IN PIPE TYPES


Dagoba.addPipeType('vertex', function(graph, args, gremlin, state) {
  if(!state.vertices) state.vertices = graph.findVertices(args)
  if(!state.vertices.length) return 'done'
  var vertex = state.vertices.pop()
  return Dagoba.makeGremlin(vertex, (gremlin||{}).state)          // use incoming gremlin state, if it exists
})
  
Dagoba.addPipeType('out', function(graph, args, gremlin, state) {
  if(!gremlin && (!state.edges || !state.edges.length)) return 'pull'
  
  if(!state.edges || !state.edges.length) {
    state.gremlin = gremlin
    state.edges = graph.findOutEdges(gremlin.vertex).filter(Dagoba.filterEdges(args[0]))
  }
  
  if(!state.edges.length) return 'pull'
  
  var vertex = state.edges.pop()._in // what?
  return Dagoba.gotoVertex(state.gremlin, vertex)
})

Dagoba.addPipeType('in', function(graph, args, gremlin, state) {
  if(!gremlin && (!state.edges || !state.edges.length)) return 'pull'
  
  if(!state.edges || !state.edges.length) {
    state.gremlin = gremlin
    state.edges = graph.findInEdges(gremlin.vertex).filter(Dagoba.filterEdges(args[0]))
  }
  
  if(!state.edges.length) return 'pull'
  
  var vertex = state.edges.pop()._out // what?
  return Dagoba.gotoVertex(state.gremlin, vertex)
})
  

// TODO: show how to refactor 'out', 'outN', and 'outAllN' using adverbs. also the 'in' equivalents. also make adverbs.

Dagoba.addPipeType('outAllN', function(graph, args, gremlin, state) {
  var filter = args[0]
  var limit = args[1]-1
  
  if(!state.edgeList) {                                           // initialize
    if(!gremlin) return 'pull'
    state.edgeList = []
    state.current = 0
    state.edgeList[0] = graph.findOutEdges(gremlin.vertex).filter(Dagoba.filterEdges(filter))
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
  
  var vertex = state.edgeList[state.current].pop()._in
  
  if(state.current < limit) {                                     // add all our matching edges to the next level
    if(!state.edgeList[state.current+1]) state.edgeList[state.current+1] = []
    state.edgeList[state.current+1] = state.edgeList[state.current+1].concat(
      graph.findOutEdges(vertex).filter(Dagoba.filterEdges(filter))
    )
  }
  
  return Dagoba.gotoVertex(gremlin, vertex)
})
  
Dagoba.addPipeType('inAllN', function(graph, args, gremlin, state) {
  var filter = args[0]
  var limit = args[1]-1
  
  if(!state.edgeList) {                                           // initialize
    if(!gremlin) return 'pull'
    state.edgeList = []
    state.current = 0
    state.edgeList[0] = graph.findInEdges(gremlin.vertex).filter(Dagoba.filterEdges(filter))
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
      graph.findInEdges(vertex).filter(Dagoba.filterEdges(filter))
    )
  }
  
  return Dagoba.gotoVertex(gremlin.state, vertex)
})
  
Dagoba.addPipeType('property', function(graph, args, gremlin, state) {
  if(!gremlin) return 'pull'
  gremlin.result = gremlin.vertex[args[0]]
  return gremlin.result == null ? false : gremlin                 // undefined or null properties kill the gremlin
})
  
Dagoba.addPipeType('unique', function(graph, args, gremlin, state) {
  if(!gremlin) return 'pull'
  if(state[gremlin.vertex._id]) return 'pull'                     // we've seen this gremlin, so get another instead
  state[gremlin.vertex._id] = true
  return gremlin
})
  
Dagoba.addPipeType('filter', function(graph, args, gremlin, state) {
  if(!gremlin) return 'pull'
  if(typeof args[0] != 'function') 
    return Dagoba.onError('Filter arg is not a function: ' + args[0]) || gremlin
  if(!args[0](gremlin.vertex, gremlin)) return 'pull'             // gremlin fails filter function 
  return gremlin
})
  
Dagoba.addPipeType('take', function(graph, args, gremlin, state) {
  state.taken = state.taken ? state.taken : 0
  if(state.taken == args[0]) {
    state.taken = 0
    return 'done'
  }
  if(!gremlin) return 'pull'
  state.taken++ // FIXME: mutating! ugh!
  return gremlin
})

Dagoba.addPipeType('as', function(graph, args, gremlin, state) {
  if(!gremlin) return 'pull'
  gremlin.state.as = gremlin.state.as ? gremlin.state.as : {}     // initialize gremlin's 'as' state
  gremlin.state.as[args[0]] = gremlin.vertex                      // set label to the current vertex
  return gremlin
})

Dagoba.addPipeType('back', function(graph, args, gremlin, state) {
  if(!gremlin) return 'pull'
  return Dagoba.gotoVertex(gremlin, gremlin.state.as[args[0]])    // TODO: check for nulls
})

Dagoba.addPipeType('except', function(graph, args, gremlin, state) {
  if(!gremlin) return 'pull'
  if(gremlin.vertex == gremlin.state.as[args[0]]) return 'pull'   // TODO: check for nulls
  return gremlin
})


// HELPER FUNCTIONS

Dagoba.hooks = {}                                                 // callbacks triggered on various occasions

Dagoba.addHook = function(type, callback) {                       // add a new callback
  if(!Dagoba.hooks[type]) Dagoba.hooks[type] = []
  Dagoba.hooks[type].push(callback)
}

Dagoba.fireHooks = function(type, query) {                        // trigger callbacks of type 'type'
  var args = [].slice.call(arguments, 2)
  return ((Dagoba.hooks || {})[type] || []).reduce(
      function(acc, callback) {
          return callback.apply(query, acc)}, args) }

Dagoba.makeGremlin = function(vertex, state) {                    // gremlins are simple creatures: 
  return {vertex: vertex, state: state || {} } }                  // a current vertex, and some state

Dagoba.gotoVertex = function(gremlin, vertex) {                   // clone the gremlin 
  return Dagoba.makeGremlin(vertex, gremlin.state) }              // THINK: add path tracking here?

Dagoba.filterEdges = function(arg) {
  return function(thing) {
    return !arg ? true                                            // nothing is true
         : arg+'' === arg ? thing._label == arg                   // check the label
         : Array.isArray(arg) ? !!~arg.indexOf(thing._label)      // or a list of labels
         : Dagoba.objectFilter(thing, arg) } }                    // try it as an object

Dagoba.objectFilter = function(thing, obj) {
  for(var key in obj)
    if(thing[key] != obj[key])
      return false; return true }

Dagoba.find = function(arr, fun) {
  for (var i = 0, len = arr.length; i < len; i++)
    if(fun(arr[i], i, arr))
      return arr[i] }

Dagoba.cleanvertex = function(key, value) {return (key == '_in' || key == '_out') ? undefined : value} // for JSON.stringify
Dagoba.cleanedge   = function(key, value) {return key == '_in' ? value._id : key == '_out' ? value._id : value}

Dagoba.uniqueify = function (results) {                           // OPT: do this in the query via gremlin collision counting
  return [results.filter(function(item, index, array) {return array.indexOf(item) == index})]}

Dagoba.cleanclone = function (results) {                          // remove all _-prefixed properties
 return [results.map(function(item) {return JSON.parse(JSON.stringify(item, function(key, value) {return key[0]=='_' ? undefined : value}))})]}
  
Dagoba.onError = function(msg) {
  console.log(msg)
  return false 
}


// more todos
// - tune gremlins (collisions, history, etc)
// - interface: show query pieces and params,
// - interface: resumable queries
// - generational queries
// - intersections
// - adverbs

// THINK: the user may retain a pointer to vertex, which they might mutate later >.<
// can take away user's ability to set _id and lose the index cache hash, because building it causes big rebalancing slowdowns and runs the GC hard. (or does it?) [this was with a million items, indexed by consecutive ints. generally we need settable _id because we need to grab vertices quickly by external key]
// OPT: we could also/instead take away edge _id setting, and then get rid of Dagoba.find and make findEdgeById like findVertexById (ish)


/*
        ---> no edge _id stuff
        ---> simplify driver loop helpers
        ---> refactor outAllN etc (mmm but adverbs?)
        ---> leo's queries !
*/


// re: Dagoba.Q.addPipeType
// TODO: accept string fun and allow extra params, for building quick aliases like
//       Dagoba.addPipeType('children', 'out') <-- if all out edges are kids
//       Dagoba.addPipeType('nthGGP', 'inN', 'parent')
// var methods = ['out', 'in', 'take', 'property', 'outAllN', 'inAllN', 'unique', 'filter', 'outV', 'outE', 'inV', 'inE', 'both', 'bothV', 'bothE']

// re: hooks
// NOTE: add these hooks if you need them. (our vertex payloads are immutable, and we uniqueify prior to taking.)
// Dagoba.addHook('postquery', Dagoba.uniqueify)
// Dagoba.addHook('postquery', Dagoba.cleanclone)
// THINK: the uniquify hook happens after the take component so it smushes results down, possibly returning fewer than you wanted...
