Dagoba = {}

Dagoba.Query = {}

Dagoba.Query.add = function(list) {
  this.queue.push(list)
  return this
}

Dagoba.make_fun = function(name) {
  return function() { return this.add([name].concat(Array.prototype.slice.apply(arguments))) } }

Dagoba.query = function(graph) { 
  var query = Object.create(Dagoba.Query) 
  query.queue = []                  // an array of 
  query.graph = graph
  query.result = null
//  query.pointer = 0
//  query.history = []              // array of arrays, mapping queue position to state stack
  query.gremlins = []               // array of gremlins for each step
  query.state = []                  // array of state for each step
  return query
}

Dagoba.Query.run = function() {
  // for each queue element: invoke the attached function once, pass input to next function, until end
  // after end, back up to each function until you find one that still gives results, then move forward
  // if you reach the beginning and have no results, stop.
  // need a way for 'take 10' to keep everything prior to it from churning... the difference between 'no further results' and 'done'
  
  // THINK: run all the transformers first
  // THINK: check all the queue funs in Funs
  
  // DONE CONDITIONS:
  // - all gremlins are dead (good or bad)
  // - 'done' instead of 'no further results' [are these equivalent? can you get 'done' w/o being done? like g.out.take(10).out.take(5) ?]
  
  //  var item = this.queue[this.pointer]
  //  var history = this.history[this.pointer] || []
  // var foo = Dagoba.Funs[item[0]](this.graph, this.gremlins, history, item.slice(1))
  // this.gremlins = foo.gremlins // ugh this is unnecessary 
  // this.history[this.pointer] = foo.history[this.pointer] // kinda this too (but kinda not)
  
  var graph = this.graph
  var state = this.state
  var queue = this.queue
  var gremlins = this.gremlins
  
  if(!queue.length) {
    this.result = []
    return this }
  
  function gremlin_boxer(step_index) { return function(gremlin) { return [step_index, gremlin] } }
  
  function stepper(step_index, gremlin) {
    var step = queue[step_index]
    if(!Dagoba.Funs[step[0]]) return Dagoba.onError('Unrecognized function call: ' + step[0]) || {}
    return Dagoba.Funs[step[0]](graph, step.slice(1) || {}, gremlin || {}, state[step_index] || {})
  }
  
  function eat_result(step_index, result) {
    state[step_index] = result.state
    eat_gremlins(step_index, result)
  }
  
  function eat_gremlins(step_index, result) {
    gremlins = gremlins.concat((result.stay || []).map(gremlin_boxer(step_index))   || [])
                       .concat((result.go   || []).map(gremlin_boxer(step_index+1)) || [])
  }
  
  
  // add final queue item
  this.add(['collector'])         // the collector just takes gremlins out of circulation
  
  // bootstrap the first step
  // var step  = this.queue[0]
  // var args  = step.slice(1)
  // var state = this.state[0]
  // var gremlin = {}
  // var first = Dagoba.Funs[step[0]](graph, args, gremlin, state)
  // eat_gremlins(0, first)
  
  eat_gremlins(0, stepper(0))
  
  // process the queue
  while(gremlins.length) {
    var gremlinbox = gremlins.pop()
    var result = stepper(gremlinbox[0], gremlinbox[1])
    eat_result(gremlinbox[0], result)
  }
  
  // cultivate results
  // this.result = this.gremlins.filter(function(gremlin) {return gremlin.state == 'alive'})
  //                            .map(function(gremlin)    {return gremlin.path[gremlin.path.length-1]})
  var collection = this.state[this.state.length - 1] || []
  this.result = collection.map(function(gremlin) {return gremlin.vertex})

  this.result = Dagoba.firehooks('postquery', this, this.result)[0]

  return this.result
}

Dagoba.Query.name = function() {
  this.add(['name'])
  this.run()
  return this.result
  // return this.result.map(function(vertex) {return vertex.name})  // THINK: maybe this instead
}

var methods = ['out', 'in', 'attr', 'outV', 'outE', 'inV', 'inE', 'both', 'bothV', 'bothE', 'filter']
methods.forEach(function(name) {Dagoba.Query[name] = Dagoba.make_fun(name)})

Dagoba.Funs = {
  vertex: function(graph, args, gremlin, state) {
    // if(!gremlin) gremlin = Dagoba.make_gremlin()
    if(!gremlin.state)
      gremlin.state = graph.findVertices(args)
    if(gremlin.state.length == 0)
      return {} // original gremlin dies here...
    
    var vert = gremlin.state.pop()
    var vertex = graph.findVertexById(vert._id)
    var clone = Dagoba.make_gremlin(vertex)
    return {stay: [gremlin], go: [clone]}
  
    // if(state.status == 'done') return false    
    // var vertices = args[0] ? graph.findVertexById(args[0]) : graph.vertices
    // var gremlin = Dagoba.make_gremlin(vertex)
    // return {stay: [], go: [gremlin], state: {status: 'done'}}
    // return thread(args[0], graph.findVertexById, Dagoba.make_gremlin, ...)
    // return { gremlins: [{ state: 'alive', path: [graph.findVertexById(args[0])] }], history: ['done'] } 
  },
  
  out: function(graph, args, gremlin, state) {
    if(!gremlin.state)
      gremlin.state = graph.findOutEdges(gremlin.vertex).filter(Dagoba.filterThings(args[0]))
        //function(edge) {return args[0] ? edge._label == args[0] : true})
    if(gremlin.state.length == 0)
      return {} // original gremlin dies here...
    var vertex = gremlin.state.pop()._in
    var clone = Dagoba.make_gremlin(vertex)
    return {stay: [gremlin], go: [clone]}
  },
  
  'in': function(graph, args, gremlin, state) {
    if(!gremlin.state)
      gremlin.state = graph.findInEdges(gremlin.vertex).filter(Dagoba.filterThings(args[0]))
        //function(edge) {return args[0] ? edge._label == args[0] : true})
    if(gremlin.state.length == 0)
      return {} // original gremlin dies here...
    var vertex = gremlin.state.pop()._out
    var clone = Dagoba.make_gremlin(vertex)
    return {stay: [gremlin], go: [clone]}
  },
  
  // attr: function(graph, args, gremlin, state) {
  //   return graph.findVertexById(gremlin.vertex)[args[0]]
  // },
  
  collector: function(graph, args, gremlin, state) {
    return { state: (state.concat ? state : []).concat(gremlin) }
  }
}



Dagoba.make_gremlin = function(vertex, state) {
  return {vertex: vertex, state: state}
}

/*

  Daggr
  - add template
  - set heuristic function for determining template based on data
  - set layout function
  - set/add/remove data [immutable always for first pass]
  - render
  - add effect functions?
  - add layout/heuristic so they can be referenced by name

*/

/*
range = Array.apply(0, Array(10000)).map(function(val, key) {return key})
range.forEach(function(x) {G.addVertex({name: "foo" + x, age: x, lang: "idris"})})
range.forEach(function(x) {G.addEdge({weight: 2*(1+x), _label: "numnum", _out: x, _in: ~~(Math.random()*100)})})

ok. realistically, we can
- add edge refs internally and run perf tests [keep old style too]
- display graph walking for small graphs
- display graph walking for large graphs [only show local context to current step]
- rewrite in ES6
- history: back, loop, path count, etc
- smarter path count (merge gremlins)
- orthopto

*/



Dagoba.hooks = {}
Dagoba.addhook = function(type, callback) {
  if(!Dagoba.hooks[type]) Dagoba.hooks[type] = []
  Dagoba.hooks[type].push(callback)
}

Dagoba.firehooks = function(type, query) {
  var args = [].slice.call(arguments, 2)
  return ((Dagoba.hooks || {})[type] || []).reduce(function(acc, callback) {return callback.apply(query, acc)}, args)
}

Dagoba.addhook('postquery', Dagoba.uniqueify = function (results) { // THINK: should we inline this and merge&count in the gremlins?
  return [results.filter(function(item, index, array) {return array.indexOf(item) == index})]
})

Dagoba.addhook('postquery', Dagoba.cleanclone = function (results) { // THINK: do we always want this?
  return [results.map(function(item) {return JSON.parse(JSON.stringify(item, function(key, value) {return key[0]=='_' ? undefined : value}))})]
})



Dagoba.Graph = {}
Dagoba.Graph.v = function() {
  var query = Dagoba.query(this)
  query.add(['vertex'].concat( [].slice.call(arguments) ))
  return query
}

Dagoba.graph = function() { 
  var graph = Object.create( Dagoba.Graph ) 
  graph.vertices = [] // can't stick these on the prototype or they'll be shared
  graph.edges = []
  graph.vertexIndex = {}
  return graph
}

Dagoba.Graph.addVertex = function(vertex) {
  if(!vertex._id) 
    vertex._id = this.vertices.length+1
  this.vertices.push(vertex)
  this.vertexIndex[vertex._id] = vertex
  vertex._out = []; vertex._in = []
}

Dagoba.Graph.addVertices = function(vertices) {
  vertices.forEach(this.addVertex.bind(this))
}

Dagoba.Graph.addEdge = function(edge) {
  if(!edge._label) return false
  edge._in  = this.findVertexById(edge._in)
  edge._out = this.findVertexById(edge._out)
  if(!(edge._in && edge._out)) return false
  edge._in._out.push(edge)
  edge._in._in.push(edge)
  this.edges.push(edge)
}

Dagoba.Graph.addEdges = function(edges) {
  edges.forEach(this.addEdge.bind(this))
}

Dagoba.Graph.findVertexById = function(vertex_id) {
  return this.vertexIndex[vertex_id]
  // return this.vertices.first(function(vertex) {return vertex._id == vertex_id})
}

Dagoba.Graph.findVerticesByIds = function(ids) {
  return ids.length ? ids.map(this.findVertexById.bind(this)).filter(Boolean) : this.vertices.slice()
}

Dagoba.Graph.findVertices = function(ids) {
  return !ids.length || 1+ +ids[0] ? this.findVerticesByIds(ids) : this.searchVertices(ids)
}

Dagoba.Graph.searchVertices = function(obj) {
  return this.vertices.filter(
    function(vertex) {
      return Object.keys(obj[0]).reduce(
        function(acc, key) {
          return acc && obj[0][key] == vertex[key] }, true ) } ) }

Dagoba.Graph.findEdgeById = function(edge_id) {
  return this.edges.first(function(edge) {return edge._id == edge_id}) }

Dagoba.Graph.findOutEdges = function(vertex) {
  return vertex._out;
  return this.edges.filter(function(edge) {return edge._out == vertex._id}) }

Dagoba.Graph.findInEdges = function(vertex) {
  return vertex._in;
  return this.edges.filter(function(edge) {return edge._in == vertex._id}) }

Dagoba.filterThings = function(arg) {
  return function(thing) {
    return !arg ? true : arg+''===arg ? thing._label == arg
                : Array.isArray(arg)  ? !!~arg.indexOf(thing._label) : Dagoba.objFilter(thing, arg) } }

Dagoba.objFilter = function(thing, obj) {
  for(var key in obj)
    if(thing[key] != obj[key])
      return false; return true }

Array.prototype.first = function(fun) {
  for (var i = 0, len = this.length; i < len; i++)
    if(fun(this[i]))
      return this[i] }

Dagoba.onError = function(msg) {
  console.log(msg)
  return false
}