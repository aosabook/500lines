# Dagoba: an in-memory graph database

_An exploration of connectedness through the lens of genetic and familial lineage_

A long time ago, when the world was still young, all data walked happily in single file. To ask a question of your data you merely put a fence in the path, and each datum jumped it in turn. Life was easy and programming was a breeze.

Then came the random access revolution, and data grazed freely across the hillside. Herding data became a serious concern -- if you can access any piece of data at any time, how do you know which one to pick next? Techniques were developed for corralling the data by forming links between items [N] [the network model (CODASYL), the hierarchical model (IMS), etc], marshaling groups of units into formation through their linking assemblage. Questioning data meant picking a sheep and pulling along everything connected to it. 

Later programmers departed from this tradition, imposing a set of rules on how data would be aggregated. [N] [Codd, etc] Rather than tying disparate data directly together they would cluster by content, decomposing data into bite-sized pieces, clustered in kennels and collared with a name tag. Questions were declaratively posited, resulting in accumulating pieces of partially decomposed data (a state the relationalists refer to as "normal") into a frankencollection returned to the programmer.

For much of recorded history this relational model reigned supreme. Its dominance remained unchallenged through two major language wars and countless skirmishes. It offered everything you could ask for in a model, for the small price of inefficiency, clumsiness and lack of scalability. For eons that was a price programmers were willing to pay. Then the internet happened.

The distributed revolution changed everything, again. Data broke free of spacial constraints and roamed from machine to machine. CAP-wielding theorists busted the relational monopoly, opening the door to a plethora of new herding techniques -- some of which harken back to the earliest attempts to domesticate random-access data. We're going to look at one of these, a style known as the graph database.



## What's all this now?

Graph databases allow us to elegantly solve all kinds of interesting problems. So what's a graph database?

Well, the dictionary defines "graph database" as a database for graphs. Thanks, dictionary! Let's break that down a little.

A data base is like a fort for data. You can put data in it and get data back out of it.

A graph in this sense is a set of vertices and a set of edges. It's basically a bunch of dots connected by lines. 

What kinds of problems can we solve? Suppose that you are one of those who have discovered the unbridled joy of tracking ancestral trees: parents, children, all that kind of thing. You'd like to develop a system that allows you to make natural and elegant queries like "Who is Thor's second cousin once removed?" or "How many of Freya's descendants are Valkyries?".

A reasonable schema for this data structure would be to have a table of entities and a table of relationships. A query for Thor's parents might look like:

```
SELECT e.* FROM entities as e, relationships as r WHERE r.out = "Thor" AND r.type = "parent" AND r.in = e.id
```

But how do we extend that to grandparents? We need to do a subquery, or use some other type of vendor-specific extension to SQL. And by the time we get to second cousins once removed we're going to have ALOTTA SQL. [I don't like ALOTTA.]

What would we like to write? Something both concise and flexible; something that models our query in a natural way and extends to other queries like it. ```second_cousins_once_removed('Thor')``` is concise, but it doesn't give us any flexibility. The SQL above is flexible, but lacks concision.

Something like ```Thor.parents.parents.parents.children.children.children``` strikes a reasonably good balance. The primitives give us flexibility to ask many similar questions, but the query is also very concise and natural. This particular phrasing gives us too many results, as it includes first cousins and siblings, but we're going for gestalt here.

What's the simplest thing we can build that gives us this kind of interface? We could make a list of entities, and a list of edges, just like the relational schema, and then build some helper functions. It might look something like this:

```javascript
///  hi, this needs work!
///  vertices = ['Thor', 'Freya', 'Odin', 'Loki']
  V = [1,2,3,4,5,6]
  E = [ [1,2], [2,3], [3,1], [3,4], [4,5], [4,6] ]
  
  parents  = function(x) { return E.reduce( function(acc, e) { return (e[1] === x) ? acc.concat(e[0]) : acc }, [] )}
  children = function(x) { return E.reduce( function(acc, e) { return (e[0] === x) ? acc.concat(e[1]) : acc }, [] )}
```

Now we can say something like ```children(children(children(parents(parents(parents('Thor'))))))```. It reads backwards and has a lot of silly parens [does eveyone know "parens"?  it took me a moment to parse], but is otherwise pretty close to what we wanted. Take a minute to look at the code. Can you see any ways to improve it?

Well, we're treating the edges as a global variable, which means we can only ever have one database at a time using these helper functions. That's pretty limiting. 

We're also not using the vertices at all. What does that tell us? It implies that everything we need is in the edges array, which in this case is true: the vertex values are scalars, so they exist independently in the edges array. If we want to answer questions like "How many of Freya's descendants were Valkyries?" we'll need to add more information to the vertices, which means making them compound values, which means the edges array should reference them by pointer instead of copying the value.

The same holds true for our edges: they contain an 'in' vertex and an 'out' vertex [footnote1], but no elegant way to incorporate additional information. We'll need that to answer questions like "How many stepparents did Loki have?" or "How many children did Odin have before Thor was born?"

You don't have to squint very hard to tell that the code for our two selectors looks very similar, which suggests there's a deeper abstraction from which those spring [diff eq]. 

Do you see any other issues?



[footnote1]
  Notice that we're modeling edges as a pair of vertices. Also notice that those pairs are ordered, because we're using arrays. That means we're modeling a *directed graph*, where every edge has a starting vertex and an ending vertex. [lines have arrows.] Doing it this way adds some complexity to our model because we have to keep track of the direction of edges, but it also allows us to ask more interesting questions, like "which vertices point in to vertex 3?" or "which vertex has the most outgoing edges?". [footnote2] [footnote in a footnote? Can you get away with that if you're not David Foster Wallace?]

[footnote2]
  If needed we can model an undirected graph by doubling up our edges, but it can be cumbersome to simulate a directed graph from an undirected one. So the directed graph model is more versatile in this context.  [I'm usually good at following the code, at least a bit.  I don't see how the code below relates to this footnote.]
```
    function undirectMe (edges) 
      { return edges.reduce( function(acc, edge)
        { acc.push([ edge[1], edge[0] ]) }, edges.slice() )}
```


### Solve some problems

Let's solve a few of the problems we've discovered. Having our vertices and edges be global constructs limits us to one graph at a time, but we'd like to have more. To solve this we'll need some structure. Let's start with a namespace.

```javascript
Dagoba   = {}                                                     // the namespace
```

We're also going to want some graph things. We can build these using a classic OOP pattern, but JavaScript offers us prototypal inheritance, which means we can build up a prototype object -- we'll call it Dagoba.G -- and then instantiate copies of that using a factory function. An advantage of this approach is that we can return different types of objects from the factory, instead of binding the creation process to a single class constructor. So we get some extra flexibility for free.

```javascript
Dagoba.G = {}                                                     // the prototype

Dagoba.graph = function(V, E) {                                   // the factory
  var graph = Object.create( Dagoba.G )
  graph.vertices = []                                             // fresh copies so they're not shared
  graph.edges = []
  graph.vertexIndex = {}
  if(V && Array.isArray(V)) graph.addVertices(V)                  // arrays only, because you wouldn't
  if(E && Array.isArray(E)) graph.addEdges(E)                     // call this with singular V and E
  return graph
}
```

We're calling addVertices and addEdges. Let's define those now.

```javascript
Dagoba.G.addVertices = function(vertices) { vertices.forEach(this.addVertex.bind(this)) }
Dagoba.G.addEdges    = function(edges)    { edges   .forEach(this.addEdge  .bind(this)) }
```

Okay, that was too easy. Those are just convenience methods for calling addVertex and addEdge in a list context.

```javascript
Dagoba.G.addVertex = function(vertex) {
  if(!vertex._id) 
    vertex._id = this.vertices.length+1                           // auto-incrementing id
  this.vertices.push(vertex)
  this.vertexIndex[vertex._id] = vertex                           // a fancy index thing
  vertex._out = []; vertex._in = []                               // empty arrays of edges
}
```

So that's pretty simple too. We actually only need the first three lines for now -- the rest are just optimizations that we'll talk about later.

```javascript
Dagoba.G.addEdge = function(edge) {
  if(!edge._label) return false                                   // labels are mandatory
  edge._in  = this.findVertexById(edge._in)
  edge._out = this.findVertexById(edge._out)
  if(!(edge._in && edge._out)) return false                       // something is missing
  edge._out._out.push(edge)
  edge._in._in.push(edge)                                         // the edge's vertex's edges [I find myself wondering if you mean "edge's vertices' edges", but can't follow the code well enough...]
  this.edges.push(edge)
}
```

Mostly more optimizations -- the last line is where all the action is. Take the edge, add it to our graph's list of edges: easy peasy lemon squeezy.

### Vertices are fine but how do we ask questions?





## Make it right

So we've clearly got some work to do. Let's start with an issue we didn't highlight above: 



### Laziness

[Suppose you've got a hobby, like tennis or philately. I've got a hobby: re-building genealogy. Can we find anyone who likes both our hobbies? ]

Suppose we'd like to find a few people who have both tennis and philately as a hobby. We can start from tennis, go out to its hobbyists, out again to their hobbies, back in from philately and then take a few. (change this query)

```
G.v('tennis').in().as('person').outV('philately').in().matches('person').take(5)
```

And this works great, until our graph gets large and we run out of memory before Dave Brubeck gets to play [huh?  Is this the sign things are devolving?]. The problem is that we're completing each query segment before passing the data along to the next one, so we end up with an immense amount of data. If there were a way for later segments to pull data from earlier segments instead of having it pushed in to them, that would solve this problem. In other words, we need lazy evaluation. 

Now in some languages that would be trivial, but JavaScript is an eager language, so we're going to have to do a little work. First of all, our query segments can't just return data any more. Instead we're going to need to process each segment on demand. That means we'll need some way of driving this process forward. Here's how we'll do that.

---> also: don't blow your stack on large queries (so we can't use straight recursion, need to use a different approach. lots of other benefits to this, like history and reversibility etc)


// introduce the driver loop

We'll need to modify our query components to work within this new system. This is moderately straightforward:

```javascript
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
  var done = -1                                                   // behind which things have finished
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
```

// new query components

Cool, now we can query without blowing our memory limits and wasting a bunch of cycles on unneeded computation. As a side benefit, we can build a new query component that allows us to progressively take more elements until we get all that we need:

```javascript
Dagoba.Q.add = function(list) {                                  // add a new traversal to the query
  this.program.push(list)
  return this
}

Dagoba.addQFun = function(name, fun) {                            // add a new traversal type
  Dagoba.QFuns[name] = fun
  Dagoba.Q[name] = function() { return this.add([name].concat([].slice.apply(arguments))) } 
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
```

// state

Notice that we're keeping all the component state up at the driver loop level. This allows us to keep track of all the state in one place so we can easily read it and clear it.  And it means we can keep the components as "pure" functions that take some inputs and give some output, so the driver loop doesn't need to instantiate them or indeed know anything about them. It's tidier too, because everything we need for re-running the query is contained in pc, program and state.

So why the scare quotes around pure? Because we're mutating the state argument inside the function. We usually try to avoid mutation to curb spooky action at a distance, but in this case we're taking advantage of JS's mutable variables. This state variable is only changed within the component itself, so if we're careful to respect this within any state-peeking features we add later, then hopefully we can avoid having this bite us. The advantages are that we can simplify our return value and cut down on garbage created in the components.

// new pipe thing

We can even resume this after coming back in from an asynchronous event, so we can use external sources to drive our querying. 

// example

```javascript
Q = G.v(1).out().property('name').take(1)

Q.run() // ["lop"]
Q.run() // ["josh"]
Q.run() // ["vadas"]

Q.run() // []
```



/// anyway later we'll include stepparents, but w/ parents ALWAYS older than children (DAG). then open it up for general graph stuff after.

```
/// (this is old, and may get skipped entirely)
/// How do we get laziness in JS? Well, we're using ES6 features, so generators come to mind. That could look like this: []
/// -- good idea, but: can't go backward, can't orthopt, 
/// --> query transformer: id() -> attr('_id')
/// --> then we add label matching for easy edge walking
/// --> then we add object queries for more complicated stuff
```


### Updates

(maybe lead in from the async stuff above, since that provides the context for concurrency issues.)

There's a problem with our 'out' query component: if someone deletes an edge we've visited while we're in the middle of a query, we'll skip a different edge because our counter is off. We could lock the vertices in our query, but one of the strengths of this approach is driving the iteration through the query space from code, so our query object might be long-lived. Even though we're in a single-threaded event loop, our queries can span multiple asynchronous re-entries, which means concurrency concerns like this are a very real problem. 
So instead we'll slice and pop the edge list each time we reach a new vertex. This burns some extra CPU and pushes more work onto the GC, so we'll stick a note here so we know what to do if this shows up as a hotspot during our profiling.

// new 'out' query component (and friends)

One concern with doing our queries this new way is that we're still not seeing a completely consistent chronology. Skipping random edges, like we did before, leaves us with an entirely inconsistent view of the universe, where things that have always existed may appear to be gone. This is generally undesirable, though many modern systems for storing very large amounts of data have exactly this property. [N] [google, facebook, kayak, etc -- often queries over heavily sharded datasets or multiple apis with pagination or timeouts have this property]

The change we just made means we will always traverse every edge a particular vertex had _at the moment we visited it_. That means that as we begin traversing edges, we may see new vertices at different points in the graph chronology, and may even see the same vertex at different points in the chronology at different points in our query. Depending on the relationships we're storing, this may provide a view of the universe that seemingly defies the laws of physics, even though no laws have actually been broken. 

If we need to see the world as it exists at a particular moment in time (e.g. 'now', where now is the moment our query begins) we can change our driver loop and the update handlers to add versioning to the data, and pass a pointer to that particular version into the query system. Doing this also opens the door to true transactions, and automated rollback/retries in an STM-like fashion. 

```
/// What happens if someone grabs some data while someone else updates some data? What if two folks update at the same time? 
/// - immutable data (crypto sig of hash of data a la puffs)
///   - weak map for bonus data
///   - immutable lib, query over time

/// If we want to treat the data we receive as immutable we have a couple of choices: JSON/clone or hand out references to graph data but clone on change (update-on-write). The latter requires we trust our users not to mess with our data, or maybe we can "Freeze" it to keep them from changing it. In ES6 we can use Proxy Objects for this [maybe]. 
/// Even doing this doesn't free us from other concurrency concerns, though: if multiple write[s?] come in from different places, what happens? Last write wins? Or do we require a reference to the previous object (Clojure's atoms), otherwise fail / retry (retry case could be like STM)? Do we lock nodes that are undergoing a transaction? What is a transaction in this context anyway? [do once/queue/later help here?]
```

Generational queries -- add a 'gen' param to everything, and only query things with a gen lower than the query's gen. [what about updates and deletes?]

```
topological ordering (necessary for scoring, because of the dependencies): 
G.v().noOut('parent').as('x').outAllFull('parent').merge('x').take(1)
// outAllFull -> outAll -> outAllN() -> outAllN(0) -> outAllN(0, 'parent')
--> or bind/spread
--> cache: G.v().noOut('parent'): test each new V... (oh, but also each changed V? if you add an edge it might change *everything*)
G.X.noOut('parent') <-- X contains 'static' methods
G.v(1).p(G.X.out().in(), G.Y.all, G.Y.times(5)) 
G.v(1).inN(5) --> G.v(1).p(G.X.in(), G.Y.times(5))
so we can bind query segments and manipulate them
can we do this with js instead? er, like, what does G.Y.times(5) return? f(query-segments)? is it just a transformer?
because that's pretty interesting. orthopt -> transformers (also debug, rollback, etc) -> transformers as query abstractors

diff between G.Y.all().times(5) and [G.Y.all(), G.Y.times(5)]

G.v().outDegree(5, 'parent')
```

/////////

out -> outN -> outNAll

Cross your eyes and squint a little and the similarities are obvious. It's good policy in general to take a cross-eyed squinty first pass of new code. You might end up with a nickname like ol' cross-eyed McSquintyPants, but no one will question your deduplication prowess. 

Spotting this similarity leaves us at a bit of a dilemma, however. [on the horns of]. There's a tension at play between wanting to not repeat ourselves and our desire to maintain the [isolation] of one-fun-per-component. 

Unfortunately there's no hard and fast rule for this situation -- it really comes down to good judgement. If the internals are likely to mutate away from each other, leave them separate. If maintaining the one-fun-per-component [invariant] is structurally useful, leave it. If you find yourself endlessly repeating the same chunk of boilerplate with small, easily-parametrizable variations, consolidate it -- but in the absence of such strong indicators you'll need to follow your nose.

Another approach we could take is leaving the individual functions, but pull 'component' out of their bodies and squish 'em together. This seems like pure win, but there are tradeoffs here as well. We're introducing a layer of abstraction, which always ups the complexity cost. We're moving the guts of the component elsewhere, forcing us to hunt to find its meaning. 

Developing abstraction barriers that minimize overall code complexity is a subtle art, but one well worth mastering. 

Let's take another look at these functions. What would it take to parametrize them?

[list of params]

This actually seems kind of painful to parametrize fully. Let's take a step back and try a different tack instead. What if we make some helper functions that allow us to treat any single function this way? [downsides] [sequence of ops -- transformers]

So ultimately we can take any component (verb) and n-ize it (adverb) and any sequence of verbs (out-out-in) and 'all' them (or any other adverb, even n-ize) 

```
g.v(1).in().out().in().out().in().out()
g.v(1).st().in().out().et().nize(3)
g.v(1).st('a').in().out().et('a').nize(3)

g.v(1)
  .start()
    .start('a')
      .in()
      .out()
    .end('a')  <-- done three times
    .nize(3)
               <-- .all() inserted here, because st--et+adverb is a single verb clause
    .out()     
               <-- and inserted here, after the next verb clause
  .end()
  .all()

so G.v(1).start().in().out().end().all().nize(2) becomes
   G.v(1).in().all() .out().all() .in().all() .out().all() 
because the all() adverb cracks open the start-end wrapper and injects all() inside it, then nize doubles the verb phrase

but G.v(1).start().in().out().end().nize(2).all() becomes
    G.v(1).in().out().all() .in().out().all() 
because nize creates a new verb phrase wrapper like [[in,out], [in,out]] and the all is injected at the outermost level.

actually since adverbs are always 'end' nodes, we really just need start nodes. and they don't actually need labels because the start always correlates with the closest unbound adverb. (like matched parens, with implicit matching at the beginning of the query for unmatched adverbs)

G.v(1).s().in().out().nize(2).all()

And then we can use orthopt to push .out().out().out() -> .outN(3) for efficiency (if it's actually more efficient)
and likewise for .out().all().out().all() -> .outAllN(3)
so we get to eat our cake and have it fast too.
```




[this is how far I've made it so far!]

### Hooks

Uniqueness of results vs counting # of results -- both are valid use cases. Can we solve with query component for counting plus hook for uniqueness?

(Maybe merge this section with updating, because half the hooks involve cloning)


### error handling

What if we did a query like this:
```
> G.v(-23).out([2,3,4]).take('a').foo()
```

Maybe we should handle those errors a little better.

### tests

- tests for components
- end-to-end tests

### serialization

---> transit as an option for json serialization over the wire; 



### persistence

Persistence is usually one of the trickier parts of a database: disks are horridly slow, but relatively safe. Basthing writes, making them atomic, journaling -- these are all difficult things to get right. 

Fortunately, we're building an IN-MEMORY database, so we don't have to worry about any of that! We may, though, occasionally want to save a copy of the db locally for fast restart on page load. We can use our serialize / deserlizer together with localStorage to do just that. 

// is localhost really atomic/secure? if not build it ourselves (at cost of storage/2); storing in localhost is crowded, often not the best option (filewriter?)


### OO vs FP re GDB wrt Dagoba (extensibility, pipeline management, etc)


## Make it fast

All production graph databases share a very particular performance characteristic: graph traversal queries are constant time with respect to total graph size. [N] [The fancy term for this is "index-free adjacency".] In a non-graph database, asking for the list of someone's friends can require time proportional to the number of entries, because in the naive case you have to look at every entry. The means if a query over ten entries takes a millisecond then a query over ten million entries will take almost two weeks. Your friend list would arrive faster if sent by Pony Express! [N] [Though only in operation for 18 months due to the arrival of the transcontinental telegraph and the outbreak of the American Civil War, the Pony Express is still remembered today for delivering mail coast to coast in just ten days.]

// do a perf test with the O(n) graph
```
Dagoba.Graph.findOutEdges = function(vertex) { return this.edges.filter(function(edge) {return edge._out == vertex._id} ) }
Dagoba.Graph.findInEdges = function(vertex) { return this.edges.filter(function(edge) {return edge._in == vertex._id} ) }
```

To alleviate this dismal performance most databases index over oft-queried fields, which turns an O(n) search into an O(log n) search. This gives considerably better search time performance, but at the cost of some write performance and a lot of space -- indices can easily double the size of a database. Much of the modern DBA's role lies in carefully balancing these time/space tradeoffs and tediously tending to the index garden, weeding out unneeded indices and adding new ones when necessary. [replace this]

// add an index
// do a perf test with an indexed graph
```
Dagoba.Graph.findOutEdges = function(vertex) { return this.outEdgeIndex[vertex._id] }
Dagoba.Graph.findInEdges = function(vertex) { return this.inEdgeIndex[vertex._id] }
```


Graph databases sidestep this issue by making direct connections between vertices and edges, so graph traversals are just pointer jumps: no need to read through everything, no need for indices. Now finding your friends has the same price regardless of total number of people in the graph, with no additional space cost or write time cost. One downside to this approach is that the pointers work best when the whole graph is in memory on the same machine. Sharding a graph across multiple machines is still an active area of research. [N] [point to a couple graph theory papers on NP-completeness of optimal splitting, but also some practical takes on this that mostly work ok]

// add pointers
// perf test
// wow such perform
```
Dagoba.Graph.findOutEdges = function(vertex) { return vertex._out; }
Dagoba.Graph.findInEdges = function(vertex) { return vertex._in; }
```

### Orthogonal Optimization

We've improved our performance for large graphs by several dozen orders of magnitude. That's pretty good, but we can do better. Each step in our query has a fixed cost for building the gremlins and making the function calls, as well as a per-step cost. Because we're splitting each step out into its own separate unit, those per-step costs can be quite high compared to what they could be if we could combine some steps. We've sacrificed performance for code simplicity. 
Many will argue that this sacrifice is acceptable, and that simplicity should trump performance whenever possible, but this is a false dichotomy. We can have our simple, easily understood model and also gain the performance benefits of combining steps -- we just have to beef up our compiler a little. 
How do we do that without sacrificing simplicity? By making the new compilation steps orthogonal to our existing model. We already have a working system, so keep that in place. But let's add some new stepflavours, and some new preprocessors that we can turn on to pull those flavours in to our pipeline. We'll tag the preprocessors so we can turn them all on or off easily. 

// ok build that
// perf test it
// cool it's faster

Great, we're fast! Notice that by deliberately ignoring the chances we had to optimize early and by writing everything in as decomposed and simplistic a way as we possibly could that we've opened up opportunities for global optimizations. [more]

We should probably confirm that our optimizations don't break anything. Maybe we can write a bunch of tests for each of these new pieces, like we did before. 

// write a test or two

This is *really* boring... and error prone. And it doesn't reveal any of the unexpected ways that combinations of optimizations may interact, or how the preprocessor affects that. Ok, new plan: probabilistic testing!
Because we have a working, well-tested pipeline and because we've created our optimizations as an optional, orthogonal pipeline we can now write tests that take advantage of this fact.

// build a random graph
// build a random query
// compare opt and non-opt flavours

We've created a (very specialized) version of quickcheck [N] in 9 lines of code! Now we just let this run for awhile whenever we add new optimizations and it will tell us when they fail.

## Wrapping up

So what have we learned? Graph databases are great for storing interconnected [Z] data that you plan to query via graph traversals. Adding laziness allows for a fluent interface over queries you could never express in an eager system for performance reasons, and allows you to cross async boundaries. Time makes things complicated, and time from multiple perspectives (i.e. concurrency) makes things very complicated, so whenever we can avoid introducing a temporal dependence (e.g. mutable state, measurable effects, etc) we make reasoning about our system easier. Building in a simple, decoupled and painfully unoptimized style leaves the door open for global optimizations later on, and using a driver loop allows for orthogonal optimizations -- each without introducing the brittleness and complexity into our code that is the hallmark of most optimization techniques. 

That last point can't be overstated: keep it simple. Eschew optimization in favor of simplicity. Work hard to achieve simplicity by finding the right model. Explore many possibilities. The chapters in this book provide ample evidence that highly non-trivial applications can have a small, tight kernel. Once you find that kernel for the application you are building, fight to keep complexity from polluting it. Build hooks for attaching additional functionality, and maintain your abstraction barriers at all costs. Using these techniques well is not easy, but they can give you leverage over otherwise intractable problems. 









[Z] Not *too* interconnected, though -- you'd like the number of edges to grow in direct proportion to the number of vertices. In other words the average number of edges a vertex has shouldn't vary with the size of the graph. Most systems we'd consider putting in a graph database already have this property: if we add 100,000 Nigerian films to our movie database that doesn't increase the degree of the Kevin Bacon vertex.




Adverbs: 
we need to get collect gremlins from 'all' steps without having them modified by interleaving traversal steps.
so we need a way to get the gremlin from the current 'all' step up to the last 'all' step. 
we could teleport it there, but 
- can we only teleport forward in time? backwards makes loops, which really cranks up the complexity of our queries
- marking the teleportation endpoint and the individual steps that can send to it causes a lot of cross-step connections, which reduces our compositional / transformational abilities
so instead we could wrap the gremlin in a bubble and float it downstream until it hits an allbuster that's wired to pop it. do they pop anything? anything from the same type of step? or is it labeled? you could miss one then, which is weird. 
and the gremlin actually does have to pass through everything eventually, triggering it as it goes -- so you really need two gremlins, a bubble one and a non-bubbled one after it. the 'all' could do that, though, by cloning it. how do you bubble? how do you pop? how do you bypass bubbles?










======================================================================================================

What follows is detritus in need of cleaning.

======================================================================================================



  
  /* 
      new idea: 
      - start with last component (collector)
      - it provides a 'pull' request as its return value
      - driver loop goes to previous component:
        - it might 'push' results
        - it might make a 'pull' request
        - it might be 'done'
  
      a query with a take(1) at the end provides one result at a time: you can keep calling it to get more results
      - this requires the take component to update itself on re-query: does it only provide 'done' once? 
        - once anything is 'done' everything above it is probably done also, so there's no backtracking?
        - it'd be better to toggle this on re-query: downstream components might be funky.
        - track it in the query? a list of 'done' components: don't even ask them.
      
      intersection/backtracking/etc... gremlin memory: it's not the vertex that arrives at the end, it's the gremlin.
      if we want history/state/replay then add that as a query stack modifier (inject between each queue item)
      likewise with gremlin collision (merging): query-inject it into the queue after collision-capable components
      
      (really query-injectors and query-transformers are the same: they're all just transformers. 
       can be run ad hoc, or added to the default queue-transformer list. [ordering? probably priority numbers... but proper dependencies / pre-pendencies (has to run before) would be better (a before/after list might be easy (but break cycles))])
  */
  
  

---  Suppose you've been asked to create a booking system for an airline. [Toronto to Timbuktu...]
---  
---  Let's start with a simple question: how many connected components are there in the Lagarias arithmetic derivative of the first N numbers out to k steps? 
---  [N] [https://cs.uwaterloo.ca/journals/JIS/VOL6/Ufnarovski/ufnarovski.pdf,
---  http://christopherolah.files.wordpress.com/2011/04/arithmetic_derivative_250.png]



Or we could ask about the shortest path from one vertex to another:

  function shortestPath (node) {}

Let's run some tests! 

...

maybe we load in the collatz sequence as a test graph? http://xkcd.com/710/ [it's a tree, less interesting]
prime factors graph. [it's too connected, perhaps less interesting]
random graph (add 1-10, then each new node connects to 3 existing nodes selected at random) [just right (goldilocks graph)]
random graph, show threshold for connectivity (cite proof etc)
Watts-Strogatz small-world model simulator
Barabási–Albert model
Dorogovtsev and Mendes : http://arxiv.org/pdf/1007.4031v1.pdf

----


// TODO: change all node -> vertex   (/sigh)
// can you put the _id in the node/edge? does addNodes return a list of ids or a list of nodes? 
// property graph model

var g = Dagoba.graph()
g.addNodes([ {_id:1, name:"Fred"}, {_id:2, name:"Bob"}, {_id:3, name:"Tom"}, {_id:4, name:"Dick"}, {_id:5, name:"Harry"} ])
g.addEdges([ {_inV: 1, _outV: 2, _label: "father"}, {_inV: 2, _outV: 3, _label: "father"}, {_inV: 2, _outV: 4, _label: "father"}, {_inV: 2, _outV: 5, _label: "father"} ])

g.v(1).out().out('father').name    // TODO: check proxies for this -- can we use them? otherwise...
g.v(1).out().out('father').attr('name')
g.v(1).out().out('father').attr('name').paths()

so each of these g.xxx things is actually just adding elements to a query list, which can be optimized when something calls it and everything happens lazily. how do we do that?


// function incoming (node) { return edges.reduce(function(edge) {if (edge[1] === node) acc.push(edge[0]) }, [])}
// function outgoing (node) { return edges.reduce(function(edge) {if (edge[0] === node) acc.push(edge[1]) }, [])}
// function neighbors (node) { return incoming(node).concat(outgoing(node)) }

//  I have quite a few more features to add and I want a debugger/visualizer/history thing 
//  and I want to convert it all to ES6 but this is where it's at right now.


make it right:

there's a problem here though -- it's really cumbersome to combine these questions. What if we wanted the nearest neighbor with a foo of bar?

use filters to create subgraphs...

a fluent style for creating subgraphs...

modularlize it so you can have multiple graphs and a nice api...

and allow query filters (and later optimizers) to be added at runtime...

non-enumerable properties for storing edges on nodes?

http://christopherolah.wordpress.com/2011/04/11/arithmetic-derivative-graph-and-thoughts/



-------------------  
  
  
  
a relational db of companies and people, with info on when each person worked at which company in a join table. ok, but now we want to know every person Joe Smith has worked with. duhn duhn duhn.


hercules.out('depictedIn').as('movie').out('hasActor').out('role').retain(hercules).back(2).out('actor').as('star').selected({ it:name })

--> note: back, as, selected, and edges only have their name instead of real properties

collision detection?
dags for parsing and rule-based systems and data flow etc

GraphSON https://github.com/tinkerpop/blueprints/wiki/GraphSON-Reader-and-Writer-Library

graph query language vs graph traversal language

Kahn process networks, tinkerpop pipes, gremlin and pacer, 

later: GraphML import/export support, https://github.com/lambdazen/pixy/wiki , 




step through debugger, reversible, 


updates:
- mutate in place
- remove and re-add
issues: 
- performance (mutate-in-place is a bit faster -- for large updates this probably matters)
- remove and re-add still has to connect to all the old pointers by _id: otherwise the old edges fail
  - also the only (natural) way to add loops and be a full graph
- normal issues with mutate-in-place apply: shared mutable state plus concurrency -> asplode
- but if we hand out references to the real nodes other things can mutate them, even if we don't

so: let's mutate-in-place for perf and simplicity, but never hand out refs to prevent mutation issues. it's a little slower because we have to serialize (or at least deep clone) before we hand over the results, but that part happens in a callback that we can cancel if our application doesn't need it.


Definitions



concurrency. immutable data structures. update syntax -- no problem, can't update directly anyway. {'foo.baz.smile': 123, 'args.lala.gogo': 333}




============================================================================

