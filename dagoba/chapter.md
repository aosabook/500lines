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

But how do we extend that to grandparents? We need to do a subquery, or use some other type of vendor-specific extension to SQL. And by the time we get to second cousins once removed we're going to have ALOTTA SQL.

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

Now we can say something like ```children(children(children(parents(parents(parents('Thor'))))))```. It reads backwards and has a lot of silly parens, but is otherwise pretty close to what we wanted. Take a minute to look at the code. Can you see any ways to improve it?

Well, we're treating the edges as a global variable, which means we can only ever have one database at a time using these helper functions. That's pretty limiting. 

We're also not using the vertices at all. What does that tell us? It implies that everything we need is in the edges array, which in this case is true: the vertex values are scalars, so they exist independently in the edges array. If we want to answer questions like "How many of Freya's descendants were Valkyries?" we'll need to add more information to the vertices, which means making them compound values, which means the edges array should reference them by pointer instead of copying the value.

The same holds true for our edges: they contain an 'in' vertex and an 'out' vertex [footnote1], but no elegant way to incorporate additional information. We'll need that to answer questions like "How many stepparents did Loki have?" or "How many children did Odin have before Thor was born?"

You don't have to squint very hard to tell that the code for our two selectors looks very similar, which suggests there's a deeper abstraction from which those spring [diff eq]. 

Do you see any other issues?



[footnote1]
  Notice that we're modeling edges as a pair of vertices. Also notice that those pairs are ordered, because we're using arrays. That means we're modeling a *directed graph*, where every edge has a starting vertex and an ending vertex. [lines have arrows.] Doing it this way adds some complexity to our model because we have to keep track of the direction of edges, but it also allows us to ask more interesting questions, like "which vertices point in to vertex 3?" or "which vertex has the most outgoing edges?". [footnote2] [footnote in a footnote? Can you get away with that if you're not David Foster Wallace?]

[footnote2]
  If needed we can model an undirected graph by doubling up our edges, but it can be cumbersome to simulate a directed graph from an undirected one. So the directed graph model is more versatile in this context. [maybe explain this better]
```
    function undirectMe (edges) 
      { return edges.reduce( function(acc, edge)
        { acc.push([ edge[1], edge[0] ]) }, edges.slice() )}
```


## Solve some problems

Let's solve a few of the problems we've discovered. Having our vertices and edges be global constructs limits us to one graph at a time, but we'd like to have more. To solve this we'll need some structure. Let's start with a namespace.

```javascript
Dagoba = {}                                             // the namespace
```

We'll use an object as our namespace. An object in JavaScript is mostly just an unordered set of key/value pairs. We only have four basic data structures to choose from in JS, so we'll be using this one a lot.

Now we need some some graphs. We can build these using a classic OOP pattern, but JavaScript offers us prototypal inheritance, which means we can build up a prototype object -- we'll call it Dagoba.G -- and then instantiate copies of that using a factory function. An advantage of this approach is that we can return different types of objects from the factory, instead of binding the creation process to a single class constructor. So we get some extra flexibility for free.

```javascript
Dagoba.G = {}                                           // the prototype

Dagoba.graph = function(V, E) {                         // the factory
  var graph = Object.create( Dagoba.G )

  graph.edges       = []                                // fresh copies so they're not shared
  graph.vertices    = []
  graph.vertexIndex = {}                                // lookup optimization (explained later)
  
  graph.autoid = 1                                      // an auto-incrementing id counter
  
  if(Array.isArray(V)) graph.addVertices(V)             // arrays only, because you wouldn't
  if(Array.isArray(E)) graph.addEdges(E)                //   call this with singular V and E
  
  return graph
}
```

We'll accept two optional arguments: a list of vertices and a list of edges. Then we'll create a new object that has all of our prototype's abilities and none of its weaknesses. We create a brand new array (one of the other basic JS data structures) for our edges, another for the vertices, and a new object for something called vertexIndex -- more on that later.

Then we're calling addVertices and addEdges from inside our factory, so let's define those now.

```javascript
Dagoba.G.addVertices = function(vertices) { vertices.forEach(this.addVertex.bind(this)) }
Dagoba.G.addEdges    = function(edges)    { edges   .forEach(this.addEdge  .bind(this)) }
```

Okay, that was too easy -- we're just passing off the work to addVertex and addEdge. We should define those now too.

```javascript
Dagoba.G.addVertex = function(vertex) {                 // accepts a vertex-like object, with properties
  if(!vertex._id)
    vertex._id = this.autoid++
  else if(this.findVertexById(vertex._id))
    return Dagoba.error('A vertex with that id already exists')
    
  this.vertices.push(vertex)
  this.vertexIndex[vertex._id] = vertex                 // a fancy index thing
  vertex._out = []; vertex._in = []                     // placeholders for edge pointers
  return vertex._id
}
```

We assign the vertex an _id property if it doesn't already have one, and cast it out if it's a duplicate. Then we push it into our graph's list of vertices, add it to the vertexIndex for efficient lookup by _id, and add two additional properties to it: _out and _in, which will both become lists of edges. [Footnote on 'list' vs 'array': we want to fulfill the list abstract data structure, and do so using an array concrete data structure.]

Note that we are using the object we're handed as a vertex instead of creating a new object of our own. If the entity invoking the addVertex function retains a pointer to the vertex they give us they can manipulate it at runtime and potentially mess things up. On the other hand, while doing a deep copy here would give us some protection from outside tampering it would also double our space usage. There's a tension here between performance and protection, and the right balance depends on your use cases.

```javascript
Dagoba.G.addEdge = function(edge) {                     // accepts an edge-like object, with properties
  edge._in  = this.findVertexById(edge._in)
  edge._out = this.findVertexById(edge._out)
  
  if(!(edge._in && edge._out)) 
    return Dagoba.error("That edge's " + (edge._in ? 'out' : 'in') + " vertex wasn't found")
  
  edge._out._out.push(edge)                             // add edge to the edge's out vertex's out edges
  edge._in._in.push(edge)                               // vice versa
  
  this.edges.push(edge)
}
```

We're using a function called 'findVertexById', which as you might guess finds a vertex by its id. If it can't find one it returns a false value. The definition is simple, since we have our handy vertexIndex:

```javascript
Dagoba.G.findVertexById = function(vertex_id) {
  return this.vertexIndex[vertex_id] 
}
```

Without vertexIndex we'd have to go through each vertex in our list one at a time to decide if it matched the id -- turning a constant time operation into a linear time one, and any O(n) operations that directly rely on it into O(n^2) operations. Ouch!

Then we reject the edge if it's missing either vertex. We'll use a helper function to log an error when this happens, so we can override its behavior on a per-application basis.

```javascript
Dagoba.error = function(msg) {
  return console.log(msg) && false
}
```

Then we'll add our new edge to both vertices' edge lists: the edge's out vertex's list of out-side edges, and the in vertex's list of in-side edges.

And that's all the graph we need for now!

## Enter the query

There's really only two parts to this system: the part that holds the graph and the part that answers questions about the graph. The part that holds the graph is pretty simple, as we've seen. The query part is a little trickier.

We'll start just like before, with a prototype and a query factory:

```javascript
Dagoba.Q = {}                                           // prototype

Dagoba.query = function(graph) {                        // factory (only called by a graph's query initializers)
  var query = Object.create( Dagoba.Q )
  
  query.   graph = graph                                // the graph itself
  query.   state = []                                   // state for each step
  query. program = []                                   // list of steps to take  
  query.gremlins = []                                   // gremlins for each step

  return query
}
```

Now's a good time to introduce some new friends:

A *program* is a series of steps. Each step is like a pipe in a pipeline -- a piece of data comes in one end, is transformed in some fashion, and goes out the other end. Our pipeline doesn't quite work like that, but it's a good first approximation. 

Each step in our program can have *state*, and query.state is a list of per-step state that index correlates with the list of steps in query.program. 

A *gremlin* is a creature that travels through the graph doing our bidding. They trace their heritage back to Tinkerpop's Blueprints and the Gremlin query language. They remember where they've been and allow us to find answers to interesting questions. 

Remember that query we wanted to answer? The one about Thor's second cousins once removed? We decided ```Thor.parents.parents.parents.children.children.children``` was a pretty good way of expressing that. Each of the ```parents``` and ```children``` instances is a *step* in our *program*. Each step is a reference to a *pipetype*, which is just a function with a specific signature.

The actual query in our system might look like ```g.v('Thor').out().out().out().in().in().in()```. Each of the steps is a function call, and so they can take *arguments*. The interpreter passes the step's arguments and state in to the step's pipetype function, along with a gremlin from the previous step, if there was one.

We'll need a way to add steps to our query. Here's a helper function for that:

```javascript
Dagoba.Q.add = function(pipetype, args) {               // add a new step to the query
  var step = [pipetype, args]
  this.program.push(step)                               // step is an array: first the pipe type, then its args
  return this
}
```

Each step is a composite entity, combining the pipetype function with the arguments to apply to that function. We could combine the two into a partially-applied function at this stage, instead of using a tuple [data structure footnote], but then we'd lose some introspective power that will prove helpful later. 

## The problem with being eager

Before we look at the pipetypes themselves we're going to take a slight diversion into the exciting world of execution strategy. There are two main schools of thought: the Call By Value clan, also known as eager beavers, strictly insist that all arguments be evaluated before the function is applied. They are opposed by the Call By Needians, are content to procrastinate until the last possible moment before doing anything, and even then do as little as possible -- they are, in a word, lazy.

JavaScript, being a strict language, will process each of our steps as they are called. Hence we would expect the evaluation of ```g.v('Thor').out().in()``` would first find the Thor vertex, then find all vertices connected to it by outgoing edges, and from each of those vertices finally return all vertices they are connected to by inbound edges.

And in a lazy language we would get the same result -- the execution strategy doesn't make much difference here. But what if we added a few additional calls? Given how well-connected Thor is, our ```g.v('Thor').out().out().out().in().in().in()``` query may produce many results -- in fact, because we're not limiting our vertex list to unique results, it may produce many more results than we have vertices in our total graph. That could set us back a bit.

We're probably only interested in getting a few unique results out, so we'll change the query a little: ```g.v('Thor').out().out().out().in().in().in().unique().take(10)```. Now we'll only get at most 10 results out. What happens if we evaluate this strictly, though? We're still going to have to build up septillions of results before returning only the first 10.

And so we see the need for laziness: all graph databases have to support a mechanism for doing as little work as possible, and most choose some form of lazy evaluation to do so. Since we're building our own interpreter, evaluating our program lazily is certainly within our purview. But the road to lazyville is paved with good intentions and surprising consequences.

[
ALSO:
    ### Laziness

    [Suppose you've got a hobby, like tennis or philately. I've got a hobby: re-building genealogy. Can we find anyone who likes both our hobbies? ]

    Suppose we'd like to find a few people who have both tennis and philately as a hobby. We can start from tennis, go out to its hobbyists, out again to their hobbies, back in from philately and then take a few. (change this query)

    ```
    G.v('tennis').in().as('person').outV('philately').in().matches('person').take(5)
    ```

    And this works great, until our graph gets large and we run out of memory before Dave Brubeck gets to play. The problem is that we're completing each query segment before passing the data along to the next one, so we end up with an immense amount of data. If there were a way for later segments to pull data from earlier segments instead of having it pushed in to them, that would solve this problem. In other words, we need lazy evaluation. 

    Now in some languages that would be trivial, but JavaScript is an eager language, so we're going to have to do a little work. First of all, our query segments can't just return data any more. Instead we're going to need to process each segment on demand. That means we'll need some way of driving this process forward. Here's how we'll do that.

    ---> also: don't blow your stack on large queries (so we can't use straight recursion, need to use a different approach. lots of other benefits to this, like history and reversibility etc)

]


## The heart of the matter

Putting var declarations at the top is like eating desert before your entree. All the surprise is gone. Everything is revealed. And yet nothing has any meaning -- we don't know why we need them yet, they're just empty words.

[This would be a great section to build up to slowly, with fits and starts. But it's not going to be easy.]

```javascript
Dagoba.Q.run = function() {                             // our virtual machine for query processing

  var max = this.program.length - 1                     // last step in the program
  var maybe_gremlin = false                             // a gremlin, a signal string, or false
  var results = []                                      // results for this particular run
  var done = -1                                         // behindwhich things have finished
  var pc = max                                          // our program counter -- we start from the end
  
  var step, state, pipetype
  
```

So now we have a bunch of machinery in place in case we need it later. Let's find out whether we do.

```javascript
  // driver loop
  while(done < max) {
    
    step = this.program[pc]                             // step is an array: first the pipe type, then its args
    state = (this.state[pc] = this.state[pc] || {})     // the state for this step: ensure it's always an object
    pipetype = Dagoba.getPipetype(step[0])              // a pipetype is just a function
    
    maybe_gremlin = pipetype(this.graph, step[1], maybe_gremlin, state)
    
    if(maybe_gremlin == 'pull') {                       // 'pull' tells us the pipe wants further input
      maybe_gremlin = false
      if(pc-1 > done) {
        pc--                                            // try the previous pipe
        continue
      } else {
        done = pc                                       // previous pipe is finished, so we are too
      }
    }
    
    if(maybe_gremlin == 'done') {                       // 'done' tells us the pipe is finished
      maybe_gremlin = false
      done = pc
    }
    
    pc++                                                // move on to the next pipe
    
    if(pc > max) {
      if(maybe_gremlin)
        results.push(maybe_gremlin)                     // a gremlin popped out the end of the pipeline
      maybe_gremlin = false
      pc--                                              // take a step back
    }
  }

  results = results.map(function(gremlin) {             // return either results (like property('name')) or vertices
    return gremlin.result != null 
         ? gremlin.result : gremlin.vertex } )

  return results
}
```

[program counter is like a head that moves forward or backward. it's a kind of turing machine. it reads the current entry, possibly changing it (the state), and then moves either forward or backward. very simple.]

[ALSO:

    NOTE: see // state note below: keeping it in the driver loop aids instrumentation & debugging (& cloning), cuts down on per-pipe garbage, and keeps the pipetype functions "pure" in the sense that they don't keep local state, are referentially transparent [ish] and don't cause effects [ish].

    // state

    Notice that we're keeping all the component state up at the driver loop level. This allows us to keep track of all the state in one place so we can easily read it and clear it.  And it means we can keep the components as "pure" functions that take some inputs and give some output, so the driver loop doesn't need to instantiate them or indeed know anything about them. It's tidier too, because everything we need for re-running the query is contained in pc, program and state.

    So why the scare quotes around pure? Because we're mutating the state argument inside the function. We usually try to avoid mutation to curb spooky action at a distance, but in this case we're taking advantage of JS's mutable variables. This state variable is only changed within the component itself, so if we're careful to respect this within any state-peeking features we add later, then hopefully we can avoid having this bite us. The advantages are that we can simplify our return value and cut down on garbage created in the components.

]

Ah, much better! Now we see where it all fits in. That makes perfect sense!

Let's move on to something very intriguing: the pipetypes. 

```javascript
Dagoba.PipeTypes = {}                                   // every pipe has a type

Dagoba.addPipeType = function(name, fun) {              // adds a new method to our query object
  Dagoba.PipeTypes[name] = fun
  Dagoba.Q[name] = function() {
    return this.add(name, [].slice.apply(arguments)) }  // capture the pipetype and args
}

Dagoba.getPipetype = function(name) {
  var pipetype = Dagoba.PipeTypes[name]                 // a pipe type is just a function 

  if(!pipetype)                                         // most likely this actually results in a TypeError
    Dagoba.error('Unrecognized pipe type: ' + name)   // but if you do make it here you get a nice message

  return pipetype || Dagoba.fauxPipetype
}

Dagoba.fauxPipetype = function(_, _, maybe_gremlin) {   // if you can't find a pipe type 
  return maybe_gremlin || 'pull'                        // just keep things flowing along
}
```

See those underscores? We use those to label params that won't be used in our function. Most other pipetypes will use all three params. We really only did this here to make the comments line up nicely.


## Pipetypes

Let's introduce our cast of characters.

#### Vertex

Most of the pipetypes we will meet take gremlins as their input, but this special pipetype generates new ones. Given a vertex id it will create a new gremlin on that vertex, if it exists. 

You can also give it a query, and it will find all matching vertices. It creates one new gremlin at a time until it's worked through all of them. We'll look at the findVertices function a little later.

```javascript
Dagoba.addPipeType('vertex', function(graph, args, gremlin, state) {
  if(!state.vertices) 
    state.vertices = graph.findVertices(args)           // state initialization

  if(!state.vertices.length)                            // all done
    return 'done'

  var vertex = state.vertices.pop()                     // OPT: this relies on cloning the vertices
  return Dagoba.makeGremlin(vertex, gremlin.state)      // we can have incoming gremlins from as/back queries
})
```

We first check to see if we've already gathered matching vertices. If not then we use findVertices to try to find some. 

Note that we're directly mutating the state argument here, and not passing it back. An alternative would be to return an object instead of a gremlin or signal, and pass state back that way. If JS allowed multiple return values it would make this option more elegant. 

We would still need to find a way to deal with the mutations, though, as the call site still has a reference to the original variable. Linear types would solve this by automatically taking the reference out of scope when the pipetype is called. Then we would assign it again in the call site's scope once the pipetype function returned its new version of the scope object. 

Linear types would allow us to avoid expensive copy-on-write schemes or complicated persistent data structures, while still retaining the benefits of immutability -- in this case, avoiding spooky action at a distance. Two references to the same mutable data structure act like a pair of walkie-talkies, allowing whoever holds them to communicate directly. Those walkie-talkies can be passed around from function to function, and cloned to create a whole set of walkie-talkies. This completely subverts the natural communication channels your code already possesses. In a system with no concurrency you can sometimes get away with it, but introduce multithreading or asynchronous behavior and all those walkie-talkies squawking can really be a drag. 

JS lacks linear types, but we can get the same effect if we're really, really disciplined. Which we will be. For now.

TODO: discuss OPT note
TODO: discuss incoming gremlins note


#### In-N-Out

Walking the graph is as easy as ordering a burger. These two lines set up the 'in' and 'out' pipetypes for us.

```javascript
Dagoba.addPipeType('out', Dagoba.simpleTraversal('out'))
Dagoba.addPipeType('in',  Dagoba.simpleTraversal('in'))
```

The simpleTraversal function returns a pipetype handler that accepts a gremlin as its input, and then spawns a new gremlin each time it's queried. When it's out of gremlins it sending back a 'pull' request to get a new gremlin from its predecessor. 

```javascript
Dagoba.simpleTraversal = function(dir) {
  var find_method = dir == 'out' ? 'findOutEdges' : 'findInEdges'
  var edge_list   = dir == 'out' ? '_in' : '_out'
  
  return function(graph, args, gremlin, state) {
    if(!gremlin && (!state.edges || !state.edges.length))         // query initialization
      return 'pull'
  
    if(!state.edges || !state.edges.length) {                     // state initialization
      state.gremlin = gremlin
      state.edges = graph[find_method](gremlin.vertex)            // get edges that match our query
                         .filter(Dagoba.filterEdges(args[0]))
    }

    if(!state.edges.length)                                       // all done
      return 'pull'

    var vertex = state.edges.pop()[edge_list]                     // use up an edge
    return Dagoba.gotoVertex(state.gremlin, vertex)
  }
}
```

The first couple lines handle the differences between the in version and the out version. Then we're ready to return our pipetype function, which looks quite a bit like the vertex pipetype we just saw. That's a little surprising, since this one takes in a gremlin whereas the vertex pipetype creates gremlins ex nihilo.

But we can see the same beats being hit here, with the addition of a query initialization step. If there's no gremlin and we're out of available edges then we pull. If we have a gremlin but haven't yet set state then we find any edges going the appropriate direction and add them to our state [footnote on filtering]. If there's a gremlin but its current vertex has no appropriate edges then we pull. And finally we pop off an edge and return a freshly cloned gremlin on the vertex to which it points.

Glancing at this code we see ```!state.edges.length``` repeated in each of the three clauses. It's tempting to refactor this to reduce the complexity of those conditionals. There are two issues keeping us from doing so. One is relatively minor: the third ```!state.edges.length``` means something different than the first two, since ```state.edges``` has been changed between the second and third conditional. This actually encourages us to refactor, because having the same label mean two different things inside a single function usually isn't ideal.

But this isn't the only pipetype function we're writing, and we'll see these ideas of query initialization and/or state initialization repeated over and over. There's always a balancing act when writing code between structured qualities and unstructured qualities. Too much structure and you pay a high cost in boilerplate and abstraction complexity. Too little structure and you'll have to keep all the plumbing minutia in your head.

In this case, with a dozen or so pipetypes, the right choice seems to be to style each of the pipetype functions as similarly as possible, and label the constituent pieces with comments. So we resist our impulse to refactor this particular pipetype, because doing so would reduce uniformity, but we also resist the urge to engineer a formal structural abstraction for query initialization, state initialization, and the like. If there were hundreds of pipetypes that latter choice would likely be the right one -- the complexity cost of the abstraction is constant, while the benefit accrues linearly with the number of units. When handling that many moving pieces anything you can do to enforce regularity among them is helpful.


#### Property

Let's pause for a moment to consider an example query based on the three pipetypes we've seen. We can ask for Thor's grandfathers like this: ```g.v('Thor').in('father').in('father').run()```. But what if we wanted their names? [Have we mentioned run() yet?]

We could put a map on the end of that:
```g.v('Thor').in('father').in('father').run().map(function(vertex) {return vertex.name})```

But this is a common enough operation that we'd prefer to write something more like:
```g.v('Thor').in('father').in('father').property('name').run()```

Plus this way the property pipe is an integral part of the query, instead of something appended after. This has some interesting benefits, as we'll soon see.

```javascript
Dagoba.addPipeType('property', function(graph, args, gremlin, state) {
  if(!gremlin) return 'pull'                            // query initialization
  gremlin.result = gremlin.vertex[args[0]]
  return gremlin.result == null ? false : gremlin       // undefined or null properties kill the gremlin
})
```

Our query initialization here is trivial: if there's no gremlin, we pull. If there is a gremlin, we'll set its result to the property's value. Then the gremlin can continue onward. If it makes it through the last pipe its result will be collected and returned from the query. [Gremlins without a result value return the last vertex they visited.]

Note that if the property doesn't exit we return false instead of the gremlin, so property pipes also act as a type of filter. Can you think of a use for this? What are the tradeoffs in this design decision? 


#### Unique

If we want to collect all of Thor's grandparents' grandchildren -- his cousins, his siblings, and himself -- we could do a query like this: ```g.v('Thor').in().in().out().out().run()```. That would give us many duplicates, however. In fact there would be at least four copies of Thor himself. (Can you think of a time when there might be more?)

To resolve this we introduce a new pipetype called 'unique'. Our new query ```g.v('Thor').in().in().out().out().unique().run()``` produces output in one-to-one correspondence with the grandchildren.

```javascript
Dagoba.addPipeType('unique', function(graph, args, gremlin, state) {
  if(!gremlin) return 'pull'                            // query initialization
  if(state[gremlin.vertex._id]) return 'pull'           // we've seen this gremlin, so get another instead
  state[gremlin.vertex._id] = true
  return gremlin
})
```

A unique pipe is purely a filter: it either passes the gremlin through unchanged or it tries to pull a new gremlin from the previous pipe. 

We initialize by trying to collect a gremlin. If the gremlin's current vertex is in our cache, then we've seen it before so we try to collect a new one. Otherwise, we add the gremlin's current vertex to our cache and pass it along. Easy peasy.


#### Filter

We've seen two simplistic ways of filtering, but sometimes we need more elaborate constraints. What if we wanted Thor's siblings whose weight in skippund is greater than their height in fathoms? This query would give us our answer: ```g.v('Thor').in().out().unique().filter(function(asgardian) {return asgardian.weight > asgardian.height}).run()```

[Æsir?] [depending on the density of asgardian flesh this may return many results, or none at all]

```javascript
Dagoba.addPipeType('filter', function(graph, args, gremlin, state) {
  if(!gremlin) return 'pull'                            // query initialization

  if(typeof args[0] != 'function') {
    Dagoba.error('Filter is not a function: ' + args[0]) 
    return gremlin                                      // keep things moving
  }

  if(!args[0](gremlin.vertex, gremlin)) return 'pull'   // gremlin fails filter function 
  return gremlin
})
```

If the filter's first argument is not a function we trigger an error, and then pass the gremlin along. Pause for a minute, and consider the alternatives. Why would we decide to continue the query once an error is encountered?

There are two possibilities for this error to arise. The first involves the user typing in queries, either in a REPL or in code they're actively working on. When that query is run it will produce results, but also generate an error. The user then corrects the error to filter down the set of results produced. The alternative approach for this use case would be to display the error produce and produce no results. Which of those you prefer is mostly personal preference.

The second possibility is that the filter is being applied dynamically at run time. This is a much more important case, because the person invoking the query is not necessarily the author of the query code. Because this is on the web, our default rule is to always show as much as we can, and never break things. It is usually preferable to soldier on in the face of grave tribulations rather than succumb to our wounds and present the user with a grisly error message.

For those rare occasions when showing too few results is better than showing too many, Dagoba.error can be overridden to throw an error, circumventing the natural control flow and generally breaking things. This is the path of pedants, and typically results in a horrible user experience. If you are launching missiles you should like some pedants on your team. 


#### Take

We don't always want all the results at once. Sometimes we only need a handful of results: we want a dozen of Thor's contemporaries, going all the way back to the primeval cow Auðumbla: 

```
g.v('Thor').in().in().in().in().out().out().out().out().unique().take(12).run()
```

Without the take pipe that query could take quite a while to run, but thanks to our lazy evaluation strategy the query with the take pipe is very fast.

Sometimes we just want one at a time: we'll process the result, work with it, and then come back for another one. This pipetype allows us to do that as well.

```javascript
q = g.v('Auðumbla').out().out().out().property('name').take(1)

q.run() // ["Odin"]
q.run() // ["Vili"]
q.run() // ["Vé"]
q.run() // []
```

Our query can function in an asynchronous environment, allowing us to collect more results as needed. When we run out an empty array is returned.


```javascript
Dagoba.addPipeType('take', function(graph, args, gremlin, state) {
  state.taken = state.taken || 0                        // state initialization
  
  if(state.taken == args[0]) {
    state.taken = 0
    return 'done'                                       // all done
  }
  
  if(!gremlin) return 'pull'                            // query initialization
  state.taken++
  return gremlin
})
```

We initialize ```state.taken``` to zero if it doesn't already exist. JavaScript has implicit coercion, but coerces ```undefined``` into ```NaN```, so we have to be explicit here. 

The when ```state.taken``` reaches ```args[0]``` we return 'done', sealing off the pipes before us. We also reset the ```state.taken``` counter, allowing us to repeat the query later.

We do those two steps before query initialization to handle the cases of ```take(0)``` and ```take()```, which return no results. [double check this]. Then we increment our counter and return the gremlin.


#### As

These next three pipetypes work as a group to allow more advanced queries. This one just allows you to label the current vertex. We'll use that label with the next two pipetypes.

```javascript
Dagoba.addPipeType('as', function(graph, args, gremlin, state) {
  if(!gremlin) return 'pull'                            // query initialization
  gremlin.state.as = gremlin.state.as || {}             // initialize gremlin's 'as' state
  gremlin.state.as[args[0]] = gremlin.vertex            // set label to the current vertex
  return gremlin
})
```

After initializing the query, we then ensure the gremlin's local state has an 'as' parameter. Then we set a property of that parameter to the gremlin's current vertex.

[Have we talked about gremlin local state yet?]


#### Except

We've already seen cases where we would like to say "Give me all of Thor's siblings who are not Thor". We can do that with a filter:

```g.v('Thor').in().out().unique().filter(function(asgardian) {return asgardian.name != 'Thor'}).run()```
[Test this]

It's more straightforward with 'as' and 'except':

```
g.v('Thor').as('me').in().out().except('me').unique().run()
```

But there are also queries that would be very difficult to try to filter. What if we wanted Thor's uncles and aunts? How would we filter out his parents? It's easy with 'as' and 'except':

```g.v('Thor').in().as('parent').in().out().out().except('parent').unique().run()```
[Test this... it doesn't actually work quite like that. If DAG not tree then this breaks. Better example?]
[How would you get 'pure' uncles and aunts instead of half-uncles and step-aunts? and the pure u/a's current SOs?]

```javascript
Dagoba.addPipeType('except', function(graph, args, gremlin, state) {
  if(!gremlin) return 'pull'                            // query initialization
  if(gremlin.vertex == gremlin.state.as[args[0]]) return 'pull'
  return gremlin
})
```

Here we're just checking whether the current vertex is equal to the one we stored previously. If it is, we skip it.


#### Back

Some of the questions we might ask involve checking further into the graph, only to return later to our point of origin if the answer is in the affirmative. Suppose we wanted to know which of Freya's daughters had children with one of Odin's sons? 

```g.v('Freya').out('daughter').as('me').out().in('father').in('father').filter(function(asgardian) {return asgardian == 'Odin'}).back('me').unique().run()```
[Test this]

This is really all the pipetypes we need to do some pretty serious queries. 

[Add more examples here]

```javascript
Dagoba.addPipeType('back', function(graph, args, gremlin, state) {
  if(!gremlin) return 'pull'                            // query initialization
  return Dagoba.gotoVertex(gremlin, gremlin.state.as[args[0]])
})
```

We're using the ```Dagoba.gotoVertex``` helper function to do all real work here. Let's check a look at that and some other helpers now.


## Helper functions

The pipetypes above rely on a few helper functions. We'll look at those first, and then look at some functions and features that haven't been mentioned yet.

#### Gremlins

Gremlins are simple creatures: they have a current vertex, and some local state. So to make a new one we just need to make an object with those two things.

```javascript
Dagoba.makeGremlin = function(vertex, state) {
  return {vertex: vertex, state: state || {} }
}
```

Any object that has a vertex property and a state property is a gremlin by this definition, so we could just inline the constructor, but wrapping it in a function allows us to add new properties to all gremlins in a single place.

We can also take an existing gremlin and send it to a new vertex, as we saw in the 'back' pipetype and the simpleTraversal function.

```javascript
Dagoba.gotoVertex = function(gremlin, vertex) {         // clone the gremlin 
  return Dagoba.makeGremlin(vertex, gremlin.state)
}
```

Note that this function actually returns a brand new gremlin -- a clone of the old one, sent to our desired destination. That means a gremlin can sit on a vertex and be cloned out to many other vertices, which is exactly what happens in simpleTraversal.

As an example of possible enhancements, we could add tracking for every vertex the gremlin visits, and then add new pipetypes to take advantage of those paths.


#### Filtering

We saw that simpleTraversal uses a filtering function on the edges it encounters. It's a simple function, but powerful enough for our purposes.

```javascript
Dagoba.filterEdges = function(filter) {
  return function(edge) {
    if(!filter)                                         // if there's no filter, everything is valid
      return true

    if(typeof filter == 'string')                       // if the filter is a string, the label must match
      return edge._label == filter

    if(Array.isArray(filter))                           // if the filter is an array, the label must be in it
      return !!~filter.indexOf(edge._label)

    return Dagoba.objectFilter(edge, filter)            // try the filter as an object
  }
}
```

The first case is no filter at all: ```g.v('Odin').out().run()``` traverses all out edges from Odin.

The second filters on the edge's label: ```g.v('Odin').out('son').run()``` traverses all out edges with a label of 'son'.

The third case accepts an array of labels: ```g.v('Odin').out(['daughter', 'son']).run()``` traverses both son and daughter edges.

And the fourth case uses another helper function:

```javascript
Dagoba.objectFilter = function(thing, filter) {         // thing has to match all of filter's properties
  for(var key in filter)
    if(thing[key] != filter[key])
      return false
  
  return true 
}
```

This allows us to query the edge using a filter object: ```g.v('Odin').out({position: 2, _label: daughter}).run()``` finds Odin's second daughter, if position is genderized. 


#### Performance

All production graph databases share a very particular performance characteristic: graph traversal queries are constant time with respect to total graph size. [footnote: The fancy term for this is "index-free adjacency".] In a non-graph database, asking for the list of someone's friends can require time proportional to the number of entries, because in the naive case you have to look at every entry. The means if a query over ten entries takes a millisecond then a query over ten million entries will take almost two weeks. Your friend list would arrive faster if sent by Pony Express! [footnote: Though only in operation for 18 months due to the arrival of the transcontinental telegraph and the outbreak of the American Civil War, the Pony Express is still remembered today for delivering mail coast to coast in just ten days.]

To alleviate this dismal performance most databases index over oft-queried fields, which turns an O(n) search into an O(log n) search. This gives considerably better search time performance, but at the cost of some write performance and a lot of space -- indices can easily double the size of a database. Much of the modern DBA's role lies in carefully balancing these time/space tradeoffs and tediously tending to the index garden, weeding out unneeded indices and adding new ones when necessary. [replace this]

Graph databases sidestep this issue by making direct connections between vertices and edges, so graph traversals are just pointer jumps: no need to read through everything, no need for indices. Now finding your friends has the same price regardless of total number of people in the graph, with no additional space cost or write time cost. One downside to this approach is that the pointers work best when the whole graph is in memory on the same machine. Sharding a graph across multiple machines is still an active area of research. [footnote: point to a couple graph theory papers on NP-completeness of optimal splitting, but also some practical takes on this that mostly work ok]

We can see this at work in the microcosm of Dagoba if we replace the functions for finding edges. Here's a naive version that searches through all the edges in linear time.

```
Dagoba.Graph.findOutEdges = function(vertex) { return this.edges.filter(function(edge) {return edge._out == vertex._id} ) }
Dagoba.Graph.findInEdges  = function(vertex) { return this.edges.filter(function(edge) {return edge._in  == vertex._id} ) }
```

We can add an index for edges, which gets us most of the way there with small graphs but has all the classic indexing issues for large ones.

```
Dagoba.Graph.findOutEdges = function(vertex) { return this.outEdgeIndex[vertex._id] }
Dagoba.Graph.findInEdges  = function(vertex) { return this.inEdgeIndex[vertex._id]  }
```

And here we have our old friends back again: pure, sweet index-free adjacency.

```
Dagoba.Graph.findOutEdges = function(vertex) { return vertex._out }
Dagoba.Graph.findInEdges  = function(vertex) { return vertex._in  }
```

Run these yourself to experience the graph database difference.


#### Orthogonal Optimization

We've improved our performance for large graphs by several dozen orders of magnitude. That's pretty good, but we can do better. Each step in our query has a fixed cost for building the gremlins and making the function calls, as well as a per-step cost. Because we're splitting each step out into its own separate unit, those per-step costs can be quite high compared to what they could be if we could combine some steps. We've sacrificed performance for code simplicity. 
Many will argue that this sacrifice is acceptable, and that simplicity should trump performance whenever possible, but this is a false dichotomy. We can have our simple, easily understood model and also gain the performance benefits of combining steps -- we just have to beef up our compiler a little. 
How do we do that without sacrificing simplicity? By making the new compilation steps orthogonal to our existing model. We already have a working system, so keep that in place. But let's add some new stepflavours, and some new preprocessors that we can turn on to pull those flavours in to our pipeline. We'll tag the preprocessors so we can turn them all on or off easily. 

// ok build that
// perf test it
// cool it's faster

Great, we're fast! Notice that by deliberately ignoring the chances we had to optimize early and by writing everything in as decomposed and simplistic a way as we possibly could that we've opened up opportunities for global optimizations. [more]

We should probably confirm that our optimizations don't break anything. Maybe we can write a bunch of tests for each of these new pieces, like we did before. 


#### Testing

// write a test or two

This is *really* boring... and error prone. And it doesn't reveal any of the unexpected ways that combinations of optimizations may interact, or how the preprocessor affects that. Ok, new plan: probabilistic testing!
Because we have a working, well-tested pipeline and because we've created our optimizations as an optional, orthogonal pipeline we can now write tests that take advantage of this fact.

// build a random graph
// build a random query
// compare opt and non-opt flavours

We've created a (very specialized) version of quickcheck [N] in 9 lines of code! Now we just let this run for awhile whenever we add new optimizations and it will tell us when they fail.


#### Serialization

Having a graph in memory is great, but how do we get it there in the first place? We saw that our graph constructor can take a list of vertices and edges and create a graph for us, but once those structures have been built is there any way to get them back out?

Our natural inclination is to do something like ```JSON.stringify(graph)```, which produces the terribly helpful error ```TypeError: Converting circular structure to JSON```. What's happened is that our vertices were linked to their edges, and their edges to their vertices, and now everything refers to everything else. So how can we extract our nice neat sets again? JSON replacer functions to the rescue.

The JSON.stringify function takes a value to stringify, but it also takes two additional parameters: a replacer function and a whitespace number [footnote: doing JSON.stringify(x, 0, 2) in the console is a great way to make ugly data readable]. The replacer allows you to customize how the stringify function operates. 

In our case, we'd like to treat the vertices and edges differently, so we're going to manually merge the two sides into a single JSON string. Manually manipulating JSON like this isn't recommended, because the format is insufferably persnickety, but with only a dozen characters in our raw output we should be okay. [footnote: He says glibly. The first three attempts at this were botched in some browsers. It really is insufferable.]

```javascript
Dagoba.jsonify = function(graph) {
  return '{"V":' + JSON.stringify(graph.vertices, Dagoba.cleanVertex)
       + ',"E":' + JSON.stringify(graph.edges,    Dagoba.cleanEdge)
       + '}' 
}
```

And these are the replacers for vertices and edges.

```javascript
Dagoba.cleanVertex = function(key, value) {
  return (key == '_in' || key == '_out') ? undefined : value 
}

Dagoba.cleanEdge = function(key, value) {
  return (key == '_in' || key == '_out') ? value._id : value 
}
```

The only difference between them is what they do when a cycle is about to be formed: for vertices, we skip the edge list entirely. For edges, we make a list of each vertex's identifier. 

We could glue these together into a single function, and even hit the whole graph with a single replacer to avoid having to manually massage the JSON output, but the result would probably be messier. Try it yourself and see if you can come up with a well-factored solution that avoids hand-coded JSON. [footnote: bonus points if it fits in a tweet.]


#### Persistence

Persistence is usually one of the trickier parts of a database: disks are dreadfully slow, but relatively safe. Batching writes, making them atomic, journaling -- all of these are difficult to get right. 

Fortunately, we're building an *in-memory* database, so we don't have to worry about any of that! We may, though, occasionally want to save a copy of the database locally for fast restart on page load. We can use the serializer we just built to do exactly that.

```
Dagoba.persist = function (graph, name) {
  name = 'DAGOBA::' + (name || 'graph')
  var flatgraph = Dagoba.jsonify(graph)
  localStorage.setItem(name, flatgraph)
}

Dagoba.depersist = function (name) {
  name = 'DAGOBA::' + (name || 'graph')
  var flatgraph = localStorage.getItem(name)
  var seedgraph = JSON.parse(flatgraph)                 // this can throw
  return Dagoba.graph(seedgraph.V, seedgraph.E)
}
```

We preface the name with a faux namespace to avoid polluting the localStorage properties of the domain, as it can get quite crowded in there [footnote: it also makes our closing parens all line up vertically, which is always a nice bonus]. There's usually a low storage limit also, so for larger graphs we'd probably want to use a Blob of some sort. 

There are also potential issues if multiple browser windows from the same domain are persisting and depersisting simultaneously. The localStorage space is shared between those windows, and they're potentially on different event loops, so there's the possibility for one to carelessly overwrite the work of another. The spec says there should be a mutex required for read/write access to localStorage, but it's inconsistently implemented between different browsers, and even with it a naive implementation like ours could still encounter issues.

If we wanted our persistence implementation to be concurrency aware we could use the storage events that are fired when changes are made to localStorage, and update our local graph accordingly. 


#### Updates

There's a problem with our 'out' query component: if someone deletes an edge we've visited while we're in the middle of a query, we'll skip a different edge because our counter is off. We could lock the vertices in our query, but one of the strengths of this approach is driving the iteration through the query space from code, so our query object might be long-lived. Even though we're in a single-threaded event loop, our queries can span multiple asynchronous re-entries, which means concurrency concerns like this are a very real problem. 
So instead we'll slice and pop the edge list each time we reach a new vertex. This burns some extra CPU and pushes more work onto the GC, so we'll stick a note here so we know what to do if this shows up as a hotspot during our profiling.

// new 'out' query component (and friends)

One concern with doing our queries this new way is that we're still not seeing a completely consistent chronology. Skipping random edges, like we did before, leaves us with an entirely inconsistent view of the universe, where things that have always existed may appear to be gone. This is generally undesirable, though many modern systems for storing very large amounts of data have exactly this property. [N] [google, facebook, kayak, etc -- often queries over heavily sharded datasets or multiple apis with pagination or timeouts have this property]

The change we just made means we will always traverse every edge a particular vertex had _at the moment we visited it_. That means that as we begin traversing edges, we may see new vertices at different points in the graph chronology, and may even see the same vertex at different points in the chronology at different points in our query. Depending on the relationships we're storing, this may provide a view of the universe that seemingly defies the laws of physics, even though no laws have actually been broken. 

If we need to see the world as it exists at a particular moment in time (e.g. 'now', where now is the moment our query begins) we can change our driver loop and the update handlers to add versioning to the data, and pass a pointer to that particular version into the query system. Doing this also opens the door to true transactions, and automated rollback/retries in an STM-like fashion. 


## Wrapping up

So what have we learned? Graph databases are great for storing interconnected [Z] data that you plan to query via graph traversals. Adding laziness allows for a fluent interface over queries you could never express in an eager system for performance reasons, and allows you to cross async boundaries. Time makes things complicated, and time from multiple perspectives (i.e. concurrency) makes things very complicated, so whenever we can avoid introducing a temporal dependence (e.g. mutable state, measurable effects, etc) we make reasoning about our system easier. Building in a simple, decoupled and painfully unoptimized style leaves the door open for global optimizations later on, and using a driver loop allows for orthogonal optimizations -- each without introducing the brittleness and complexity into our code that is the hallmark of most optimization techniques. 

That last point can't be overstated: keep it simple. Eschew optimization in favor of simplicity. Work hard to achieve simplicity by finding the right model. Explore many possibilities. The chapters in this book provide ample evidence that highly non-trivial applications can have a small, tight kernel. Once you find that kernel for the application you are building, fight to keep complexity from polluting it. Build hooks for attaching additional functionality, and maintain your abstraction barriers at all costs. Using these techniques well is not easy, but they can give you leverage over otherwise intractable problems. 





[Z] Not *too* interconnected, though -- you'd like the number of edges to grow in direct proportion to the number of vertices. In other words the average number of edges a vertex has shouldn't vary with the size of the graph. Most systems we'd consider putting in a graph database already have this property: if we add 100,000 Nigerian films to our movie database that doesn't increase the degree of the Kevin Bacon vertex.




THE END
-------





OTHER STUFF
-----------









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

