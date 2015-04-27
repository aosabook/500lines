# Dagoba: an in-memory graph database

_An exploration of connectedness through the lens of familial lineage_

A long time ago, when the world was still young, all data walked happily in single file. If you wanted your data to jump over a fence, you just set the fence down in its path and each datum jumped it in turn. Punch cards in, punch cards out. Life was easy and programming was a breeze.

Then came the random access revolution, and data grazed freely across the hillside. Herding data became a serious concern -- if you can access any piece of data at any time, how do you know which one to pick next? Techniques were developed for corralling the data by forming links between items [footnoteA], marshaling groups of units into formation through their linking assemblage. Questioning data meant picking a sheep and pulling along everything connected to it. 

Later programmers departed from this tradition, imposing a set of rules on how data would be aggregated[footnoteB]. Rather than tying disparate data directly together they would cluster by content, decomposing data into bite-sized pieces, collected in kennels and collared with a name tag. Questions were declaratively posited, resulting in accumulating pieces of partially decomposed data (a state the relationalists refer to as "normal") into a frankencollection returned to the programmer.

For much of recorded history this relational model reigned supreme. Its dominance went unchallenged through two major language wars and countless skirmishes. It offered everything you could ask for in a model, for the small price of inefficiency, clumsiness and lack of scalability. For eons that was a price programmers were willing to pay. Then the internet happened.

The distributed revolution changed everything, again. Data broke free of spacial constraints and roamed from machine to machine. CAP-wielding theorists busted the relational monopoly, opening the door to a plethora of new herding techniques -- some of which harken back to the earliest attempts to domesticate random-access data. We're going to look at one of these, a style known as the graph database.

[footnoteA: One of the very first database designs was the hierarchical model, which grouped items into tree-shaped hierarchies and is still used as the basis of IBM's IMS product, a high-speed transaction processing system. It's influence can also been seen in XML, file systems and geographic information storage. The network model, invented by Charles Bachmann and standardized by CODASYL, generalized the hierarchical model by allowing multiple parents, forming a DAG instead of a tree. These navigational database models came in to vogue in the 1960s and continued their dominance until performance gains made relational databases usable in the 1980s.]

[footnoteB: Codd developed relational database theory while working at IBM, but Big Blue feared that a relational database would cannibalize the sales of IMS. While IBM eventually built a research prototype called System R, it was based around a new non-relational language called SEQUEL, instead of Codd's original Alpha language. The SEQUEL language was copied by Larry Ellison in his Oracle Database based on pre-launch conference papers, and the name changed to SQL to avoid trademark disputes.]


## Definitions and introductions

This graph database we build will allow us to elegantly solve all kinds of interesting problems. So what's a graph database?

Well, the dictionary defines "graph database" as a database for graphs. Thanks, dictionary! Let's break that down a little.

A "data base" is like a fort for data. You can put data in it and get data back out of it.

A graph in this sense is a set of vertices and a set of edges. It's basically a bunch of dots connected by lines. 

What kinds of problems can it solve? Suppose that you are one of those who have discovered the unbridled joy of tracking ancestral trees: parents, children, all that kind of thing. You'd like to develop a system that allows you to make natural and elegant queries like "Who are Thor's second cousins once removed?" or "What is Freyja's connection to the Valkyries?".

A reasonable schema for this data structure would be to have a table of entities and a table of relationships. A query for Thor's parents might look like:

```javascript
SELECT e.* FROM entities as e, relationships as r WHERE r.out = "Thor" AND r.type = "parent" AND r.in = e.id
```

But how do we extend that to grandparents? We need to do a subquery, or use some other type of vendor-specific extension to SQL. And by the time we get to second cousins once removed we're going to have ALOTTA SQL.

What would we like to write? Something both concise and flexible; something that models our query in a natural way and extends to other queries like it. `second_cousins_once_removed('Thor')` is concise, but it doesn't give us any flexibility. The SQL above is flexible, but lacks concision.

Something like `Thor.parents.parents.parents.children.children.children` strikes a reasonably good balance. The primitives give us flexibility to ask many similar questions, but the query is also very concise and natural. This particular phrasing gives us too many results, as it includes first cousins and siblings, but we're going for gestalt here.

What's the simplest thing we can build that gives us this kind of interface? We could make a list of vertices and a list of edges, just like the relational schema, and then build some helper functions. It might look something like this:

```javascript
V = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
E = [ [1,2], [1,3], [2,4], [2,5], [3,6], [3,7], [4,8], [4,9], [5,10], [5,11], [6,12], [6,13], [7,14], [7,15] ]

// imperative style
parents = function(vertices) {
  var accumulator = []
  for(var i=0; i < E.length; i++) {
    var edge = E[i]
    if(vertices.indexOf(edge[1]) !== -1)
      accumulator.push(edge[0])
  }
  return accumulator
}
```

The essence of the above function is to iterate over a list, evaluating some code for each item and building up an accumulator of results. It's a little hard to see that though, because the looping construct introduces some unnecessary complexity. 

It'd be nice if there was a more specific looping construct designed for this purpose. As it happens, the `reduce` function does exactly what we'd like: given a list and a function, it evaluates the function for each element of the list, threading the accumulator through each evaluation pass.

Written in this more functional style, our functions are both shorter and clearer in intent:

```javascript
parents  = (vertices) => E.reduce( (acc, [parent, child]) => vertices.includes(child)  ? acc.concat(parent) : acc , [] )
children = (vertices) => E.reduce( (acc, [parent, child]) => vertices.includes(parent) ? acc.concat(child)  : acc , [] )
```

Given a list of vertices we then reduce over the edges, adding an edge's parent to the accumulator if the edge's child is in our input list. The children function is identical, but it looks at the edge's parent to determine whether to add the edge's child.

Those functions are valid JS, but use a few features browsers haven't implemented as of this writing. This translated version will work today:

```javascript
parents  = function(x) { return E.reduce( function(acc, e) { return ~x.indexOf(e[1]) ? acc.concat(e[0]) : acc }, [] )}
children = function(x) { return E.reduce( function(acc, e) { return ~x.indexOf(e[0]) ? acc.concat(e[1]) : acc }, [] )}
```

Now we can say something like `children(children(children(parents(parents(parents([8]))))))`. It reads backwards and gets us lost in silly parens, but is otherwise pretty close to what we wanted. Take a minute to look at the code. Can you see any ways to improve it?

Well, we're treating the edges as a global variable, which means we can only ever have one database at a time using these helper functions. That's pretty limiting. 

We're also not using the vertices at all. What does that tell us? It implies that everything we need is in the edges array, which in this case is true: the vertex values are scalars, so they exist independently in the edges array. If we want to answer questions like "What is Freyja's connection to the Valkyries?" we'll need to add more information to the vertices, which means making them compound values, which means the edges array should reference vertices instead of copying their value.

The same holds true for our edges: they contain an 'in' vertex and an 'out' vertex [footnote], but no elegant way to incorporate additional information. We'll need that to answer questions like "How many stepparents did Loki have?" or "How many children did Odin have before Thor was born?"

You don't have to squint very hard to tell that the code for our two selectors looks very similar, which suggests there may be a deeper abstraction from which those spring. 

Do you see any other issues?

[footnote on vertex]
  Notice that we're modeling edges as a pair of vertices. Also notice that those pairs are ordered, because we're using arrays. That means we're modeling a *directed graph*, where every edge has a starting vertex and an ending vertex. Our "dots and lines" visual model becomes a "dots and arrows" model instead.
  This adds complexity to our model, because we have to keep track of the direction of edges, but it also allows us to ask more interesting questions, like "which vertices point to vertex 3?" or "which vertex has the most outgoing edges?". 
  If we need to model an undirected graph we could add a reversed edge for each existing edge in our directed graph. It can be cumbersome to go the other direction, and simulate a directed graph from an undirected one. Can you think of a way to do it?


## Build a better graph

Let's solve a few of the problems we've discovered. Having our vertices and edges be global constructs limits us to one graph at a time, but we'd like to have more. To solve this we'll need some structure. Let's start with a namespace.

```javascript
Dagoba = {}                                             // the namespace
```

We'll use an object as our namespace. An object in JavaScript is mostly just an unordered set of key/value pairs. We only have four basic data structures to choose from in JS, so we'll be using this one a lot. (A fun question to ask people at parties is "What are the four basic data structures in JavaScript?")

Now we need some some graphs. We can build these using a classic OOP pattern, but JavaScript offers us prototypal inheritance, which means we can build up a prototype object -- we'll call it Dagoba.G -- and then instantiate copies of that using a factory function. An advantage of this approach is that we can return different types of objects from the factory, instead of binding the creation process to a single class constructor. So we get some extra flexibility for free. 

```javascript
Dagoba.G = {}                                           // the prototype

Dagoba.graph = function(V, E) {                         // the factory
  var graph = Object.create( Dagoba.G )

  graph.edges       = []                                // fresh copies so they're not shared
  graph.vertices    = []
  graph.vertexIndex = {}                                // a lookup optimization
  
  graph.autoid = 1                                      // an auto-incrementing id counter
  
  if(Array.isArray(V)) graph.addVertices(V)             // arrays only, because you wouldn't
  if(Array.isArray(E)) graph.addEdges(E)                //   call this with singular V and E
  
  return graph
}
```

We'll accept two optional arguments: a list of vertices and a list of edges. JavaScript is very lax about parameters, so all named parameters are optional and default to 'undefined' if not supplied [footnote]. We will often have the vertices and edges before building the graph and use the V and E parameters, but it's also common to not have those at creation time and to build the graph up programmatically [footnote].

[footnote on supplied: It's also lax the other direction: all functions are variadic, and all arguments are available by position via the `arguments` object, which is almost like an array but not quite. ('Variadic' is just a fancy way of saying a function has indefinite arity. Which is a fancy way of saying it takes a variable number of variables.)]

[footnote on programmatically: The `Array.isArray` checks here are to distinguish our two different use cases, but in general we won't be doing many of the validations one would expect of production code in order to focus on the architecture instead of the trash bins.]

Then we create a new object that has all of our prototype's strengths and none of its weaknesses*. We build a brand new array (one of the other basic JS data structures) for our edges, another for the vertices, a new object called vertexIndex and an id counter -- more on those latter two later. (Think: why can't we just put all of these in the prototype?)

Then we call addVertices and addEdges from inside our factory, so let's define those now.

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

If the vertex doesn't already have an _id property we assign it one using our autoid [footnote: Why can't we just use this.vertices.length here?]. If the _id already exists on a vertex in our graph we reject the new vertex. Wait, when would that happen? And what exactly is a vertex?

In a traditional object-oriented system we would expect to find a vertex class, which all vertices would be an instance of. We're going to take a different approach and consider as a vertex any object containing the three properties _id, _in and _out. Why is that? Ultimately, it comes down to giving Dagoba control over which data is shared with the host application.

If we create some Dagoba.Vertex instance inside the addVertex function, our internal data will never be shared with the host application. If we accept a Dagoba.Vertex instance as the argument to our addVertex function, the host application could retain a pointer to that vertex object and manipulate it at runtime, breaking our invariants.

So if we create a vertex instance object, we're forced to decide up front whether we will always copy the provided data into a new object -- potentially doubling our space usage -- or allow the host application unfettered access to the database objects. There's a tension here between performance and protection, and the right balance depends on your specific use case.

Duck typing on the vertex's properties allows us to make that decision at run time, by either deep copying* the incoming data or using it directly as a vertex*. We don't always want to put the responsibility for balancing safety and performance in the hands of the user, but because these two sets of use cases diverge so widely the extra flexibility is important.

Okay, now that we've got our new vertex we'll add it in to our graph's list of vertices, add it to the vertexIndex for efficient lookup by _id, and add two additional properties to it: _out and _in, which will both become lists of edges*. 

[footnote on deep copying: Often when faced with space leaks due to deep copying the solution is to use a path copying persistent data structure, which allows mutation-free changes for only log(N) extra space. But the problem remains: if the host application retains a pointer to the vertex data then it can mutate that data any time, regardless of what strictures we impose in our database. The only practical solution is deep copying vertices, which doubles our space usage. Dagoba's original use case involves vertices that are treated as immutable by the host application, which allows us to avoid this issue, but requires a certain amount of discipline on the part of the user.]

[footnote on vertex: We could make this decision based on a Dagoba-level configuration parameter, a graph-specific configuration, or possibly some type of heuristic.]

[footnote on edges: We use the term 'list' to refer to the abstract data structure requiring push and iterate operations. We use JavaScript's 'array' concrete data structure to fulfill the API required by the list abstraction. Technically both "list of edges" and "array of edges" are correct, so which we use at a given moment depends on context: if we are relying on the specific details of JavaScript arrays, like the `.length` property, we will say "array of edges". Otherwise we say "list of edges", as an indication that any list implementation would suffice.]

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

First we find both vertices the edge connects, then reject the edge if it's missing either vertex. We'll use a helper function to log an error on rejection. All errors flow through this helper function, so we can override its behavior on a per-application basis. We could later extend this to allow onError handlers to be registered, so the host application could link in its own callbacks without overwriting the helper. We might allow such handlers to be registered per-graph, per-application, or both, depending on the level of flexibility required.

```javascript
Dagoba.error = function(msg) {
  console.log(msg)
  return false 
}
```

Then we'll add our new edge to both vertices' edge lists: the edge's out vertex's list of out-side edges, and the in vertex's list of in-side edges.

And that's all the graph structure we need for now!


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

A *program* is a series of *steps*. Each step is like a pipe in a pipeline -- a piece of data comes in one end, is transformed in some fashion, and goes out the other end. Our pipeline doesn't quite work like that, but it's a good first approximation. 

Each step in our program can have *state*, and `query.state` is a list of per-step state that index correlates with the list of steps in query.program. 

A *gremlin* is a creature that travels through the graph doing our bidding. A gremlin might be a surprising thing to find in a database, but they trace their heritage back to Tinkerpop's Blueprints[1], and the Gremlin and Pacer query languages[2]. They remember where they've been and allow us to find answers to interesting questions. 

[1: http://euranova.eu/upl_docs/publications/an-empirical-comparison-of-graph-databases.pdf]

[2: http://edbt.org/Proceedings/2013-Genova/papers/workshops/a29-holzschuher.pdf]

Remember that question we wanted to answer? The one about Thor's second cousins once removed? We decided `Thor.parents.parents.parents.children.children.children` was a pretty good way of expressing that. Each `parents` or `children` instance is a step in our program. Each of those steps contains a reference to its *pipetype*, which is the function that performs that step's operation.

That query in our actual system might look like `g.v('Thor').out().out().out().in().in().in()`. Each of the steps is a function call, and so they can take *arguments*. The interpreter passes the step's arguments in to the step's pipetype function, so in the query `g.v('Thor').out(2, 3)` the `out` pipetype function would receive `[2, 3]` as its first parameter.

We'll need a way to add steps to our query. Here's a helper function for that:

```javascript
Dagoba.Q.add = function(pipetype, args) {               // add a new step to the query
  var step = [pipetype, args]
  this.program.push(step)                               // step is an array: first the pipe type, then its args
  return this
}
```

Each step is a composite entity, combining the pipetype function with the arguments to apply to that function. We could combine the two into a partially-applied function at this stage, instead of using a tuple [footnote: A tuple is another abstract data structure -- one that is more constrained than a list. In particular a tuple has a fixed size: in this case we're using a 2-tuple (also known as a "pair" in the technical jargon of data structure researchers). Using the term for the most constrained abstract data structure required is a nicety for future implementors.], but then we'd lose some introspective power that will prove helpful later.

We'll use a small set of query initializers that create generate a new query from a graph. Here's one that starts most of our examples: the `v` method. It builds a new query, then uses our `add` helper to populate the initial query program. This makes use of the `vertex` pipetype, which we'll look at soon.

```javascript
Dagoba.G.v = function() {                                         // a query initializer: g.v() -> query
  var query = Dagoba.query(this)
  query.add('vertex', [].slice.call(arguments))                   // add a vertex pipetype step to our program
  return query
}
```

Note that `[].slice.call(arguments)` is JS parlance for "Please pass me an array of the arguments to this function". You would be forgiven for supposing that `arguments` itself is already an array, since it behaves like one in many situations, but it is sadly lacking most of the functionality which modern JavaScript arrays have been granted.

## The problem with being eager

Before we look at the pipetypes themselves we're going to take a slight diversion into the exciting world of execution strategy. There are two main schools of thought: the Call By Value clan, also known as eager beavers, strictly insist that all arguments be evaluated before the function is applied. Their opposing faction, the Call By Needians, are content to procrastinate until the last possible moment before doing anything, and even then do as little as possible -- they are, in a word, lazy.

JavaScript, being a strict language, will process each of our steps as they are called. We would then expect the evaluation of `g.v('Thor').out().in()` to first find the Thor vertex, then find all vertices connected to it by outgoing edges, and from each of those vertices finally return all vertices they are connected to by inbound edges.

In a non-strict language we would get the same result -- the execution strategy doesn't make much difference here. But what if we added a few additional calls? Given how well-connected Thor is, our `g.v('Thor').out().out().out().in().in().in()` query may produce many results -- in fact, because we're not limiting our vertex list to unique results, it may produce many more results than we have vertices in our total graph.

We're probably only interested in getting a few unique results out, so we'll change the query a little: `g.v('Thor').out().out().out().in().in().in().unique().take(10)`. Now our query produces at most 10 results. What happens if we evaluate this eagerly, though? We're still going to have to build up septillions of results before returning only the first 10.

All graph databases have to support a mechanism for doing as little work as possible, and most choose some form of non-strict evaluation to do so. Since we're building our own interpreter the lazy evaluation our program is certainly achievable, but we may have to contend with some unintended consequences.


## Ramifications of evaluation strategy on our mental model

Up until now our mental model for evaluation has looked like this:

[[diagram]]

We would like to retain that model for our users, because it's easier to reason about, but as we've seen we can no longer use that model for the implementation. Having users think in a model that differs from the actual implementation is the source of much pain. A leaky abstraction is a small scale version of this; in the large it can lead to frustration, cognitive dissonance and ragequits. 

Our case is nearly optimal for this deception, though: the answer to any query will be the same, regardless of execution model. The only difference is the performance. The tradeoff is between having all users learn a more complicated model prior to using the system, or forcing a subset of users to transfer from the simple model to the complicated model in order to better reason about query performance. 

Some factors to consider when wrestling with this decision are: the relative cognitive difficulty of learning the simple model vs the more complex model; the additional cognitive load imposed by first using the simple model and then advancing to the complex one vs skipping the simple and learning only the complex; the subset of users required to make the transition, in terms of their proportional size, cognitive availability, available time, and so on.

In our case this tradeoff makes sense. For most uses queries will perform quickly enough that users needn't be concerned with optimizing their query structure or learning the deeper model. Those who will are the users writing advanced queries over large datasets, and they are also likely the users most well equipped to transition to a new model. Additionally, our hope is that there is only a small increase in difficulty imposed by using the simple model before learning the more complex one.

Here is the exposed surface of the more complex model:

[[diagram]]

[[Referencing diagram, spell out that we're achieving lazy evaluation with a turing machine like model]]
We'll dig deeper when we look at the implementation of the interpreter, but there are a few important points to keep in mind while we examine the pipetypes:
- Remember those gremlins we mentioned before? In our simplistic model each pipe spits out the entire set of matching vertices, once per query. In the actual implementation each pipe returns at most one gremlin, but does this potentially many times during the query. Each gremlin represents a potential query result, and they carry state with them through the pipes.
- We process the query from back to front, so if a pipe needs some input before it can produce gremlins, it returns a 'pull' signal, causing the head to move back one pipe. [[explain 'head' better here once diagram is in place]]
- If a pipe has finished and will never produce another gremlin, it returns a 'done' signal, causing the head to move forward and the done blocker to move to its position.


## Pipetypes

Before we dig in to the internals of the interpreter we'll spend some time getting to know the pipetypes that make up the core functionality of our system. Once we understand how each pipetype works we'll have a better basis for understanding how they're invoked and sequenced together.

We'll start by making a place to put our pipe types, and a way to add new ones.

```javascript
Dagoba.Pipetypes = {}                                   // every pipe has a type

Dagoba.addPipetype = function(name, fun) {              // adds a new method to our query object
  Dagoba.Pipetypes[name] = fun
  Dagoba.Q[name] = function() {
    return this.add(name, [].slice.apply(arguments)) }  // capture the pipetype and args
}
```

The pipetype's function is added to the list of pipetypes, and then a new method is added to the query object. Every pipetype must have a corresponding query method. That method adds a new step to the query program, along with its arguments. 

When we evaluate `g.v('Thor').out('father').in('brother')` the `v` call returns a query object, the `out` call adds a new step and returns the query object, and the `in` call does the same. This is what enables our method chaining API.

Note that adding a new pipetype with the same name replaces the existing one, which allows runtime modification of existing pipetypes. What's the cost of this decision? What are the alternatives?

```javascript
Dagoba.getPipetype = function(name) {
  var pipetype = Dagoba.Pipetypes[name]                 // a pipe type is just a function 

  if(!pipetype)
    Dagoba.error('Unrecognized pipe type: ' + name)

  return pipetype || Dagoba.fauxPipetype
}
```

If we can't find a pipetype we generate an error and return the default pipetype, which acts like an empty conduit: if a message comes in one side, it gets passed out the other.

```javascript
Dagoba.fauxPipetype = function(_, _, maybe_gremlin) {   // if you can't find a pipe type 
  return maybe_gremlin || 'pull'                        // then keep things flowing along
}
```

See those underscores? We use those to label params that won't be used in our function. Most other pipetypes will use all three parameters, and have all three parameter names. This allows us to distinguish at a glance which parameters a particular pipetype relies on [footnote: Actually, we only used this underscore technique here to make the comments line up nicely. No, seriously. If programs "must be written for people to read, and only incidentally for machines to execute", then it immediately follows that our predominant concern should be making code pretty.].


#### Vertex

Most pipetypes we meet will take a gremlin and produce more gremlins, but this particular pipetype generates gremlins from just a string. Given an vertex id it returns a single new gremlin. Given a query it will find all matching vertices, and yield one new gremlin at a time until it's worked through them.

```javascript
Dagoba.addPipetype('vertex', function(graph, args, gremlin, state) {
  if(!state.vertices) 
    state.vertices = graph.findVertices(args)           // state initialization

  if(!state.vertices.length)                            // all done
    return 'done'

  var vertex = state.vertices.pop()                     // OPT: this relies on cloning the vertices
  return Dagoba.makeGremlin(vertex, gremlin.state)      // we can have incoming gremlins from as/back queries
})
```

We first check to see if we've already gathered matching vertices, otherwise we try to find some. If there are any vertices then we'll pop one off and return a new gremlin sitting on that vertex. Each gremlin can carry around its own state, like a journal of where it's been and what interesting thing it has seen on its journey through the graph. If we receive a gremlin as input to this step we'll copy its journal for the exiting gremlin.

Note that we're directly mutating the state argument here, and not passing it back. An alternative would be to return an object instead of a gremlin or signal, and pass state back that way. That complicates our return value, and creates some additional garbage. [footnote: Very short lived garbage though, which is the second best kind.] If JS allowed multiple return values it would make this option more elegant. 

We would still need to find a way to deal with the mutations, though, as the call site maintains a reference to the original variable. What if we had some way to determine whether a particular reference is 'unique' -- that it is the only reference to that object? 

If we know a reference is unique then we can get the benefits of immutability while avoiding expensive copy-on-write schemes or complicated persistent data structures. With only one reference we can't tell whether the object has been mutated or a new object has been returned with the changes we requested: "observed immutability" is maintained.*

There are a couple of common ways of determining this: in a statically typed system we might make use of uniqueness types* to guarantee at compile time that each object has only one reference. If we had a reference counter* -- even cheap two-bit sticky counter -- we could know at runtime that an object only has one reference and use that knowledge to our advantage.

JavaScript doesn't have either of these facilities, but we can get almost the same effect if we're really, really disciplined. Which we will be. For now.

[footnote on maintained: Two references to the same mutable data structure act like a pair of walkie-talkies, allowing whoever holds them to communicate directly. Those walkie-talkies can be passed around from function to function, and cloned to create whole passel of walkie-talkies. This completely subverts the natural communication channels your code already possesses. In a system with no concurrency you can sometimes get away with it, but introduce multithreading or asynchronous behavior and all that walkie-talkie squawking can really be a drag.]

[footnote on uniqueness types: Uniqueness types were dusted off in the Clean language, and have a non-linear relationship with linear types, which are themselves a subtype of substructural types.]

[footnote on reference counter: Most modern JS runtimes employ generational garbage collectors, and the language is intentionally kept at arm's length from the engine's memory management to curtail a source of programmatic non-determinism.]


#### In-N-Out

Walking the graph is as easy as ordering a burger. These two lines set up the 'in' and 'out' pipetypes for us.

```javascript
Dagoba.addPipetype('out', Dagoba.simpleTraversal('out'))
Dagoba.addPipetype('in',  Dagoba.simpleTraversal('in'))
```

The simpleTraversal function returns a pipetype handler that accepts a gremlin as its input, and then spawns a new gremlin each time it's queried. Once those gremlins are gone it sends back a 'pull' request to get a new gremlin from its predecessor. 

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

But we can see the same beats being hit here, with the addition of a query initialization step. If there's no gremlin and we're out of available edges then we pull. If we have a gremlin but haven't yet set state then we find any edges going the appropriate direction and add them to our state. If there's a gremlin but its current vertex has no appropriate edges then we pull. And finally we pop off an edge and return a freshly cloned gremlin on the vertex to which it points.

Glancing at this code we see `!state.edges.length` repeated in each of the three clauses. It's tempting to refactor this to reduce the complexity of those conditionals. There are two issues keeping us from doing so. One is relatively minor: the third `!state.edges.length` means something different than the first two, since `state.edges` has been changed between the second and third conditional. This actually encourages us to refactor, because having the same label mean two different things inside a single function usually isn't ideal.

But this isn't the only pipetype function we're writing, and we'll see these ideas of query initialization and/or state initialization repeated over and over. There's always a balancing act when writing code between structured qualities and unstructured qualities. Too much structure and you pay a high cost in boilerplate and abstraction complexity. Too little structure and you'll have to keep all the plumbing minutia in your head.

In this case, with a dozen or so pipetypes, the right choice seems to be to style each of the pipetype functions as similarly as possible, and label the constituent pieces with comments. So we resist our impulse to refactor this particular pipetype, because doing so would reduce uniformity, but we also resist the urge to engineer a formal structural abstraction for query initialization, state initialization, and the like. If there were hundreds of pipetypes that latter choice would likely be the right one -- the complexity cost of the abstraction is constant, while the benefit accrues linearly with the number of units. When handling that many moving pieces anything you can do to enforce regularity among them is helpful.


#### Property

Let's pause for a moment to consider an example query based on the three pipetypes we've seen. We can ask for Thor's grandfathers like this: `g.v('Thor').out('father').out('father').run()`. [footnote: The `run()` at the end of the query invokes the interpreter and returns results.] But what if we wanted their names? 

We could put a map on the end of that:

```javascript
g.v('Thor').out('father').out('father').run().map(function(vertex) {return vertex.name})
```

But this is a common enough operation that we'd prefer to write something more like:

```javascript
g.v('Thor').out('father').out('father').property('name').run()
```

Plus this way the property pipe is an integral part of the query, instead of something appended after. This has some interesting benefits, as we'll soon see.

```javascript
Dagoba.addPipetype('property', function(graph, args, gremlin, state) {
  if(!gremlin) return 'pull'                            // query initialization
  gremlin.result = gremlin.vertex[args[0]]
  return gremlin.result == null ? false : gremlin       // undefined or null properties kill the gremlin
})
```

Our query initialization here is trivial: if there's no gremlin, we pull. If there is a gremlin, we'll set its result to the property's value. Then the gremlin can continue onward. If it makes it through the last pipe its result will be collected and returned from the query. Not all gremlins have a `result` property. Those that don't return their most recently visited vertex.

Note that if the property doesn't exist we return false instead of the gremlin, so property pipes also act as a type of filter. Can you think of a use for this? What are the tradeoffs in this design decision? 


#### Unique

If we want to collect all of Thor's grandparents' grandchildren -- his cousins, his siblings, and himself -- we could do a query like this: `g.v('Thor').in().in().out().out().run()`. That would give us many duplicates, however. In fact there would be at least four copies of Thor himself. (Can you think of a time when there might be more?)

To resolve this we introduce a new pipetype called 'unique'. Our new query `g.v('Thor').in().in().out().out().unique().run()` produces output in one-to-one correspondence with the grandchildren.

```javascript
Dagoba.addPipetype('unique', function(graph, args, gremlin, state) {
  if(!gremlin) return 'pull'                            // query initialization
  if(state[gremlin.vertex._id]) return 'pull'           // we've seen this gremlin, so get another instead
  state[gremlin.vertex._id] = true
  return gremlin
})
```

A unique pipe is purely a filter: it either passes the gremlin through unchanged or it tries to pull a new gremlin from the previous pipe. 

We initialize by trying to collect a gremlin. If the gremlin's current vertex is in our cache, then we've seen it before so we try to collect a new one. Otherwise, we add the gremlin's current vertex to our cache and pass it along. Easy peasy.


#### Filter

We've seen two simplistic ways of filtering, but sometimes we need more elaborate constraints. What if we wanted Thor's siblings whose weight in skippund is greater than their height in fathoms? This query would give us our answer: 

```javascript
g.v('Thor').out().in().unique()
 .filter(function(asgardian) { return asgardian.weight > asgardian.height })
 .run()
```

[footnote: Depending on the density of Asgardian flesh this may return many results, or possibly just Volstagg [footnote: Provided we're allowing Shakespeare by way of Jack Kirby into our pantheon.].] 

If we wanted to know which of Thor's siblings survive Ragnarök we can pass filter an object:

```javascript
g.v('Thor').out().in().unique().filter({survives: true}).run()
```

Here's how it works:

```javascript
Dagoba.addPipetype('filter', function(graph, args, gremlin, state) {
  if(!gremlin) return 'pull'                            // query initialization

  if(typeof args[0] == 'object')                        // filter by object
    return Dagoba.objectFilter(gremlin.vertex, args[0]) 
         ? gremlin : 'pull'

  if(typeof args[0] != 'function') {
    Dagoba.error('Filter is not a function: ' + args[0]) 
    return gremlin                                      // keep things moving
  }

  if(!args[0](gremlin.vertex, gremlin)) return 'pull'   // gremlin fails filter function 
  return gremlin
})
```

If the filter's first argument is not an object or function then we trigger an error, and pass the gremlin along. Pause for a minute, and consider the alternatives. Why would we decide to continue the query once an error is encountered?

There are two possibilities for this error to arise. The first involves the user typing in queries, either in a REPL or in code they're actively working on. When that query is run it will produce results, but also generate an error. The user then corrects the error to filter down the set of results produced. The alternative approach for this use case would be to display the error produce and produce no results. Which of those you prefer is mostly personal preference.

The second possibility is that the filter is being applied dynamically at run time. This is a much more important case, because the person invoking the query is not necessarily the author of the query code. Because this is on the web, our default rule is to always show as much as we can, and never break things. It is usually preferable to soldier on in the face of grave tribulations rather than succumb to our wounds and present the user with a grisly error message.

For those occasions when showing too few results is better than showing too many, Dagoba.error can be overridden to throw an error, circumventing the natural control flow.


#### Take

We don't always want all the results at once. Sometimes we only need a handful of results: we want a dozen of Thor's contemporaries, so we walk all the way back to the primeval cow Auðumbla: 

```javascript
g.v('Thor').out().out().out().out().in().in().in().in().unique().take(12).run()
```

Without the take pipe that query could take quite a while to run, but thanks to our lazy evaluation strategy the query with the take pipe is very fast.

Sometimes we just want one at a time: we'll process the result, work with it, and then come back for another one. This pipetype allows us to do that as well.

```javascript
q = g.v('Auðumbla').in().in().in().property('name').take(1)

q.run() // ["Odin"]
q.run() // ["Vili"]
q.run() // ["Vé"]
q.run() // []
```

Our query can function in an asynchronous environment, allowing us to collect more results as needed. When we run out an empty array is returned.


```javascript
Dagoba.addPipetype('take', function(graph, args, gremlin, state) {
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

We initialize `state.taken` to zero if it doesn't already exist. JavaScript has implicit coercion, but coerces `undefined` into `NaN`, so we have to be explicit here. [footnote: Some would argue it's best to be explicit all the time. Others would argue that a good system for implicits makes for more concise, readable code, with less boilerplate and a smaller surface area for bugs. One thing we can all agree on is that using JavaScript's implicit coercion effectively requires memorizing a lot of non-intuitive special cases, making it a minefield for the uninitiated.]

Then when `state.taken` reaches `args[0]` we return 'done', sealing off the pipes before us. We also reset the `state.taken` counter, allowing us to repeat the query later.

We do those two steps before query initialization to handle the cases of `take(0)` and `take()` [footnote: What would you expect each of those to return? What do they actually return?]. Then we increment our counter and return the gremlin.


#### As

These next four pipetypes work as a group to allow more advanced queries. This one just allows you to label the current vertex. We'll use that label with the next two pipetypes.

```javascript
Dagoba.addPipetype('as', function(graph, args, gremlin, state) {
  if(!gremlin) return 'pull'                            // query initialization
  gremlin.state.as = gremlin.state.as || {}             // initialize gremlin's 'as' state
  gremlin.state.as[args[0]] = gremlin.vertex            // set label to the current vertex
  return gremlin
})
```

After initializing the query, we then ensure the gremlin's local state has an 'as' parameter. Then we set a property of that parameter to the gremlin's current vertex.

#### Merge

Once we've labeled vertices we can then extract them using merge. If we want Thor's parents, grandparents and great-grandparents we can do something like this:

```javascript
g.v('Thor').out().as('parent').out().as('grandparent').out().as('great-grandparent')
           .merge('parent', 'grandparent', 'great-grandparent').run()
```

Here's the merge pipetype:

```javascript
Dagoba.addPipetype('merge', function(graph, args, gremlin, state) {
  if(!state.vertices && !gremlin) return 'pull'                   // query initialization

  if(!state.vertices) {                                           // state initialization
    var obj = (gremlin.state||{}).as || {}
    state.vertices = args.map(function(id) {return obj[id]}).filter(Boolean)
  }

  if(!state.vertices.length) return 'pull'                        // done with this batch

  var vertex = state.vertices.pop()
  return Dagoba.makeGremlin(vertex, gremlin.state)
})
```

We map over each argument, looking for it in the gremlin's list of labeled vertices. If we find it, we clone the gremlin to that vertex.  


#### Except

We've already seen cases where we would like to say "Give me all of Thor's siblings who are not Thor". We can do that with a filter:

```javascript
g.v('Thor').out().in().unique().filter(function(asgardian) {return asgardian._id != 'Thor'}).run()
```

It's more straightforward with 'as' and 'except':

```javascript
g.v('Thor').as('me').out().in().except('me').unique().run()
```

But there are also queries that would be very difficult to try to filter. What if we wanted Thor's uncles and aunts? How would we filter out his parents? It's easy with 'as' and 'except':

```javascript
g.v('Thor').out().as('parent').out().in().except('parent').unique().run()
```

[footnote: There are certain conditions under which this particular query might yield unexpected results. Can you think of any? How could you modify it to handle those cases?]

```javascript
Dagoba.addPipetype('except', function(graph, args, gremlin, state) {
  if(!gremlin) return 'pull'                            // query initialization
  if(gremlin.vertex == gremlin.state.as[args[0]]) return 'pull'
  return gremlin
})
```

Here we're just checking whether the current vertex is equal to the one we stored previously. If it is, we skip it.


#### Back

Some of the questions we might ask involve checking further into the graph, only to return later to our point of origin if the answer is in the affirmative. Suppose we wanted to know which of Fjörgynn's daughters had children with one of Bestla's sons? 

```javascript
g.v('Fjörgynn').in('daughter').as('me')                 // first gremlin's state.as is Frigg
 .in()                                                  // first gremlin's vertex is now Baldr
 .out().out()                                           // put copy of that gremlin on each grandparent
 .filter({_id: 'Bestla'})                               // only keep the gremlin on grandparent Bestla
 .back('me').unique().run()                             // jump the gremlin's vertex back to Frigg and exit
```

Here's the definition for `back`:

```javascript
Dagoba.addPipetype('back', function(graph, args, gremlin, state) {
  if(!gremlin) return 'pull'                            // query initialization
  return Dagoba.gotoVertex(gremlin, gremlin.state.as[args[0]])
})
```

We're using the `Dagoba.gotoVertex` helper function to do all real work here. Let's take a look at that and some other helpers now.


## Helper functions

The pipetypes above rely on a few helper functions. Let's take a quick look at those before diving in to the interpreter. This is ostensibly because understanding these helpers will aid in understanding the interpreter, but it's mostly just to build up the anticipation.

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

Note that this function actually returns a brand new gremlin -- a clone of the old one, sent to our desired destination. That means a gremlin can sit on a vertex while its clones are sent out to explore many other vertices. This is exactly what happens in simpleTraversal.

As an example of possible enhancements, we could add a bit of state to keep track of every vertex the gremlin visits, and then add new pipetypes to take advantage of those paths.


#### Finding 

The `vertex` pipetype uses the findVertices function to collect a set of initial vertices from which to begin our query.

```javascript
Dagoba.G.findVertices = function(args) {                          // our general vertex finding function
  if(typeof args[0] == 'object')
    return this.searchVertices(args[0])
  else if(args.length == 0)
    return this.vertices.slice()                                  // OPT: slice is costly with lots of vertices
  else
    return this.findVerticesByIds(args)
}
```

This function receives its arguments as a list. If the first one is an object it passes it to searchVertices, allowing queries like `g.v({_id:'Thor'}).run()` or `g.v({species: 'Aesir'}).run()`. 

Otherwise, if there are arguments it gets passed to findVerticesByIds, which handles queries like `g.v('Thor', 'Odin').run()`.

If there are no arguments at all, then our query looks like `g.v().run()`. This isn't something you'll want to do frequently with large graphs, especially since we're slicing the vertex list before returning it. We slice because some call sites manipulate the returned list directly by popping items off as they work through them. We could optimize this use case by cloning at the call site, or by avoiding those manipulations (we could keep a counter in state instead of popping).

```javascript
Dagoba.G.findVerticesByIds = function(ids) {
  if(ids.length == 1) {
    var maybe_vertex = this.findVertexById(ids[0])                // maybe_vertex is either a vertex or undefined
    return maybe_vertex ? [maybe_vertex] : []
  }
  
  return ids.map( this.findVertexById.bind(this) ).filter(Boolean) 
}

Dagoba.G.findVertexById = function(vertex_id) {
  return this.vertexIndex[vertex_id] 
}
```

Note the use of vertexIndex here. Without that index we'd have to go through each vertex in our list one at a time to decide if it matched the id -- turning a constant time operation into a linear time one, and any O(n) operations that directly rely on it into O(n^2) operations. 

```javascript
Dagoba.G.searchVertices = function(filter) {            // find vertices that match obj's key-value pairs
  return this.vertices.filter(function(vertex) {
    return Dagoba.objectFilter(vertex, filter)
  })
}
```

The searchVertices function uses the objectFilter helper on every vertex in the graph. We'll look at objectFilter in the next section, but in the meantime can you think of with a way to search through the vertices lazily?


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

The first case is no filter at all: `g.v('Odin').in().run()` traverses all out edges from Odin.

The second filters on the edge's label: `g.v('Odin').in('son').run()` traverses all out edges with a label of 'son'.

The third case accepts an array of labels: `g.v('Odin').in(['daughter', 'son']).run()` traverses both son and daughter edges.

And the fourth case uses the objectFilter function we saw before:

```javascript
Dagoba.objectFilter = function(thing, filter) {         // thing has to match all of filter's properties
  for(var key in filter)
    if(thing[key] !== filter[key])
      return false
  
  return true 
}
```

This allows us to query the edge using a filter object: `g.v('Odin').in({position: 2, _label: daughter}).run()` finds Odin's second daughter, if position is genderized.


## The interpreter's nature

We've arrived at the top of the narrative mountain, ready to receive our prize: the much ballyhooed interpreter. The code is actually fairly compact, but the model has a bit of subtlety.

We compared programs to pipelines earlier, and that's a good mental model for writing queries. As we saw, though, we need a different model for the actual implementation. That model is more akin to a Turing machine than a pipeline. There's a read/write head that sits over a particular step. It "reads" the step, changes its "state", and then moves either right or left.

Reading the step means evaluating the pipetype function. As we saw above, each of those functions accepts as input the entire graph, its own arguments, maybe a gremlin, and its own local state. As output it provides a gremlin, false, or a signal of 'pull' or 'done'. The output is what our quasi-Turing machine reads to change its own state.

That state is comprised of just two variables: one to record steps that are `done`, and another to record the `results` of the query. Those are potentially updated, and the machine head either moves left, moves right, or the query finishes and the result is returned.

So we've now described all the state in our machine. We'll have a list of results that starts empty:

```javascript
  var results = []
```

An index of the last `done` step that starts behind the first step:

```javascript
  var done = -1
```

We need a place to store the most recent step's output, which might be a gremlin -- or it might be nothing -- so we'll call it `maybe_gremlin`:

```javascript
  var maybe_gremlin = false
```

And finally we'll need a program counter to indicate the position of the read/write head.

```javascript
  var pc = this.program.length - 1
```

Except... wait a second. How are we going to get lazy*? The traditional way of building a lazy system out of an eager one is to store parameters to function calls as "thunks" instead of evaluating them. You can think of a thunk as an unevaluated expression. In JS, which has first-class functions and closures, we can create a thunk by wrapping a function and its arguments in a new anonymous function which takes no arguments:

[footnote on lazy: Technically we need to implement an interpreter with non-strict semantics, which means it will only evaluate when forced to do so. Lazy evaluation is a technique used for implementing non-strictness. It's a bit lazy of us to conflate the two, so we will only disambiguate when forced to do so.]

```javascript
function sum() {
  return [].slice.call(arguments).reduce(function(acc, n) { return acc + (n|0) }, 0)
}

function thunk_of_sum_1_2_3() { return sum(1, 2, 3) }

function thunker(fun, args) {
  return function() {return fun.apply(fun, args)}
}

function thunk_wrapper(fun) {
  return function() {
    return thunker.apply(null, [fun].concat([[].slice.call(arguments)]))
  }
}

sum(1, 2, 3)              // -> 6
thunk_of_sum_1_2_3()      // -> 6
thunker(sum, [1, 2, 3])() // -> 6

var sum2 = thunk_wrapper(sum)
var thunk = sum2(1, 2, 3)
thunk()                   // -> 6
```

None of the thunks are invoked until one is actually needed, which usually implies some type of output is required: in our case the result of a query. Each time the interpreter encounters a new function call, we wrap it in a thunk. Recall our original formulation of a query: `children(children(children(parents(parents(parents([8]))))))`. Each of those layers would be a thunk, wrapped up like an onion.

There are a couple of tradeoffs with this approach: one is that spatial performance becomes more difficult to reason about, because of the potentially vast thunk graphs that can be created. Another is that our program is now expressed as a single thunk, and we can't do much with it at that point.

This second point isn't usually an issue, because of the phase separation between when our compiler runs its optimizations and when all the thunking occurs during runtime. In our case we don't have that advantage: because we're using method chaining to implement a fluent interface* if we also use thunks to achieve laziness we would thunk each new method as it is called, which means by the time we get to `run()` we have only a single thunk as our input, and no way to optimize our query.

[footnote on interface: Method chaining lets us write `g.v('Thor').in().out().run()` instead of `var query = g.query(); query.add('vertex', 'Thor'); query.add('in'); query.add('out'); query.run()`]

Interestingly, our fluent interface hides another difference between our query language and regular programming languages. The query `g.v('Thor').in().out().run()` could be rewritten as `run(out(in(v(g, 'Thor'))))` if we weren't using method chaining. In JS we would first process `g` and `'Thor'`, then `v`, then `in`, `out` and `run`, working from the inside out. In a language with non-strict semantics we would work from the outside in, processing each consecutive nested layer of arguments only as needed.

So if we start evaluating our query at the end of the statement, with `run`, and work our way back to `v('Thor')`, calculating results only as needed, then we've effectively achieved non-strictness. The secret is in the linearity of our queries. Branches complicate the process graph, and also introduce opportunities for duplicate calls, which require memoization to avoid wasted work. The simplicity of our query language means we can implement an equally simple interpreter based on our linear read/write head model.

In addition to allowing runtime optimizations this style has many other benefits related to the ease of instrumentation: history, reversibility, stepwise debugging, query statistics -- all of these are easy to add dynamically because we control the interpreter and have left it as a virtual machine evaluator instead of reducing the program to a single thunk.


## Interpreter, unveiled

```javascript
Dagoba.Q.run = function() {                             // a machine for query processing

  var max = this.program.length - 1                     // index of the last step in the program
  var maybe_gremlin = false                             // a gremlin, a signal string, or false
  var results = []                                      // results for this particular run
  var done = -1                                         // behindwhich things have finished
  var pc = max                                          // our program counter

  var step, state, pipetype

  while(done < max) {
    step = this.program[pc]                             // step is an array: first the pipe type, then its args
    state = (this.state[pc] = this.state[pc] || {})     // the state for this step: ensure it's always an object
    pipetype = Dagoba.getPipetype(step[0])              // a pipetype is just a function
```

Here `max` is just a constant, and `step`, `state`, and `pipetype` cache information about the current step. We've entered the driver loop, and we won't stop until the last step is done.

```javascript    
    maybe_gremlin = pipetype(this.graph, step[1], maybe_gremlin, state)
```

Calling the step's pipetype function with its arguments.
    
```javascript    
    if(maybe_gremlin == 'pull') {                       // 'pull' tells us the pipe wants further input
      maybe_gremlin = false
      if(pc-1 > done) {
        pc--                                            // try the previous pipe
        continue
      } else {
        done = pc                                       // previous pipe is finished, so we are too
      }
    }
```

To handle the 'pull' case we first set `maybe_gremlin` to false. We're overloading our 'maybe' here by using it as a channel to pass the 'pull' and 'done' signals, but once one of those signals is sucked out we go back to thinking of this as a proper 'maybe'. [footnote: We call it `maybe_gremlin` to remind ourselves that it could be a gremlin, or it could be something else. Also because originally it was either a gremlin or Nothing.]

If the step before us isn't 'done' [footnote: Recall that done starts at -1, so the first step's predecessor is always done.] we'll move the head backward and try again. Otherwise, we mark ourselves as 'done' and let the head naturally fall forward.

```javascript
    if(maybe_gremlin == 'done') {                       // 'done' tells us the pipe is finished
      maybe_gremlin = false
      done = pc
    }    
```

Handling the 'done' case is even easier: set `maybe_gremlin` to false and mark this step as 'done'.

```javascript
    pc++                                                // move on to the next pipe
    
    if(pc > max) {
      if(maybe_gremlin)
        results.push(maybe_gremlin)                     // a gremlin popped out the end of the pipeline
      maybe_gremlin = false
      pc--                                              // take a step back
    }
  }
```

We're done with the current step, and we've moved the head to the next one. If we're at the end of the program and `maybe_gremlin` contains a gremlin then we'll add it to the results, set `maybe_gremlin` to false and move the head back to the last step in the program. 

This is also the initialization state, since `pc` starts as `max`. So we start here and work our way back, and end up here again at least once for each final result the query returns.

```javascript
  results = results.map(function(gremlin) {             // return either results (like property('name')) or vertices
    return gremlin.result != null 
         ? gremlin.result : gremlin.vertex } )

  return results
}
```

We're out of the driver loop now: the query has ended, the results are in, and we just need to process and return them. If any gremlin has its result set we'll return that, otherwise we'll return the gremlin's final vertex. Are there other things we might want to return? What are the tradeoffs here? 


## Query transformers

So we have this nice compact little interpreter for our query programs now, but we're still missing something. Every modern DBMS comes with a query optimizer as an essential part of the system. For non-relational databases optimizing our query plan rarely yields the exponential speedups seen in their relational cousins [footnote: Or, put more succinctly, a poorly phrased query is less likely to yield exponential time slowdowns over an alternate phrasing of the same query. As an end-user of an RDBMS the aesthetics of query quality can often be quite opaque.], but it's still an important aspect of database design.

What's the simplest thing we could do that could reasonably be called a query optimizer? Well, we could write little functions for transforming our query programs before we run them. We'll pass a program in as input and get a different program back out as output. 

```javascript
Dagoba.T = []                                           // transformers (more than meets the eye)

Dagoba.addTransformer = function(fun, priority) {
  if(typeof fun != 'function')
    return Dagoba.error('Invalid transformer function') 
  
  for(var i = 0; i < Dagoba.T.length; i++)              // OPT: binary search
    if(priority > Dagoba.T[i].priority) break
  
  Dagoba.T.splice(i, 0, {priority: priority, fun: fun})
}
```

Now we can add query transformers to our system. A query transformer is a program->program function, plus a priority level. Higher priority transformers are placed closer to the front of the list. We're ensuring fun is a function, because we're going to evaluate it later. [footnote: An astute reader will also notice that we're keeping the domain of the priority parameter open, so it can be an integer, a rational, a negative number, or even things like Infinity or NaN.]

We'll assume there won't be an enormous number of transformer additions, and walk the list linearly to add a new one. We'll leave a note in case this assumption turns out to be false -- a binary search is much more time optimal for long lists, but doesn't speed up short lists and adds a little complexity.

To run these transformers we're going to inject a single line of code in to the top of our interpreter:

```javascript
Dagoba.Q.run = function() {                             // our virtual machine for query processing
  this.program = Dagoba.transform(this.program)         // activate the transformers
```

And use that to call this function, which just passes our program through each transformer in turn.

```javascript
Dagoba.transform = function(program) {
  return Dagoba.T.reduce(function(acc, transformer) {
    return transformer.fun(acc)
  }, program)
}
```

Our engine up until this point has traded simplicity for performance, but one of the nice things about this strategy is that it leaves doors open for global optimizations that may have been unavailable if we had opted to locally optimize as we designed the system. 

Optimizing a program frequently increases complexity and reduces the elegance of the system, making it harder to reason about and maintain the system. Breaking abstraction barriers for performance reasons is one of the more painful ways this occurs, but even something seemingly innocuous like embedding performance-oriented code into a business logic function makes maintenance more difficult.

In light of that, this type of "orthogonal optimization" is particularly appealing. We can add optimizers in modules or even user code, instead of having them tightly coupled to the engine. We can test them in isolation, or in groups, and with the addition of generative testing we could even automate that process, ensuring the our available optimizers play nicely together.

We can also use this transformer system to add new functionality unrelated to optimization. Let's look at one of those now.


## Aliases

Making a query like `g.v('Thor').out().in()` is really compact, but is this Thor's siblings or his mates? Neither way is fully satisfying. It'd be nicer to really say what mean: either `g.v('Thor').parents().children()` or `g.v('Thor').children().parents()`.

We can use query transformers to make aliases with just a couple extra helper functions:

```javascript
Dagoba.addAlias = function(newname, oldname, defaults) {
  defaults = defaults || []                             // default arguments for the alias
  Dagoba.addTransformer(function(program) {
    return program.map(function(step) {
      if(step[0] != newname) return step
      return [oldname, Dagoba.extend(step[1], defaults)]
    })
  }, 100)                                               // these need to run early, so they get a high priority
  Dagoba.addPipetype(newname, function() {})            // because there's no method catchall in js
}

```

We're adding a new name for an existing step, so we'll need to create a query transformer that converts the new name to the old name whenever it's encountered. We'll also need to add the new name as a method on the main query object, so it can be pulled in to the query program.

We call another helper function to merge the incoming step's arguments with the alias's default arguments. Whenever the incoming step is missing an argument that the alias provides we take the alias's argument for that slot.

```javascript
Dagoba.extend = function(list, defaults) {
  return Object.keys(defaults).reduce(function(acc, key) {
    if(typeof list[key] != 'undefined') return acc
    acc[key] = defaults[key]
    return acc
  }, list)
}
```

Now we can make those aliases we wanted:

```javascript
Dagoba.addAlias('parents', 'out')
Dagoba.addAlias('children', 'in')
```

We can also start to specialize our data model a little more, by labeling each edge between a parent and child as a 'parent' edge. Then our aliases would look like this:

```javascript
Dagoba.addAlias('parents', 'out', ['parent'])
Dagoba.addAlias('children', 'in', ['parent'])
```

Now we can start adding edges for spouses, step-parents, or even jilted ex-lovers. If we enhance our addAlias function a little we can introduce new aliases for siblings, grandparents, or even cousins:

```javascript
Dagoba.addAlias('siblings', [['out', 'parent'], ['in', 'parent']])
Dagoba.addAlias('grandparents', [['out', 'parent'], ['out', 'parent']])
Dagoba.addAlias('cousins', [['out', 'parent'], ['as', 'folks'], ['out', 'parent'], ['in', 'parent'], ['except', 'folks'], ['in', 'parents'], ['unique']])
```

That `cousins` alias is a little cumbersome. Maybe we could expand our addAlias function to allow ourselves to use other aliases in our aliases, and then call it like this:

```javascript
Dagoba.addAlias('cousins', ['parents', ['as', 'folks'], 'parents', 'children', ['except', 'folks'], 'children', 'unique'])
```

Now instead of `g.v('Forseti').parents().as('parents').parents().children().except('parents').children().unique()` we can just say `g.v('Forseti').cousins()`.

We've introduced a bit of a pickle, though: while our addAlias function is resolving an alias it also has to resolve other aliases. What if `parents` called some other alias, and while we were resolving `cousins` we then had to stop to resolve `parents` and then resolve its aliases and so on? What if one of `parents` aliases ultimately called `cousins`?

This bring us in to the realm of dependency resolution, a core component of modern package managers. There are a lot of fancy tricks for choosing ideal versions, tree shaking, general optimizations and the like, but the basic idea is fairly simple. We're going to make a graph of all the dependencies and their relationships, and then try to find a way to line all of the vertices up while making the arrows go from left to right. If we can, then this particular sorting of the vertices is called a 'topological ordering', and we've proven that our dependency graph has no cycles: it is a Directed Acyclic Graph (DAG). If we fail to do so then our graph has at least one cycle. [footnote: You can learn more about dependency resolution in the Contingent chapter of this book.]

On the other hand, we expect that our queries will generally be rather short (100 steps would be a very long query) and that we'll have a reasonably low number of transformers. Instead of fiddling around with DAGs and dependency management we could add a 'did_something' return value to the transform function and run it until it stops doing anything. This requires that all transformers be idempotent, but that's a helpful property to insist on anyway. What are the pros and cons of these two pathways?


## Performance

All production graph databases share a very particular performance characteristic: graph traversal queries are constant time with respect to total graph size. [footnote: The fancy term for this is "index-free adjacency".] In a non-graph database, asking for the list of someone's friends can require time proportional to the number of entries, because in the naive worst-case you have to look at every entry. The means if a query over ten entries takes a millisecond then a query over ten million entries will take almost two weeks. Your friend list would arrive faster if sent by Pony Express! [footnote: Though only in operation for 18 months due to the arrival of the transcontinental telegraph and the outbreak of the American Civil War, the Pony Express is still remembered today for delivering mail coast to coast in just ten days.]

To alleviate this dismal performance most databases index over oft-queried fields, which turns an O(n) search into an O(log n) search. This gives considerably better search performance, but at the cost of some write performance and a lot of space -- indices can easily double the size of a database. Careful balancing of the space/time tradeoffs of indices is part of the perpetual tuning process for most databases.

Graph databases sidestep this issue by making direct connections between vertices and edges, so graph traversals are just pointer jumps: no need to scan through every item, no need for indices, no extra work at all. Now finding your friends has the same price regardless of the total number of people in the graph, with no additional space cost or write time cost. One downside to this approach is that the pointers work best when the whole graph is in memory on the same machine. Effectively sharding a graph database across multiple machines is still an active area of research. [footnote: Sharding a graph database requires partitioning the graph. Optimal graph partitioning is NP-hard, even for simple graphs like trees and grids, and even good approximations have exponential asymptotic complexity. [http://arxiv.org/pdf/1311.3144v2.pdf, http://dl.acm.org/citation.cfm?doid=1007912.1007931] ]

We can see this at work in the microcosm of Dagoba if we replace the functions for finding edges. Here's a naive version that searches through all the edges in linear time. It harkens back to our very first implementation, but uses all the structures we've since built.

```javascript
Dagoba.G.findInEdges  = function(vertex) { return this.edges.filter(function(edge) {return edge._in._id  == vertex._id} ) }
Dagoba.G.findOutEdges = function(vertex) { return this.edges.filter(function(edge) {return edge._out._id == vertex._id} ) }
```

We can add an index for edges, which gets us most of the way there with small graphs but has all the classic indexing issues for large ones.

```javascript
Dagoba.G.findInEdges  = function(vertex) { return this.inEdgeIndex [vertex._id]  }
Dagoba.G.findOutEdges = function(vertex) { return this.outEdgeIndex[vertex._id] }
```

And here we have our old friends back again: pure, sweet index-free adjacency.

```javascript
Dagoba.G.findInEdges  = function(vertex) { return vertex._in  }
Dagoba.G.findOutEdges = function(vertex) { return vertex._out }
```

Run these yourself to experience the graph database difference.

[footnote: In modern JavaScript engines filtering a list is quite fast -- for small graphs the naive version can actually be faster than the index-free version due to the way the code is JIT compiled and the underlying data structures. Try it with different sizes of graphs to see how the two approaches scale.]


## Serialization

Having a graph in memory is great, but how do we get it there in the first place? We saw that our graph constructor can take a list of vertices and edges and create a graph for us, but once the graph has been built how do we get the vertices and edges back out?

Our natural inclination is to do something like `JSON.stringify(graph)`, which produces the terribly helpful error `TypeError: Converting circular structure to JSON`. During the graph construction process the vertices were linked to their edges, and the edges are all linked to their vertices, so now everything refers to everything else. So how can we extract our nice neat lists again? JSON replacer functions to the rescue.

The `JSON.stringify` function takes a value to stringify, but it also takes two additional parameters: a replacer function and a whitespace number [footnote: Pro tip: given a deep tree deep_tree, running `JSON.stringify(deep_tree, 0, 2)` in the JS console is a quick way to make it human readable]. The replacer allows you to customize how the stringification proceeds. 

We need to treat the vertices and edges a bit differently, so we're going to manually merge the two sides into a single JSON string.

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

The only difference between them is what they do when a cycle is about to be formed: for vertices, we skip the edge list entirely. For edges, we replace each vertex with its id. That gets rid of all the cycles we created while building the graph.

We're manually manipulating JSON in `Dagoba.jsonify`, which generally isn't recommended as the JSON format is insufferably persnickety. Even in a dose this small it's easy to miss something and hard to visually confirm correctness.

We could merge the two replacer functions into a single function, and then use that new replacer function over the whole graph by doing `JSON.stringify(graph, my_cool_replacer)`. This frees us from having to manually massage the JSON output, but the resulting code may be quite a bit messier. Try it yourself and see if you can come up with a well-factored solution that avoids hand-coded JSON. (Bonus points if it fits in a tweet.)


## Persistence

Persistence is usually one of the trickier parts of a database: disks are relatively safe, but dreadfully slow. Batching writes, making them atomic, journaling -- all of these are difficult to make both fast and correct.

Fortunately, we're building an *in-memory* database, so we don't have to worry about any of that! We may, though, occasionally want to save a copy of the database locally for fast restart on page load. We can use the serializer we just built to do exactly that. First let's wrap it in a helper function:

```javascript
Dagoba.G.toString = function() { return Dagoba.jsonify(this) }
```

In JavaScript an object's `toString` function is called whenever that object is coerced into a string. So if `g` is a graph, then `g+''` will be the graph's serialized JSON string.

The `fromString` function isn't part of the language specification, but it's handy to have around.

```javascript
Dagoba.fromString = function(str) {                     // another graph constructor
  var obj = JSON.parse(str)                             // this can throw
  return Dagoba.graph(obj.V, obj.E) 
}
```

Now we'll use those in our persistence functions. The `toString` function is hiding -- can you spot it?

```javascript
Dagoba.persist = function(graph, name) {
  name = name || 'graph'
  localStorage.setItem('DAGOBA::'+name, graph)
}

Dagoba.depersist = function (name) {
  name = 'DAGOBA::' + (name || 'graph')
  var flatgraph = localStorage.getItem(name)
  return Dagoba.fromString(flatgraph)
}
```

We preface the name with a faux namespace to avoid polluting the localStorage properties of the domain, as it can get quite crowded in there. There's usually a low storage limit also, so for larger graphs we'd probably want to use a Blob of some sort. 

There are also potential issues if multiple browser windows from the same domain are persisting and depersisting simultaneously. The localStorage space is shared between those windows, and they're potentially on different event loops, so there's the possibility for one to carelessly overwrite the work of another. The spec says there should be a mutex required for read/write access to localStorage, but it's inconsistently implemented between different browsers, and even with it a naive implementation like ours could still encounter issues.

If we wanted our persistence implementation to be multi-window concurrency aware we could make use of the storage events that are fired when localStorage is changed to update our local graph accordingly. 


## Updates

Our 'out' pipetype copies the vertex's out-going edges and pops one off each time it needs one. Building that new data structure takes time and space, and pushes more work on to the memory manager. We could have instead used the vertex's out-going edge list directly, keeping track of our place with a counter variable. Can you think of a problem with that approach?

Well, if someone deletes an edge we've visited while we're in the middle of a query, that would change the size of our edge list, and we'd then skip an edge because our counter is off. To solve this we could lock all of the vertices involved our query, but then we'd either lose our capacity to regularly update the graph or the ability to have long-lived query objects responding to requests for more results on-demand. Even though we're in a single-threaded event loop, our queries can span multiple asynchronous re-entries, which means concurrency concerns like this are a very real problem.

So we'll pay the performance price to copy the edge list. There's still a problem, though, in that long-lived queries may not see a completely consistent chronology. We will traverse every edge a vertex had at the time we first visit it, but we may visit vertices at different clock times during our query. Suppose we save a query like `var q = g.v('Thor').children().children().take(2)` and then call `q.run()` to gather two of Thor's grandchildren. Some time later we need to pull another two grandchildren, so we call `q.run()` again. If Thor has had a new grandchild in the intervening time, we may or may not see it, depending on whether the parent vertex was visited the first time we ran the query.

One way to fix this non-determinism is to change the update handlers to add versioning to the data. We'll then change the driver loop to pass the graph's current version in to the query, so we're always seeing a consistent view of the world as it existed when the query was first initialized. Adding versioning to our database also opens the door to true transactions, and automated rollback/retries in an STM-like fashion. 


## Future directions

We saw one way of gathering ancestors earlier: `g.v('Thor').out().as('parent').out().as('grandparent').out().as('great-grandparent').merge(['parent', 'grandparent', 'great-grandparent']).run()`

This is pretty clumsy, and doesn't scale well -- what if we wanted six layers of ancestors? Or to look through an arbitrary number of ancestors until we found what we wanted?

It'd be nice if we could say something like this instead: `G.v('Thor').out().all().times(3).run()`. What we'd like to get out of this is something like the query above -- maybe `g.v('Thor').out().as('a').out().as('b').out().as('c').merge(['a', 'b', 'c']).run()` after the query transformers have all run.

We could run the `times` transformer first, to produce `G.v('Thor').out().all().out().all().out().all().run()`. Then run the `all` transformer and have it transform each `all` into a uniquely labeled `as`, and put a `merge` after the last `as`. 

There's a few problems with this, though. For one, this as/merge technique only works if every pathway is present in the graph -- if we're missing an entry for one of Thor's great-grandparents that will limit our results. For another, what happens if we want to do this to just part of a query and not the whole thing? What if there are multiple `all`s?  

To solve that first problem we're going to have to treat `all`s as something more than just as/merge. We need each parent gremlin to actually skip the intervening steps. We can think of this as a kind of teleportation -- jumping from one part of the pipeline directly to another -- or we can think of it as a certain kind of branching pipeline, but either way it complicates our model somewhat. Another approach would be to think of the gremlin as passing through the intervening pipes in a sort of suspended animation, until reawoken by a special pipe. Scoping the freezing/thawing pipes may be tricky, however.

The next two problems are easier: to modify just part of a query we'll wrap that portion in special start/end steps, like `g.v('Thor').out().start().in().out().end().times(4).run()`. Actually, if the interpreter knows about these special pipetypes we don't need the end step, because the end of a sequence is always a special pipetype. We'll call these special pipetypes 'adverbs', because they modify regular pipetypes like adverbs modify verbs. 

To handle multiple `all`s we need to run all `all` transformers twice: one time before the times transformer, to mark all `all`s uniquely, and again after times' time to remark all marked `all`s uniquely all over.

There's still the issue of searching through an unbounded number of ancestors -- for example, how do we find out which of Ymir's descendants are scheduled to survive Ragnarök? We could make individual queries like `g.v('Ymir').in().filter({survives: true})` and `g.v('Ymir').in().in().in().in().filter({survives: true})` and manually collect the results ourselves, but that's pretty awful. 

We'd like to use an adverb like this: `g.v('Ymir').in().filter({survives: true}).every()`, which would work like `all`+`times` but without enforcing a limit. We may want to impose a particular strategy on the traversal, though, like a stolid BFS or YOLO DFS, so `g.v('Ymir').in().filter({survives: true}).bfs()` would be more flexible. Phrasing it this way allows us to state complicated queries like "check for Ragnarök survivors, skipping every other generation" in a straightforward fashion: `g.v('Ymir').in().filter({survives: true}).in().bfs()`.


## Wrapping up

So what have we learned? Graph databases are great for storing interconnected* data that you plan to query via graph traversals. Adding non-strict semantics allows for a fluent interface over queries you could never express in an eager system for performance reasons, and allows you to cross async boundaries. Time makes things complicated, and time from multiple perspectives (i.e. concurrency) makes things very complicated, so whenever we can avoid introducing a temporal dependency (e.g. state, observable effects, etc) we make reasoning about our system easier. Building in a simple, decoupled and painfully unoptimized style leaves the door open for global optimizations later on, and using a driver loop allows for orthogonal optimizations -- each without introducing the brittleness and complexity into our code that is the hallmark of most optimization techniques. 

That last point can't be overstated: keep it simple. Eschew optimization in favor of simplicity. Work hard to achieve simplicity by finding the right model. Explore many possibilities. The chapters in this book provide ample evidence that highly non-trivial applications can have a small, tight kernel. Once you find that kernel for the application you are building, fight to keep complexity from polluting it. Build hooks for attaching additional functionality, and maintain your abstraction barriers at all costs. Using these techniques well is not easy, but they can give you leverage over otherwise intractable problems. 


[footnote on interconnected: Not *too* interconnected, though -- you'd like the number of edges to grow in direct proportion to the number of vertices. In other words the average number of edges connected to a vertex shouldn't vary with the size of the graph. Most systems we'd consider putting in a graph database already have this property: if we add 100,000 Nigerian films to our movie database that doesn't increase the degree of the Kevin Bacon vertex.]


### Acknowledgements

Many thanks are due to Michael DiBernardo, Colin Lupton, Scott Rostrup, Michael Russo, Erin Toliver, and Leo Zovik for their invaluable contributions to this chapter.
