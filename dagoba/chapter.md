So we're going to build a graph database. We should probably figure out what that means first. 

A database is a place to put data. You can read from it and you can write to it.

A graph in this sense is a set of vertices and a set of edges. In other words it's a bunch of dots connected by lines. 

So what's the simplest thing we can possibly build that could technically be called a "graph database"? We need a set of vertices and a set of edges. We need a way to write a new vertex or a new edge, and we need a way to read it.

So let's draw a shape: [stick man]. We'll number our vertices so we can keep track of them -- remember that graphs have no inherent "shape", so these graphs are all equivalent. 

We're in JS, so let's make an array [1] of vertices: 

  var vertices = [1,2,3,4,5,6]
  
And an array of edges:
  
  var edges = [ [1,2], [2,3], [3,1], [3,4], [4,5], [4,6] ]

Notice that we're modeling edges as a pair of vertices. Also notice that those pairs are ordered, because we're using arrays. That means we're modeling a *directed graph*, where every edge has a start and an end. [lines have arrows] We're doing it this way mostly because it's easier given our datastructure options, but it also adds a little complexity to our mental model because we have to keep track of the direction of edges. [2]

Cool, we have a graph! Now for the database side -- we'll need some way to add new nodes and edges:
--> maybe we don't need this yet...
//  function addNode (node) { vertices.push(node) }
//  function addEdge (edge) { edges.push(edge) }

And some way to get information back out... what kinds of questions do we want to ask? We can ask about who our neighbors are:

  function into (node) 
    { return edges.reduce( function(acc, edge) 
      { return (edge[1] === node) ? acc.concat(edge[0]) : acc }, [] )}
  function out (node) 
    { return edges.reduce( function(acc, edge) 
      { return (edge[0] === node) ? acc.concat(edge[1]) : acc }, [] )}
  function neighbors (node) { return into(node).concat(out(node)) }

simpler:
  function matchAndConcat (node, side) 
    { return function (acc, edge) 
      { return (edge[side] === node) ? acc.concat( edge[1-side] ) : acc }}

  function into (node) { return edges.reduce( matchAndConcat(node, 1) )}

second neighbors:
  neighbors(4).map(neighbors).reduce(function(acc, nodes) {return acc.concat(nodes)}, [])

wouldn't that be nicer if we could just say 

  neighbors(neighbors(4))
  
sure, we can do that. just make everything take nodes instead of node

what if we want to filter the nodes we visit?

  function filter (nodes, fun) { return nodes.filter(fun) }


Or we could ask about the shortest path from one vertex to another:

  function shortestPath (node) {}

Let's run some tests! 

...

maybe we load in the collatz sequence as a test graph? http://xkcd.com/710/ [it's a tree, less interesting]
prime factors graph. [it's too connected, perhaps less interesting]
random graph (add 1-10, then each new node connects to 3 existing nodes selected at random) [just right (goldilocks graph)]
Watts-Strogatz small-world model simulator
Barabási–Albert model
Dorogovtsev and Mendes : http://arxiv.org/pdf/1007.4031v1.pdf

This is pretty good, but what if we care about more than just numbers? String labels should work fine. What about objects? 

  alice, bob, charlie, delta
  var nodes = [ alice, bob, charlie, delta ]
  var edges = [ [alice, bob], [bob, charlie], [charlie, delta], [charlie, alice], [bob, delta] ]

  neighbors... path... 

Hey, still works!

This is great, but what if we could use the vertex attributes in our query?

  ask about friends of charlie with blah blahs
  
Fun! But what if we want to ask about all of Alice's employees? Let's add some edge attributes.

  edge -> {_inV: A_id, _outV: B_id, _label: "employed", _id: 123, foo: "bar"}
  
[and of course a way to filter on edges / nodes...]

// TODO: change all node -> vertex   (/sigh)
// THINK: can you put the _id in the node/edge? does addNodes return a list of ids or a list of nodes? 
// property graph model

var g = Dagoba.graph()
g.addNodes([ {_id:1, name:"Fred"}, {_id:2, name:"Bob"}, {_id:3, name:"Tom"}, {_id:4, name:"Dick"}, {_id:5, name:"Harry"} ])
g.addEdges([ {_inV: 1, _outV: 2, _label: "father"}, {_inV: 2, _outV: 3, _label: "father"}, {_inV: 2, _outV: 4, _label: "father"}, {_inV: 2, _outV: 5, _label: "father"} ])

g.v(1).out().out('father').name    // TODO: check proxies for this -- can we use them? otherwise...
g.v(1).out().out('father').attr('name')
g.v(1).out().out('father').attr('name').paths()

so each of these g.xxx things is actually just adding elements to a query list, which can be optimized when something calls it and everything happens lazily. how do we do that?




make it right:

there's a problem here though -- it's really cumbersome to combine these questions. What if we wanted the nearest neighbor with a foo of bar?

use filters to create subgraphs...

a fluent style for creating subgraphs...

modularlize it so you can have multiple graphs and a nice api...

and allow query filters (and later optimizers) to be added at runtime...

then make it fast:

generate some graphs for testing...

put edge pointers inside nodes to optimize...

laziness for orthogonal optimization on the query pipeline...

more generated graphs for testing -- much faster!

non-enumerable properties for storing edges on nodes?

http://christopherolah.wordpress.com/2011/04/11/arithmetic-derivative-graph-and-thoughts/



[0] We use 'vertex' in our discussion because it sounds better, and 'node' in our code, to prevent pluralization typos from eating our lunch.

[1] ES6 has proper sets, so if you're reading this from the future you can upgrade the datastructure:
  Set blah set set foo

[2] The upside of adding that complexity is that we can ask more interesting questions, like "which vertices point in to vertex 3?" or "which vertex has the most outgoing edges?". Also note that we can model an undirected graph by doubling up our edges array:

  function undirectMe (edges) 
    { return edges.reduce(function(acc, edge)
      { acc.push([ edge[0], edge[1] ])
        acc.push([ edge[1], edge[0] ]) }, []) }
  
  
  
  
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




















Definitions












# Querying

So now we've got a database and we'd like to make a query. Ideally, we'd say something like:
> Starting from vertex 5, follow every even* outgoing edge, then follow every odd* outgoing edge, then come back in on even* edges.

Ok, so how do we do that in js? well, maybe something like this:

Graph.V(5).out(even).out(odd).in(even)

[define even and odd lambdas]
And we can just pass values along:
V(5) sends an array containing vertex 5, and out and in both accept and return arrays of vertices. 
And this works great, until our graph gets large and connected and we write something like:
G.V(5).out().out().out().take(5)
and run out of memory before Dave Brubeck gets to play.

So we're going to need LAZINESS. 

How do we get laziness in JS? Well, we're using ES6 features, so generators come to mind. That could look like this.

-- good idea, but: can't go backward, can't orthopt, 

--> query transformer: id() -> attr('_id')


--> then we add label matching for easy edge walking
--> then we add object queries for more complicated stuff

# Updating

concurrency. immutable data structures. update syntax -- no problem, can't update directly anyway. {'foo.baz.smile': 123, 'args.lala.gogo': 333}



# Performance



