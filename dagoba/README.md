# Dagoba

An extensible in-memory graph database
Author: dann toliver

TODO: Everything.





 Gremlin Examples and Code Snippets
Gremlin is a powerful domain specific traversal language for graph databases. This language is supported by all popular graph databases. 

In recent past, I was doing some case studies on graph databases and realized that gremlin is the graph language that I must learn. Learning this language can ensure you to be able to work on multiple graph databases.

Gremlin works smoothly on all graph databases that support Blueprints property graph data model. Gremlin can be easily used with JVM languages like Groovy, Clojure, Scala and more.

A lot of graph databases support their custom languages (e.g. Cipher in Neo4j). These languages are really useful, however they become useless on other databases.

Learning Gremlin for graph databases is equivalent to learning SQL for relational databases. Once you know SQL, you can easily work on MySQL or Postgres or Oracle without worrying about details of vendors. Same way knowing Gremlin can ensure you can work on Neo4j or Titan Graph DB, or OrientDB and other dozens of graph databases. 

In this simple code snippets collection, I am trying to list down all commonly used snippets that you can use as a cheatsheet while doing graph query, analysis and manipulation.




Some Useful Graph Traversal Queries

These queries are focused on reading the data from a graph database. We have taken simple examples to demonstrate the commands, however you can try various complex combinations and come up with really complex queries.
Simple graph used in most of the queries on this page



Get A Vertex By Its Id

This is probably the simplest query. Make sure the "v" in query is lower case. The upper case "V" has different use.

g.v(1);


Get All The Vertices With ID Range


Lets say you want to get all the vertex that has id in the range of 1 to 100

To get the vertex object itself you can use below query
g.V[1..100]

To get a specific attribute of each vertex, lets say firstName. You can use below query

g.V[1..100].firstName




Get The Attribute Of A Vertex By Its Id

Lets say the attribute name is "firstName" then we can use below query to find first name of a vertex id 1

g.v(1).firstName;

This can be done with any custom attribute you may have added in your node. Lets say "lastName" can be obtained by this 

g.v(1).lastName;



Get A Vertex By An Attribute

Lets say we want to find a vertex (or all vertices) that have "firstName" as "John", below query can be used.
Note the V in this case is upper case. The lowercase v will not work with attribute query.

g.V('firsName','John');


Get The Id Of A Vertex By An Attribute

id attribute is a inbuilt attribute that is always available for any vertex. So even if you did not add a id attribute, graph db is going to assign at value to uniquely identify a vertex. You can find out the id of a vertex by this query. 

g.V('firsName','John').id;

Get The Count Of Vertices With A Attribute Value

Lets say you just want to know how may people in you database with first name "John". This simple query can be done like below.

g.V('firsName','John').count();



Get The Edge Of A Vertex With A Label "friend"

Now starts the real fun with graph language. You may already know that you can have outgoing or incoming relations in a graph.

Lets say you want to know all the out going edges with label "friend" .

g.v(1).outE('friend');

Lets say you want to know all the in coming edges with label "friend" .

g.v(1).inE('friend');

Lets say you want to know all the in coming and out going edges with label "friend" .

g.v(1).bothE('friend');

Get The Count Of Edges Of A Vertex With Label "friend"

Count is a useful function that can be applied on vertices and edges to count them instead of getting the object itself.

Lets say you want to know, how many friends are connected to a node id 1

g.v(1).outE('friend').count();
Get The Label Of All Out Going Edges Of A Vertex

Just like vertex, you can also fetch the attributes of any edge using the name of the attribute. 

g.v(1).outE().label;

Get The Count Of All Out Going Edges Of A Vertex

Lets say you want to know, how many edges are connected to a node id 1 (irrespective of relation label)

g.v(1).outE().count();

Get The First Names Of All People That Are Connected By A Friend Relation

This query should return the first names of all people that are friend to a vertex id 1

g.v(1).outE('friend').inV().firstName;



Get The Age Of All Friends

In this queury we can get the age of all people that are connected to a person (with last name 'Doe') by a friend relation
This is another combination to demonstrate the starting point can be a property and any attribute of a connected vertex can be obtained. For example we are extracting age of all friends of all persons that have last name Doe.

g.V('lastName','Doe').outE('friend').inV().age;
Get All Friends With Age 25

In this example you can get first name of all friends of a vertex id 1 that are connected by a friend relation

g.v(1).outE('friend').inV().has('age',25).firstName;


Find All People That Have Age Greater Than 25

This may be a really common scenario, where you may want to target people with specific age group.

g.V.filter{it.age > 25}.firstName

Find All People That Have Age Greater Than 25 And Less Than 35


g.V.and(_().has("age", T.gt, 25), _().has("age", T.lt, 35));

or a simpler alternative exists with interval function

g.V.interval("age", 25, 35);


Get All People That Have Email Address

This is a reverse approach for checking all the people do not have email address as null 

g.V.hasNot('email', null)

On the other hand you can do the opposite very easily to find people who do not have email address by simply using has function instead of hasNot

g.V.has('email', null)


Get Unique Results On A Complex Query

This can be done by the dedup function. 

g.V('lastName','Doe').outE('friend').inV().dedup();


Get The Count Of Unique Results On A Complex Query

This can be achieved easily by using count function. 

g.V('lastName','Doe').outE('friend').inV().dedup().count();




Graph Manipulation Queries In Gremlin

These queries can be used to do manipulation of graph from the gremlin console or api.


Add A New Vertex In The Graph

g.addVertex([firstName:'John',lastName:'Doe',age:'25']);
g.commit();

Add Two New Vertex And A Relation (with Label 'friend') Between Them

This will require multiple statements. Note how the variables (jdoe and mj) are defined just by assigning them a value from gremlin query. 

jdoe = g.addVertex([firstName:'John',lastName:'Doe',age:'25']); 
mj = g.addVertex([firstName:'Mary',lastName:'Joe',age:'21']);
g.addEdge(jdoe,mj,'friend');
g.commit();

Add A Relation Between Two Existing Vertices With Id 1 And 2

g.addEdge(g.v(1),g.v(2),'coworker');
g.commit();



Remove All Vertices From The Graph


g.V.each{g.removeVertex(it)}
g.commit();

Remove All Edges From The Graph


g.E.each{g.removeEdge(it)}
g.commit();


Remove All Vertices With FirstName = 'John'


g.V('firstName','John').each{g.removeVertex(it)}
g.commit();

Remove A Vertex With Id 1

g.removeVertex(g.v(1));
g.commit();

Remove An Edge With Id 1

g.removeEdge(g.e(1));
g.commit();


Create A Index Using Gremlin

This is to index the graph with specific field you may want to search frequently. Lets say "myfield" 

g.createKeyIndex("myfield",Vertex.class);

Note: The index creation can be done for not existing fields therefore incase you want to create a index for existing fields you may need to delete all and then create index.



I have recently started using gremlin and had tough time figuring out a few simple things on it. This language may be more intuitive for people who understand languages like groovy or scala. 

I have tried these queries thru Rexster (server version 2.3.0) gremlin console with Neo4j (version 1.8.2) as backend, however these should be functional in any other Blueprints graph like Titan or OrientDB or later versions of Neo4j as well.

Let me know your inputs if you see any issues on other databases. 

Can you think of more gremlin queries? Feel free to suggest in comments section.

Posted by Sachin FromDev
LIKE TO SHARE?
  
?
RELATED

43-3-2014
80+ Best Free Python Tutorials, eBooks & PDF To Learn Programming Online

610-29-2013
50+ Best Free C Tutorials, eBooks & PDF To Learn C Programming Online

28-14-2013
65+ Best Free Javascript Tutorials, eBooks & PDF For Web Developers
OLDER POST
15 Tips to Have Awesome LinkedIn Profile
NEWER POST
25 Best Kids Learning Android Apps For Your Child
POST A COMMENT DEFAULT COMMENTS FACEBOOK COMMENTS
 
Sachin FromDev
September 22, 2013 at 9:39 PM
I received below important comment from the Neo4j 
--------------
From: Philip Rathle - www.neo4j.org

When the history is written, we will look back at Gremlin as having played an important role as a first-generation graph database language, but not as becoming the lingua franca. There are a few reasons for this. Gremlin is first, an imperative language, requiring the end user to tell the database what to do. We expect more of a database, which is why SQL (for example) became the standard. Secondly, Gremlin is a Groovy scripting language, dragging along all the good and bad aspects of Groovy, with security and performance counting towards the latter. While it does work with most graph databases, it does not have standards momentum that SQL had. 

What to use then? This is a challenging question, as what we like best is a language that maximizes portability, performance, security, ease-of-use, and all of the other key considerations. The truth of the matter is that the graph database space is fairly new, and that the world is exploring new and better ways of querying and using graphs. There are many ways to get at a graph database today, and the advantages of using a native implementation (today) typically outweigh the costs one pays for portability. Neo4j's Cypher fits the "declarative" bill, and has gained rapid popularity for the ease of use that brings, but it's at this stage it's too early to propose it (or anything for that matter) as a standard. This is one thing that we (Neo4j) and most other graph providers agree upon. Standard will come--and there will be the inevitable good debate facilitated by standards bodies-- but that hasn't happened yet.

As to the possibility of having a 100% portable language... I predict we'll eventually get close, but if the RDBMS world is any example, 100% portability isn't likely to happen. Despite the ANSI SQL standards, each vendor has introduced enough of their own specific variations that SQL doesn't easily port between major vendors. This is particularly true when procedural SQL extensions are added. Oracle's PL/SQL is a wholly different beast than Microsoft's t-SQL. For high-performance applications, one nearly always ends up leveraging native features, like Oracle hints, index-only tables, etc., which likewise don't carry forward. So even the standards-centered databases with generations of maturity go only so practically so far when it comes to the age-old question of portability vs. performance.

Reply
Add comment
Individuals who comment on FromDev at regular basis, will be rewarded in Top Commenter section. (Comments are selectively moderated so please do not spam)
