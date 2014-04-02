DBDB
====

Dog Bed Database (it's like a couch, but not as nice).


Intro to problem
----------------

Most projects require a data store of some kind.
You really shouldn't write your own;
there are many edge-cases that will bite you in the end.
However,
it's incredibly useful to understand
how they work.
This will inform your use of it
from a performance
as well as crash recovery/durability
perspective.


Simplifications
---------------

DBDB tries to show the basic patterns you can use
to create a key/value store
with atmoic, consistent, durable updates. 

It separates the concerns of "put this on disk somewhere"
(how data are laid out in a file)
from the logical structure of the data
(a binary tree in this example)
from the contents of the key/value store
(the association of key "a" to value "foo").


Intro to toolchain (maybe optional?)
------------------------------------

The example is written in Python 2
(THOUGH I WOULD LIKE IT TO WORK IN 2 AND 3 BOTH BEFORE THIS IS PUBLISHED).
Tests are driven using nosetests.


Explore code
------------

- Organizational units

``tool.py`` defines
a CLI tool for exploring a database.

``interface.py`` defines
a simple front-end interface
that acts like a Python dictionary.

``tree.py`` defines
lower level interfaces
and provides primitives
for building a data store
on top of a storage abstraction.

``binary_tree.py`` defines
a concrete binary tree algorithm
underneath the tree interface
using the storage abstraction.

``storage.py`` defines
addressable, persistent byte storage.

- Why?

Single Responsibility Principle,
which translates to
"a piece of code should have only one reason to change".

- Points of extensibility

The algorithm used to update the data store
can be completely changed out
by replacing the string ``BinaryTree`` in ``interface.py``.
In particular,
data stores of this nature tend to use B-trees
not binary trees
to improve the nodes-per-byte ratio
of the tree.

The choice of serializer for values
is restricted to
the ``ValueRef`` implementation.
The binary tree nodes themselves
are just a sublcass of ``ValueRef``.


- Tradeoffs (time/space, performance/readability)

The binary tree is easier to write
and hopefully easier to read
than a B-tree would have been,
but I/O performance of a B-tree
ought to be superior
for most workloads.

- Patterns or principles that can be used elsewhere

Test interfaces, not implementation.


Conclusion

- Futher extensions to make

 - Full and proper multi-client, non-clobbering support.
   Concurrent dirty readers already "just work",
   but concurrent writers,
   and readers-who-then-write
   could cause problems.
   Beyond making the implementation "safe",
   it's important to provide a useful
   and hard-to-use-incorrectly
   interface to users.

 - Database compaction.
   Compacting should be as simple as
   doing an infix-of-median traversal of the tree
   writing things out as you go.
   It's probably best if the tree nodes all go together,
   since they're what have to be traversed
   to find any piece of data.
   Packing as many intermediate nodes as possible
   into a disk sector
   should improve read performance
   at least right after compaction.

 - If compaction is enabled,
   it's probably not useful to truncate uncommitted writes
   on crash recovery.

   
- Similar real-world projects to explore

CouchDB
