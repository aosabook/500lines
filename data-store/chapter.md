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
as well as crash recovery
and durability perspectives.


Simplifications
---------------

DBDB tries to show the basic patterns used
to create a key/value store
with atomic, durable updates.
Consistency is not covered
(there are no constraints on the data stored)
and isolation is not guaranteed
(since dirty reads will not cause an abort on commit).

Intro to toolchain (maybe optional?)
------------------------------------

The example is written in Python 2
(THOUGH I WOULD LIKE IT TO WORK IN 2 AND 3 BOTH BEFORE THIS IS PUBLISHED).
Tests are driven using nosetests.


Explore code
------------

DBDB separates the concerns of "put this on disk somewhere"
(how data are laid out in a file)
from the logical structure of the data
(a binary tree in this example)
from the contents of the key/value store
(the association of key "a" to value "foo").


### Organizational units

* ``tool.py`` defines
    a CLI tool
    for exploring a database
    from a terminal window.

* ``interface.py`` defines
    a class (``DBDB``)
    which exposes an API
    like a Python dictionary
    using the concrete ``BinaryTree`` implementation
    for using DBDB inside a Python program.

* ``tree.py`` defines
    an abstract interface to a data store.

    - ``Tree`` is not tree-specific,
        and defers to a concrete sub-class to implement updates.
        It manages storage locking and dereferencing internal nodes.

    - ``ValueRef`` is a Python object that refers to
        a binary blob stored in the database.

* ``binary_tree.py`` defines
    a concrete binary tree algorithm
    underneath the tree interface
    using the storage abstraction.

    - ``BinaryTree`` provides a concrete implementation
        of a binary tree, with methods for
        getting, inserting, and deleting key/value pairs.

    - ``BinaryNode`` implements a node in the binary tree.

    - ``BinaryNodeRef`` knows how to serialize and deserialize
        a ``BinaryNode``.

* ``storage.py`` defines ``Storage`` class
    providing persistent, append-only record storage.
    Well, append-only except for an atomic "commit" operation
    used to atomically point to a new "root" record.
    A record is a variable-length string of bytes.

These modules grew from attempting
to give each class a single responsibility.
In other words,
each class should have only one reason to change.


### Points of extensibility

The algorithm used to update the data store
can be completely changed out
by replacing the string ``BinaryTree`` in ``interface.py``.
In particular,
data stores of this nature tend to use B-trees
not binary trees
to improve the nodes-per-byte ratio
of the tree.

By default, values are stored by ``ValueRef``
which expects bytes as values
(to be passed directly to ``Storage``).
The binary tree nodes themselves
are just a sublcass of ``ValueRef``.
Storing richer data
(via ``json``, ``msgpack``, ``pickle``, or your own invention)
is just a matter of writing your own
and setting it as the ``value_ref_class``.


### Tradeoffs (time/space, performance/readability)

The binary tree is much easier to write
and hopefully easier to read
than other tree structures would have been.
Structures like B-tree, B+ tree, B\*-tree
[and others](http://en.wikipedia.org/wiki/Tree_%28data_structure%29)
provide superior performance,
particularly for on-disk structures like this.
While a balanced binary tree
(and this one isn't)
needs to do $O(log_2(n))$ random node reads to find a value,
a B+tree needs many fewer, e.g. $O(log_32(n))$
because each node splits 32 ways instead of just 2.
This makes a huge different in practise,
since looking through 4 billion entries would go from
$log_2(2^32) = 32$ to $log_32(2^32) \approx 6.4$ lookups.


### Patterns or principles that can be used elsewhere

Test interfaces, not implementation.


Conclusion
----------

* Futher extensions to make

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

   
* Similar real-world projects to explore:

    - CouchDB
