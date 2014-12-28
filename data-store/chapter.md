DBDB
====

Dog Bed Database (it's like a couch, but not as nice).


What does it do?
----------------

DBDB is a Python library that implements a simple key/value database.
It lets you associate a key with a value,
and store that association on disk for later retrieval.

DBDB aims to preserve data in the face of computer crashes
and error conditions.
It also avoids holding all data in RAM at once
so you can store more data than you have RAM.

But first&hellip;


Memory
------

I remember the first time I had a bug I was really stuck on. When I finished
typing in my BASIC program and ran it, weird sparkly pixels showed up on the
screen, and the program aborted early. When I went back to look at the code,
the last few lines of the program were gone! One of my mom's friends was a
P.Eng.[^PEng] and she knew how to program, so my mom arranged a phone call so I could
explain the problem and get some feedback. Within a few minutes of talking, I
had figured out the problem: the program was too big, and had encroached into
video memory. Clearing the screen truncated the program, and the sparkles were
artifacts of Applesoft BASIC's behaviour of storing program state in RAM
just beyond the end of the program.

I learned to care about memory allocation.

I remember learning about pointers and how to allocate memory with malloc, to
stick records of arbitrary-length strings into a struct so I could sort them
by latitude and longitude.

I learned how data strutures are laid out in memory.

I understood that Erlang didn't have to copy data between processes, even
though it was "strictly message-passing", because everything was immutable.
I'm not sure that the utility of immutable data structures really sank in until
I read about Clojure's data structures in 2009.
When I read about CouchDB for my new job in 2013,
I just smiled and nodded,
recognising the structures and mechanisms for managing data as it changes.

I learned that you can design systems built around immutable data.

Then I agreed to write a book chapter.
I thought that describing the core data storage concepts of CouchDB
(as I understood them)
would be fun.
In trying to write a binary tree
algorithm that mutated the tree in place, I got frustrated with how complicated
things were getting. The number of edge cases and trying to reason about how
changes in one part of the tree affected others was making my head hurt.
So I took a peek at a [recursive algorithm for
updating immutable binary trees](http://en.wikipedia.org/w/index.php?title=Binary_search_tree&oldid=628341612#Insertion),
and it turned out to be relatively straightforward.

I learned that it's easier to reason about things that don't change.

So starts the story. ``:)``


[^PEng]: From the [The Association of Professional Engineers and Geoscientists
    of Alberta](http://www.apega.ca/applicants/Engineers/peng_general.html):
    &ldquo;What is a P.Eng.? The P.Eng. designation is a professional licence,
    allowing you to practice engineering in Alberta and take responsibility for
    that work.  Only engineers licensed with APEGA, or those practising under
    the direct supervision of a P.Eng. licensed with APEGA, have a legal right
    to practice engineering in Alberta.&rdquo;


Why is it interesting?
----------------------

Most projects require a database of some kind.
You really shouldn't write your own;
there are many edge-cases that will bite you in the end,
even if you're just writing JSON to disk:

* What happens if your filesystem runs out of space?
* What happens if your laptop battery dies while saving?
* What if your data size exceeds available memory?
  (unlikely for most applications on modern desktop computers&hellip;but
  not unlikely for a mobile device or server-side web application)

DBDB provides a simple example
of how a database works.
It will help inform your use of other databases
from performance,
crash recovery,
and durability[^durability] perspectives.

[^durability]: Data stored is said to be "durable"
    if it is still readable after a power loss or crash.


Simplifications
---------------

DBDB is not [ACID](http://en.wikipedia.org/wiki/ACID).
While updates are atomic and durable,
consistency is not covered
(there are no constraints on the data stored)
and isolation is not guaranteed[^isolation].
Application code can, of course, impose its own consistency guarantees,
but proper isolation requires a transaction manager.
That could be its own 500 lines;
maybe you should write it for the 2nd edition!.

[^isolation]: Given key:values ``{a:1, b:1}``,
   and two transactions ``a = b + 1`` and ``b = a + 1``,
   full isolation requires that even when running them concurrently,
   the result is the same as if they had been run one after another (in some order).
   Results of ``{a:2, b:3}`` or ``{a:3, b:2}`` are both possible with isolation.
   Because DBDB doesn't provide this "serialisable" isolation property,
   you could end up with ``{a:2, b:2}`` instead.

Stale data is not reclaimed in this implementation,
so repeated updates
(even to the same key)
will eventually consume all disk space.
[PostgreSQL](http://www.postgresql.org/) calls this reclamation "vacuuming"
(which makes old row space available for re-use),
and [CouchDB](http://couchdb.apache.org/) calls it "compaction"
(by rewriting the "live" parts of the data store into a new file,
and atomically moving it over the old one).
DBDB can be enhanced to add a compaction feature,
but it is left as an exercise for the reader.
Bonus feature:
you can ensure that the compacted tree structure is balanced,
which helps with performance.


Intro to the toolchain
----------------------

The code is written in polyglot Python 2/3
(tested via [``tox``](https://testrun.org/tox/latest/)).

I recommend using [``virtualenv``](https://virtualenv.pypa.io/en/latest/)
when installing dependencies:

```bash
500lines/data-store$ virtualenv env
...
500lines/data-store$ source env/bin/activate
(env)500lines/data-store$ pip install -r requirements.txt
...
```

Tests can be run using [``nosetests``](https://nose.readthedocs.org/en/latest/):

```bash
(env)500lines/data-store$ nosetests
.....................
----------------------------------------------------------------------
Ran 21 tests in 0.102s

OK

```


Exploring the code
------------------

DBDB separates the concerns of "put this on disk somewhere"
(how data are laid out in a file; the physical layer)
from the logical structure of the data
(a binary tree in this example; the logical layer)
from the contents of the key/value store
(the association of key "a" to value "foo"; the public API).


### Organisational units

* ``tool.py`` defines
    a command-line tool
    for exploring a database
    from a terminal window.

* ``interface.py`` defines
    a class (``DBDB``)
    which implements the Python dictionary API
    using the concrete ``BinaryTree`` implementation.
    This is how you'd use DBDB inside a Python program.

* ``logical.py`` defines
    the logical layer.
    It's an abstract interface to a key/value store.

    - ``LogicalBase`` provides the API for logical updates
        (like get, set, and commit)
        and defers to a concrete sub-class
        to implement the updates themselves.
        It also manages storage locking
        and dereferencing internal nodes.

    - ``ValueRef`` is a Python object that refers to
        a binary blob stored in the database.
        The indirection lets us avoid loading
        the entire data store into memory all at once.

* ``binary_tree.py`` defines
    a concrete binary tree algorithm
    underneath the logical interface.

    - ``BinaryTree`` provides a concrete implementation
        of a binary tree, with methods for
        getting, inserting, and deleting key/value pairs.
        ``BinaryTree`` represents an immutable tree;
        updates are performed by returning a new tree
        which shares common structure with the old one.

    - ``BinaryNode`` implements a node in the binary tree.

    - ``BinaryNodeRef`` is a specialised ``ValueRef``
        which knows how to serialise and deserialise
        a ``BinaryNode``.

* ``physical.py`` defines
    physical layer.
    The ``Storage`` class
    provides persistent, append-only record storage.
    The only exception to the append-only policy
    is the atomic "commit" operation
    which updates the first few bytes of the file to point
    at a new "root" record.
    This is atomic for all block devices
    (e.g. hard disks, SSDs, flash memory devices).
    A record is a length-delimited series of bytes.

These modules grew from attempting
to give each class a single responsibility.
In other words,
each class should have only one reason to change.


### How it works

DBDB's data structures are immutable from the API consumer's perspective.
``BinaryTree`` nodes are created
with an associated key and value,
and left and right children.
Those associations never change.
Updating a key/value pair involves creating new nodes
from the inserted/modified/deleted leaf node
all the way back up to the new root.
Internally,
a node's private attributes mutate
to remember where its data were written
(writing happens during the commit process).

Update functions return a new root node,
and the old root is garbage collected if it's no longer referenced
(it could still be in use by a concurrent reader).
When it's time to commit changes to disk,
the tree is walked from the bottom-up
("postfix" or "depth-first" traversal),
new nodes are serialised to disk.
Finally, the disk address of the new root node is written atomically
(we know this because single-block disk writes are atomic).

This also means that readers
get lock-free access to a consistent
(but possibly out-of-date)
view of the tree.

![Tree nodes on disk before update](nodes_on_disk_1.svg)

![Tree nodes on disk after update](nodes_on_disk_2.svg)

To avoid keeping the entire tree structure in memory at the same time,
when a logical node is read in from disk,
the disk address of its left and right children
(as well as its value)
are loaded into memory.
Accessing children and their values
requires one extra function call to `NodeRef.get()`
to dereference ("really get") the data.

When changes to the tree are not committed,
they exist in memory
with references from the root down to the changed leaves.
The changes aren't saved to disk yet,
so the changed nodes contain concrete keys and values
and no disk addresses.
The process doing the writing can see uncommitted changes
and can make more changes before issuing a commit,
because `NodeRef.get()` will return the uncommitted value if it has one;
there is no difference between committed and uncommitted data
when accessed through the API.
All the updates will appear atomically to other readers
because changes aren't visible
until the new root node address is written to disk.
Concurrent updates are blocked by a lockfile on disk.
The lock is acquired on first-update, and released after commit.



### Exercises for the reader

The algorithm used to update the data store
can be completely changed out
by replacing the string ``BinaryTree`` in ``interface.py``.
Data stores tend to use more complex types of search trees
such as [B-trees](http://en.wikipedia.org/wiki/B-tree),
[B+ trees](http://en.wikipedia.org/wiki/B%2B_tree),
[and others](http://en.wikipedia.org/w/index.php?title=Template:CS_trees)
to improve the performance.
While a balanced binary tree
(and this one isn't)
needs to do $O(log_2(n))$ random node reads to find a value,
a B+tree needs many fewer, e.g. $O(log_32(n))$
because each node splits 32 ways instead of just 2.
This makes a huge different in practise,
since looking through 4 billion entries would go from
$log_2(2^32) = 32$ to $log_32(2^32) \approx 6.4$ lookups.
Each lookup is a random access,
which is incredibly expensive for hard disks with spinning platters.
SSDs help with the latency, but the savings in I/O still stand.

By default, values are stored by ``ValueRef``
which expects bytes as values
(to be passed directly to ``Storage``).
The binary tree nodes themselves
are just a sublcass of ``ValueRef``.
Storing richer data
(via [``json``](http://json.org)
or [``msgpack``](http://msgpack.org))
is just a matter of writing your own
and setting it as the ``value_ref_class``.
``BinaryNodeRef`` is an example of using
[``pickle``](https://docs.python.org/3.4/library/pickle.html)
to serialise data.

Database compaction.
Compacting should be as simple as
doing an infix-of-median traversal of the tree
writing things out as you go.
It's probably best if the tree nodes all go together,
since they're what's traversed
to find any piece of data.
Packing as many intermediate nodes as possible
into a disk sector
should improve read performance,
at least right after compaction.

### Patterns or principles that can be used elsewhere

Test interfaces, not implementation.
As part of developing DBDB,
I wrote a number of tests
that described how I wanted to be able to use it.
The first tests ran against an in-memory version of the database,
then I extended DBDB to persist to disk,
and even later added the concept of NodeRefs.
Most of the tests didn't have to change,
which gave me confidence that things were still working as expected.

Single Responsibility Principle.
Classes should have at most one reason to change.
That's not strictly the case with DBDB,
but there are multiple avenues of extension
with only localised changes required.
Refactoring as I added features was a pleasure!
