DBDB (Dog Bed Database) is a Python library that implements a simple key/value database.
It lets you associate a key with a value,
and store that association on disk for later retrieval.

DBDB aims to preserve data in the face of computer crashes
and error conditions.
It also avoids holding all data in RAM at once
so you can store more data than you have RAM.


Memory
------

I remember the first time I had a bug I was really stuck on. When I finished
typing in my BASIC program and ran it, weird sparkly pixels showed up on the
screen, and the program aborted early. When I went back to look at the code,
the last few lines of the program were gone! 

One of my mom's friends knew how to program. Within a few minutes of speaking
with her, I found the problem: the program was too big, and had
encroached into video memory. Clearing the screen truncated the program, and
the sparkles were artifacts of Applesoft BASIC's behaviour of storing program
state in RAM just beyond the end of the program.

From that moment onwards, I learned to care a lot about memory allocation.  I
learned about pointers and how to allocate memory with malloc. I learned how my
data structures were laid out in memory. And I learned to be very, very careful
about how I changed them.

Some years later, while reading about a process-oriented language called
Erlang, I learned that it didn't actually have to copy data to send messages
between processes, because everything was immutable. I then discovered
immutable data structures in Clojure, and it really began to sink in. 

When I read about CouchDB for my new job in 2013, I just smiled and nodded,
recognising the structures and mechanisms for managing complex data as it
changes.

I learned that you can design systems built around immutable data.

Then I agreed to write a book chapter.

I thought that describing the core data storage concepts of CouchDB
(as I understood them)
would be fun.

While trying to write a binary tree
algorithm that mutated the tree in place, I got frustrated with how complicated
things were getting. The number of edge cases and trying to reason about how
changes in one part of the tree affected others was making my head hurt. I had
no idea how I was going to explain all of this.

Remember lessons learned, I took a peek at a [recursive algorithm for
updating immutable binary trees](http://en.wikipedia.org/w/index.php?title=Binary_search_tree&oldid=628341612#Insertion),
and it turned out to be relatively straightforward.

I learned, once again, that it's easier to reason about things that don't change.

So starts the story.


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

DBDB's operations are not fully [ACID](http://en.wikipedia.org/wiki/ACID).
ACID refers to four important attributes of databases:
atomicity, consistency, isolation, and durability.
Updates in DBDB are atomic and durable,
two attributes which are described later in the chapter.
There are no consistency guarantees provided by DBDB
as there are no constraints on the data stored.
Isolation is likewise not implemented[^isolation].
Application code can, of course, impose its own consistency guarantees,
but proper isolation requires a transaction manager.
That could be its own 500 lines;
maybe you should write it for the 2nd edition! ``:)``

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
which helps maintain performance over time.


Intro to the toolchain
----------------------

The code is written in the common subset of Python 2.7 and 3.4
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
    In this implementation, a record's address on disk
    is its location (in bytes) in the file,
    but consumers make no assumptions about this.

These modules grew from attempting
to give each class a single responsibility.
In other words,
each class should have only one reason to change.


### A debugger's-eye view

#### Reading a value

Let's see what happens
when we try to get the value associated with key ``foo`` in ``example.db``:
```bash
$ python -m dbdb.tool example.db get foo
```

This runs the ``main()`` function from module ``dbdb.tool``:
**QUESTION: Is it useful to have the whole function, or should I elide the bits we're not focusing on (mostly error checking)?**
```python
# dbdb/tool.py
def main(argv):
    if not (4 <= len(argv) <= 5):
        usage()
        return BAD_ARGS
    dbname, verb, key, value = (argv[1:] + [None])[:4]
    if verb not in {'get', 'set', 'delete'}:
        usage()
        return BAD_VERB
    db = dbdb.connect(dbname)          # CONNECT
    try:
        if verb == 'get':
            sys.stdout.write(db[key])  # GET VALUE
        elif verb == 'set':
            db[key] = value
            db.commit()
        else:
            del db[key]
            db.commit()
    except KeyError:
        print("Key not found", file=sys.stderr)
        return BAD_KEY
    return OK
```

The ``connect()`` function
opens the database file
(possibly creating it,
but never overwriting it)
and returns an instance of ``DBDB``:
```python
# dbdb/__init__.py
def connect(dbname):
    try:
        f = open(dbname, 'r+b')
    except IOError:
        fd = os.open(dbname, os.O_RDWR | os.O_CREAT)
        f = os.fdopen(fd, 'r+b')
    return DBDB(f)
```

DBDB implements the Python API
by coordinating ``Storage`` and a ``BinaryTree``:
```python
# dbdb/interface.py
class DBDB(object):

    def __init__(self, f):
        self._storage = Storage(f)
        self._tree = BinaryTree(self._storage)
```

Getting the value at ``key``
is as simple as a dictionary lookup (``db[key]``)
which calls ``DBDB.__getitem__()``.
```python
# dbdb/interface.py
class DBDB(object):
# ...
    def __getitem__(self, key):
        self._assert_not_closed()
        return self._tree.get(key)
```

``__getitem__()`` ensures that the database is still open
and then retrieves the value associated with ``key``
on the internal ``_tree`` by calling ``_tree.get()``.

``_tree.get()`` is provided by ``LogicalBase``:
```python
# dbdb/logical.py
class LogicalBase(object):
# ...
    def get(self, key):
        if not self._storage.locked:
            self._refresh_tree_ref()
        return self._get(self._follow(self._tree_ref), key)
```

``get()`` checks to see if we have the tree locked:
if it is, then it cannot have changed since we last refreshed it;
but if it isn't, then we update to the most recent data on disk.
Because we don't lock the tree at this point,
any subsequent read might get different data.
This is known as a "dirty read".
Many different processes can do dirty reads at once with no locking
because the root node address is updated atomically,
only after all the data have been written to disk.
Another way to look at it is that ``self._tree_ref``
is only changed by ``_refresh_tree_ref()``.
While a second read operation might use a different ``_tree_ref``
(that points to different data),
each read operation is guaranteed to succeed:
to return a value that associated with the key in this tree,
or find that they key isn't in this tree.

If you needed to be able to read some data consistently
(for example,
reading a value and then updating it based on that value)
you would have to lock the database before doing your first read.
Other processes can continue to make dirty reads
even if this process has locked the database,
but none of the other processes can change any data.
Implementing this function is left as an excercise for the reader.

The tree as a whole
is represented by a mutable ``BinaryTree`` object.
Inside the ``BinaryTree``,
``Node``s and ``NodeRef``s are value objects:
they are immutable and their contents never change
``Node``s are created
with an associated key and value,
and left and right children.
Those associations never change.
The content of the whole ``BinaryTree`` only visibly changes
when then root node is replaced.
We'll see how these classes interact
concretely in more detail
in the *Inserting/updating* section.

For now, getting the actual data
is just a matter of doing a standard binary tree search,
following refs to their nodes:
**QUESTION: Is it helpful to show the stack by listing files at the beginning of each snippet? I'm not entirely sure how to represent it so that it's unambiguous and not distracting.**
```python
# dbdb/tool.py
# dbdb/interface.py
# dbdb/logical.py
# dbdb/binary_tree.py
class BinaryTree(LogicalBase):
# ...
    def _get(self, node, key):
        while node is not None:
            if key < node.key:
                node = self._follow(node.left_ref)
            elif node.key < key:
                node = self._follow(node.right_ref)
            else:
                return self._follow(node.value_ref)
        raise KeyError
```

The returned value is then written to ``stdout`` by ``main()``,
without adding any extra newlines,
to preserve the user's data exactly.


#### Inserting/updating

Now we'll set key ``foo`` to value ``bar`` in ``example.db``:
```bash
$ python -m dbdb.tool example.db set foo bar
```

Again, this runs the ``main()`` function from module ``dbdb.tool``:
**QUESTION: Is it useful to have the whole function, or should I elide the bits we're not focusing on?**
```python
# dbdb/tool.py
def main(argv):
    if not (4 <= len(argv) <= 5):
        usage()
        return BAD_ARGS
    dbname, verb, key, value = (argv[1:] + [None])[:4]
    if verb not in {'get', 'set', 'delete'}:
        usage()
        return BAD_VERB
    db = dbdb.connect(dbname)          # CONNECT
    try:
        if verb == 'get':
            sys.stdout.write(db[key])
        elif verb == 'set':
            db[key] = value            # SET VALUE
            db.commit()                # COMMIT
        else:
            del db[key]
            db.commit()
    except KeyError:
        print("Key not found", file=sys.stderr)
        return BAD_KEY
    return OK
```

This time we set the value with ``db[key] = value``
which calls ``DBDB.__setitem__()``.
```python
# dbdb/interface.py
class DBDB(object):
# ...
    def __setitem__(self, key, value):
        self._assert_not_closed()
        return self._tree.set(key, value)
```

``__setitem__`` ensures that the database is still open
and then stores the association from ``key`` to ``value``
on the internal ``_tree`` by calling ``_tree.set()``.

``_tree.set()`` is provided by ``LogicalBase``:
```python
# dbdb/logical.py
class LogicalBase(object):
# ...
    def set(self, key, value):
        if self._storage.lock():
            self._refresh_tree_ref()
        self._tree_ref = self._insert(
            self._follow(self._tree_ref), key, self.value_ref_class(value))
```

``set()`` ensures that the tree is locked,
and that we have the most recent root node reference
so we don't lose any updates that another process has made
since we last refreshed the tree from disk.
Then it replaces the root tree node
with a new tree containing the inserted (or updated) key/value.
Note that there are no changes to anything on disk at this point.

Inserting or updating the tree doesn't mutate any nodes,
because ``_insert()`` returns a new tree.
The new tree shares unchanged parts with the previous tree
to save on memory and execution time.
It's natural to implement this recursively:
```python
# dbdb/binary_tree.py
class BinaryTree(LogicalBase):
# ...
    def _insert(self, node, key, value_ref):
        if node is None:
            new_node = BinaryNode(
                self.node_ref_class(), key, value_ref, self.node_ref_class(), 1)
        elif key < node.key:
            new_node = BinaryNode.from_node(
                node,
                left_ref=self._insert(
                    self._follow(node.left_ref), key, value_ref))
        elif node.key < key:
            new_node = BinaryNode.from_node(
                node,
                right_ref=self._insert(
                    self._follow(node.right_ref), key, value_ref))
        else:
            new_node = BinaryNode.from_node(node, value_ref=value_ref)
        return self.node_ref_class(referent=new_node)
```

Notice how we always return a new node
(wrapped in a ``NodeRef``).
Instead of updating a node to point to a new subtree,
we make a new node which shares the unchanged subtree.
This is what makes this binary tree an immutable data structure.

Committing involves writing out all of the dirty state in memory,
and then saving the disk address of the tree's new root node.
Starting from the API:
```python
# dbdb/interface.py
class DBDB(object):
# ...
    def commit(self):
        self._assert_not_closed()
        self._tree.commit()
```

The implementation of ``_tree.commit()`` comes from ``LogicalBase``:
```python
# dbdb/logical.py
class LogicalBase(object)
# ...
    def commit(self):
        self._tree_ref.store(self._storage)
        self._storage.commit_root_address(self._tree_ref.address)
```

All ``NodeRef``s know how to serialise themselves to disk
by first asking their children to serialise via ``prepare_to_store()``:
```python
# dbdb/logical.py
class ValueRef(object):
# ...
    def store(self, storage):
        if self._referent is not None and not self._address:
            self.prepare_to_store(storage)
            self._address = storage.write(self.referent_to_string(self._referent))
```

``self._tree_ref`` in ``LogicalBase`` is actually a ``BinaryNodeRef``
(a subclass of ``ValueRef``) in this case,
so the concrete implementation of ``prepare_to_store()`` is:
```python
# dbdb/binary_tree.py
class BinaryNodeRef(ValueRef):
    def prepare_to_store(self, storage):
        if self._referent:
            self._referent.store_refs(storage)
```

The ``BinaryNode`` in question, ``_referent``,
asks its refs to store themselves:
```python
# dbdb/binary_tree.py
class BinaryNode(object):
# ...
    def store_refs(self, storage):
        self.value_ref.store(storage)
        self.left_ref.store(storage)
        self.right_ref.store(storage)
```

This recurses all the way down for any ``NodeRef``
which has unwritten changes (i.e. no ``_address``).

Back up the stack, I'll repeat this code snippet for context.
The last step of ``store()`` is to serialise this node
and save its storage address:
```python
# dbdb/logical.py
class ValueRef(object):
# ...
    def store(self, storage):
        if self._referent is not None and not self._address:
            self.prepare_to_store(storage)
            self._address = storage.write(self.referent_to_string(self._referent))
```

At this point
the ``NodeRef``'s ``_referent`` is guaranteed to have addresses available for all of its own refs,
so we can serialise it by creating a bytestring representing this node:
```python
# dbdb/binary_tree.py
class BinaryNodeRef(ValueRef):
# ...
    @staticmethod
    def referent_to_string(referent):
        return pickle.dumps({
            'left': referent.left_ref.address,
            'key': referent.key,
            'value': referent.value_ref.address,
            'right': referent.right_ref.address,
            'length': referent.length,
        })
```

Updating the address in the ``store()`` method
is technically a mutation of the ``ValueRef``.
Because it has no effect on the user-visible value though,
we can consider it to be immutable.

Once ``store()`` on the root ``_tree_ref`` is complete
(in ``LogicalBase.commit()``),
we know that all of the data are written to disk.
We just have to commit the root address by calling:
```python
# dbdb/physical.py
class Storage(object):
# ...
    def commit_root_address(self, root_address):
        self.lock()
        self._f.flush()
        self._seek_superblock()
        self._write_integer(root_address)
        self._f.flush()
        self.unlock()
```

We ensure that the file handle is flushed
(so that the OS knows we want all the data saved to stable storage like an SSD)
and write out the address of the root node.
We know this last write is atomic because we store the disk address on a sector boundary.
It's the very first thing in the file,
so this is true regardless of sector size,
and single-sector disk writes are guaranteed atomic by the disk hardware.

Because the root node address has either the old or new value
(never a bit of old and a bit of new),
other processes can read from the database without getting a lock.
An external process might see the old or the new tree,
but never a mix of the two.
In this way, commits are atomic.

Because we write the new data to disk and call the ``fsync`` syscall[^fsync]
before we write the root node address,
uncommitted data are unreachable.
Conversely, once the root node address has been updated,
we know that all the data it references is also on disk.
In this way, commits are also durable.

[^fsync]: Calling ``fsync`` on a file descriptor
   asks the operating system and hard drive (or SSD)
   to write all buffered data immediately.
   Operating systems and drives don't usually write everything immediately
   in order to improve performance.

We're done!


### How NodeRefs save memory

To avoid keeping the entire tree structure in memory at the same time,
when a logical node is read in from disk,
the disk address of its left and right children
(as well as its value)
are loaded into memory.
Accessing children and their values
requires one extra function call to ``NodeRef.get()``
to dereference ("really get") the data.

All we need to construct a ``NodeRef`` is an address:

    +---------+
    | NodeRef |
    | ------- |
    | addr=3  |
    | get()   |
    +---------+

Calling ``get()`` on it will return the concrete node,
along with that node's references as ``NodeRef``s:

    +---------+     +---------+     +---------+
    | NodeRef |     | Node    |     | NodeRef |
    | ------- |     | ------- | +-> | ------- |
    | addr=3  |     | key=A   | |   | addr=1  |
    | get() ------> | value=B | |   +---------+
    +---------+     | left  ----+
                    | right ----+   +---------+
                    +---------+ |   | NodeRef |
                                +-> | ------- |
                                    | addr=2  |
                                    +---------+

When changes to the tree are not committed,
they exist in memory
with references from the root down to the changed leaves.
The changes aren't saved to disk yet,
so the changed nodes contain concrete keys and values
and no disk addresses.
The process doing the writing can see uncommitted changes
and can make more changes before issuing a commit,
because ``NodeRef.get()`` will return the uncommitted value if it has one;
there is no difference between committed and uncommitted data
when accessed through the API.
All the updates will appear atomically to other readers
because changes aren't visible
until the new root node address is written to disk.
Concurrent updates are blocked by a lockfile on disk.
The lock is acquired on first-update, and released after commit.


### notes and odd ends

(TODO: Picture of uncommitted, modified tree nodes;
picture of the same, during the commit process;
picture of the same, once the commit is complete)

![Tree nodes on disk before update](nodes_on_disk_1.svg)

![Tree nodes on disk after update](nodes_on_disk_2.svg)


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
This makes a huge different in practice,
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


### Summary

(Discussion of the concrete: this data store, data stores in general;
and of the abstract: immutable data on disk and in memory)
