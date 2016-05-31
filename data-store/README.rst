DogBed DataBase (DBDB)
======================

A key/value store that you'd use like BDB or SQLite. It's built like a couch,
but not as nice.

Append-only tree-based data-store. An update to a leaf updates the ancestor
nodes. Common nodes are shared. Updates are flushed leaf-to-root to disk (so
that disk addresses can be written to the parent nodes, and commit is an atomic
update to a superblock, which just points at the new root node.

A real implementation would probably use a B-tree (or B+-tree or B*-tree), but that's not an
interesting detail for us. Replacing the provided naive binary tree with a
B+/B* tree is left as an exercise for the reader.

Concurrent (dirty) readers are supported. Serialized fully transactional
updates are supported.



Installing
----------

The code is written in the common subset of Python 2.7 and 3.4
(tested via `tox <https://testrun.org/tox/latest/>`_).

I recommend using `virtualenv <https://virtualenv.pypa.io/en/latest/>`_
when installing dependencies:

::

    500lines/data-store$ virtualenv env
    500lines/data-store$ source env/bin/activate
    (env)500lines/data-store$ pip install -r requirements.txt


Testing
-------

::

    nosetests -v

    dbdb.tests.test_binary_tree.TestBinaryNodeRef.test_to_string_leaf ... ok
    dbdb.tests.test_binary_tree.TestBinaryNodeRef.test_to_string_nonleaf ... ok
    dbdb.tests.test_binary_tree.TestBinaryTree.test_del_full_node_key ... ok
    ... (etc)


Extension points
----------------

Good software can be extended easily in the ways that it needs extending (i.e.
coupling is low along the common lines of change). This example uses pickle and
dictionaries to persist tree nodes to disk, but we could very easily use
``msgpack`` and tuples to reduce the size of data on disk (which reduces IO
sizes which improves performance).

The binary tree nodes and algorithms are distinct from the persistence and
commit logic, so it should be easy to implement something different, like a
B-Tree.


.. todo:: File locking to serialize writes. Currently using portalocker, but
    the implementation is buggy because BinaryTree doesn't refresh its idea of
    the root node after the lock is acquired. That means that a stale reader
    can accidentally upgrade to a write lock and clobber other updates.  Now
    I'm not sure if it's worth adding multi-writer safety for 500lines.

.. todo:: Compaction. Requires atomic file replace (not trivially available on
    Windows) along with write serialization (but only if we care about that,
    see above).

.. todo:: Truncation on crash recovery. Is it worth adding an assumption that
    the root is the end of the file? Not sure.

.. todo:: Consider msgpack to avoid having to do the length-delimiting
    ourselves. Except that that means the storage manager has to learn about
    our serialization methods, which doesn't save the complexity I'm trying to
    avoid. Using a namedtuple also doesn't save anything over a dict from the
    pickling perspective. Interesting. msgpack IS a much smaller format,
    though, and saves disk space.

.. todo:: Stress and crash tests.
