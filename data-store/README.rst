DogBed DataBase (DBDB)
======================

A key/value store that you'd use like BDB or SQLite. It's built like a couch,
but not as nice.

Append-only tree-based data-store. An update to a leaf updates the ancestor
nodes. Common nodes are shared. Updates are flushed leaf-to-root to disk (so
that disk addresses can be written to the parent nodes, and commit is an atomic
update to a superblock, which just points at the new root node.

A real implementation would probably use a B+ or B* tree, but that's not an
interesting detail for us. Replacing the provided naive binary tree with a
B+/B* tree is left as an exercise for the reader.

Concurrent (dirty) readers are inherently supported. Serialized fully
transactional updates are supported.


Extension points
----------------

Good software can be extended easily in the ways that it needs extending (i.e.
coupling is low along the common lines of change). This example uses pickle and
dictionaries to persist tree nodes to disk, but we could very easily use
``msgpack`` and tuples to reduce the size of data on disk (which reduces IO
sizes which improves performance).


.. todo:: File locking to serialize writes. Should use portalocker.

.. todo:: Compaction. Requires atomic replace (not trivially available on
    Windows) along with write serialization.

.. todo:: Truncation on crash recovery. Is it worth adding an assumption that
    the root is the end of the file? Not sure.

.. todo:: Consider msgpack to avoid having to do the length-delimiting
    ourselves. Except that that means the storage manager has to learn about
    our serialization methods, which doesn't save the complexity I'm trying to
    avoid. Using a namedtuple also doesn't save anything over a dict from the
    pickling perspective. Interesting.

.. todo:: Stress and crash tests.
