DogBed DataBase (DBDB)
======================

A key/value store that you'd use like BDB or SQLite. It's built like a couch,
but not as nice.

Append-only tree-based data-store. An update to a leaf updates the ancestor
nodes. Common nodes are shared. Updates are flushed leaf-to-root to disk (so
that disk addresses can be written to the parent nodes, and commit is an atomic
update to a superblock, which just points at the new root node.

A real implementation would probably use a B+ or B* tree, but that's not an
interesting detail for us. Consider replacing the provided naive binary tree
with a B+/B* tree as an exercise for the reader.

Concurrent (dirty) readers are inherently supported. Serialized fully
transactional updates are supported.
