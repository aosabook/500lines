from dbdb.binary_tree import BinaryTree
from dbdb.storage import Storage


class DBDB(object):
    def __init__(self, f):
        self._storage = Storage(f)
        self._tree = BinaryTree(self._storage)
        self.closed = False

    def _assert_not_closed(self):
        if self.closed:
            raise ValueError('Database closed.')

    def close(self):
        self._storage.close()
        self.closed = True

    def commit(self):
        self._assert_not_closed()
        self._tree.commit()

    def __getitem__(self, key):
        self._assert_not_closed()
        return self._tree.get(key)

    def __setitem__(self, key, value):
        self._assert_not_closed()
        return self._tree.set(key, value)

    def __delitem__(self, key):
        self._assert_not_closed()
        return self._tree.pop(key)

    def __contains__(self, key):
        try:
            self[key]
        except KeyError:
            return False
        else:
            return True

    def __len__(self):
        return len(self._tree)
