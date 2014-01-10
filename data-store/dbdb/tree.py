class Tree(object):
    node_ref_class = None

    def __init__(self, storage):
        self._storage = storage
        self._refresh_tree_ref()

    def commit(self):
        self._tree_ref.store(self._storage)
        self._storage.commit_root_address(self._tree_ref.address)

    def _refresh_tree_ref(self):
        self._tree_ref = self.node_ref_class(
            address=self._storage.get_root_address())

    def get(self, key):
        if not self._storage.locked:
            self._refresh_tree_ref()
        return self._get(self._follow(self._tree_ref), key)

    def set(self, key, value):
        if self._storage.lock():
            self._refresh_tree_ref()
        self._tree_ref = self._insert(
            self._follow(self._tree_ref), key, ValueRef(value))

    def pop(self, key):
        if self._storage.lock():
            self._refresh_tree_ref()
        self._tree_ref = self._delete(
            self._follow(self._tree_ref), key)

    def _follow(self, ref):
        return ref.get(self._storage)

    def __len__(self):
        if not self._storage.locked:
            self._refresh_tree_ref()
        root = self._follow(self._tree_ref)
        if root:
            return root.length
        else:
            return 0


class NodeRef(object):
    node_class = None

    def __init__(self, node=None, address=0):
        self._node = node
        self._address = address

    @property
    def address(self):
        return self._address

    @property
    def length(self):
        if self._node is None and self._address:
            raise RuntimeError('Asking for NodeRef length of unloaded node')
        if self._node:
            return self._node.length
        else:
            return 0

    def get(self, storage):
        if self._node is None and self._address:
            self._node = self.node_class.from_string(
                storage.read(self._address))
        return self._node

    def store(self, storage):
        if self._node is not None and not self._address:
            self._node.store_refs(storage)
            self._address = storage.write(self._node.to_string())


class ValueRef(object):
    node_class = None

    def __init__(self, value=None, address=0):
        self._value = value
        self._address = address

    @property
    def address(self):
        return self._address

    def get(self, storage):
        if self._value is None and self._address:
            self._value = storage.read(self._address)
        return self._value

    def store(self, storage):
        if self._value is not None and not self._address:
            self._address = storage.write(self._value)
