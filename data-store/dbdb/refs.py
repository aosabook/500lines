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
