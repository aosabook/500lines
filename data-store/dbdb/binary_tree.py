try:
    import cPickle as pickle
except ImportError:
    import pickle


class BinaryTree(object):
    def __init__(self, storage):
        self._storage = storage
        self._tree_ref = NodeRef(address=self._storage.get_root_address())

    def commit(self):
        self._tree_ref.store(self._storage)
        self._storage.commit_root_address(self._tree_ref.address)

    def get(self, key):
        node = self._follow(self._tree_ref)
        while node is not None:
            if key < node.key:
                node = self._follow(node.left_ref)
            elif node.key < key:
                node = self._follow(node.right_ref)
            else:
                return node.value
        raise KeyError

    def set(self, key, value):
        self._tree_ref = self._insert(
            self._follow(self._tree_ref), key, value)

    def _insert(self, node, key, value):
        if node is None:
            new_node = BinaryNode(NodeRef(), key, value, NodeRef())
        elif key < node.key:
            new_node = BinaryNode.from_node(
                node,
                left_ref=self._insert(
                    self._follow(node.left_ref), key, value))
        elif node.key < key:
            new_node = BinaryNode.from_node(
                node,
                right_ref=self._insert(
                    self._follow(node.right_ref), key, value))
        else:
            new_node = BinaryNode.from_node(node, value=value)
        return NodeRef(node=new_node)

    def pop(self, key):
        self._tree_ref = self._delete(
            self._follow(self._tree_ref), key)

    def _delete(self, node, key):
        if node is None:
            raise KeyError
        elif key < node.key:
            new_node = BinaryNode.from_node(
                node,
                left_ref=self._delete(
                    self._follow(node.left_ref), key))
        elif node.key < key:
            new_node = BinaryNode.from_node(
                node,
                right_ref=self._delete(
                    self._follow(node.right_ref), key))
        else:
            left = self._follow(node.left_ref)
            right = self._follow(node.right_ref)
            if left and right:
                replacement = self._find_max(left)
                new_node = BinaryNode(
                    self._delete(
                        self._follow(node.left_ref), replacement.key),
                    replacement.key,
                    replacement.value,
                    node.right_ref,
                )
            elif left:
                return node.left_ref
            else:
                return node.right_ref
        return NodeRef(node=new_node)

    def _find_max(self, node):
        while True:
            next_node = self._follow(node.right_ref)
            if next_node is None:
                return node
            node = next_node

    def _follow(self, ref):
        return ref.get(self._storage)


class NodeRef(object):
    def __init__(self, node=None, address=0):
        self._node = node
        self._address = address

    @property
    def address(self):
        return self._address

    def get(self, storage):
        if self._node is None and self._address:
            self._node = self._node_from_string(storage.read(self._address))
        return self._node

    def store(self, storage):
        if self._node is not None and not self._address:
            self._node.left_ref.store(storage)
            self._node.right_ref.store(storage)
            self._address = storage.write(self._node._to_string())

    @classmethod
    def _node_from_string(cls, string):
        d = pickle.loads(string)
        return BinaryNode(
            cls(address=d['left']),
            d['key'],
            d['value'],
            cls(address=d['right']),
        )


class BinaryNode(object):
    @classmethod
    def from_node(cls, node, **kwargs):
        return cls(
            left_ref=kwargs.get('left_ref', node.left_ref),
            key=kwargs.get('key', node.key),
            value=kwargs.get('value', node.value),
            right_ref=kwargs.get('right_ref', node.right_ref),
        )

    def __init__(self, left_ref, key, value, right_ref):
        self.left_ref = left_ref
        self.key = key
        self.value = value
        self.right_ref = right_ref

    def _to_string(self):
        return pickle.dumps({
            'left': self.left_ref.address,
            'key': self.key,
            'value': self.value,
            'right': self.right_ref.address,
        })
