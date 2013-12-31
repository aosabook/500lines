class BinaryTree(object):
    def __init__(self):
        self._tree = None

    def get(self, key):
        node = self._tree
        while node is not None:
            if key < node.key:
                node = node.left
            elif node.key < key:
                node = node.right
            else:
                return node.value
        raise KeyError

    def set(self, key, value):
        self._tree = self._insert(self._tree, key, value)

    def _insert(self, node, key, value):
        if node is None:
            return BinaryNode(None, key, value, None)
        elif key < node.key:
            return BinaryNode.from_node(node, left=self._insert(node.left, key, value))
        elif node.key < key:
            return BinaryNode.from_node(node, right=self._insert(node.right, key, value))
        else:
            return BinaryNode.from_node(node, value=value)

    def pop(self, key):
        self._tree = self._delete(self._tree, key)

    def _delete(self, node, key):
        if node is None:
            raise KeyError
        elif key < node.key:
            return BinaryNode.from_node(node, left=self._delete(node.left, key))
        elif node.key < key:
            return BinaryNode.from_node(node, right=self._delete(node.right, key))
        else:
            if node.left and node.right:
                replacement = self._find_max(node.left)
                return BinaryNode(
                    self._delete(node.left, replacement.key),
                    replacement.key,
                    replacement.value,
                    node.right,
                )
            else:
                return node.left or node.right

    def _find_max(self, node):
        while node.right:
            node = node.right
        return node

    def __str__(self):
        import pprint
        return "BinaryTree<\n{}\n>".format(
            pprint.pformat(self._tree and self._tree.to_dict())
        )


class BinaryNode(object):
    @classmethod
    def from_node(cls, node, **kwargs):
        return cls(
            left=kwargs.get('left', node.left),
            key=kwargs.get('key', node.key),
            value=kwargs.get('value', node.value),
            right=kwargs.get('right', node.right),
        )

    def __init__(self, left, key, value, right):
        self.left = left
        self.key = key
        self.value = value
        self.right = right

    def to_dict(self):
        return {(self.key, self.value): {
            'left': self.left and self.left.to_dict(),
            'right': self.right and self.right.to_dict(),
        }}
