# TODO: Leave the getitem and setitem stuff to the db wrapper. We need a richer
# interface at this level.

# Here, we can also make it something closer to a persistent data structure.
# Maybe with mutability when actualizing persistence.

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

    def set_tree(self, new_tree):
        self._tree = new_tree

    def set(self, key, value):
        setter = self.set_tree
        node = self._tree

        while node is not None:
            if key < node.key:
                setter = node.set_left
                node = node.left
            elif node.key < key:
                setter = node.set_right
                node = node.right
            else:
                break

        if node:
            # Found a node, therefore it's a value overwrite
            node.value = value
        else:
            setter(BinaryNode(None, key, value, None))

    def pop(self, key):
        setter = self.set_tree
        node = self._tree

        while node is not None:
            if key < node.key:
                setter = node.set_left
                node = node.left
            elif node.key < key:
                setter = node.set_right
                node = node.right
            else:
                if not node.left and not node.right:
                    setter(None)
                elif node.left and not node.right:
                    setter(node.left)
                elif not node.left and node.right:
                    setter(node.right)
                else:
                    raise NotImplementedError
                return

        raise KeyError

    def __str__(self):
        import pprint
        return "BinaryTree<\n{}\n>".format(
            pprint.pformat(self._tree and self._tree.to_dict())
        )


class BinaryNode(object):
    def __init__(self, left, key, value, right):
        self.left = left
        self.key = key
        self.value = value
        self.right = right

    def set_left(self, new_left):
        self.left = new_left

    def set_right(self, new_right):
        self.right = new_right

    def to_dict(self):
        return {(self.key, self.value): {
            'left': self.left and self.left.to_dict(),
            'right': self.right and self.right.to_dict(),
        }}
