# TODO: Leave the getitem and setitem stuff to the db wrapper. We need a richer
# interface at this level.

# Here, we can also make it something closer to a persistent data structure.
# Maybe with mutability when actualizing persistence.

class BinaryTree(object):
    _EMPTY = object()

    def __init__(self, key=_EMPTY, value=None):
        self._key = key
        self._value = value
        self._left = None
        self._right = None

    def __getitem__(self, key):
        if key < self._key:
            if self._left:
                return self._left[key]
            else:
                raise KeyError
        elif self._key < key:
            if self._right:
                return self._right[key]
            else:
                raise KeyError
        else:
            return self._value

    def __setitem__(self, key, value):
        if self._key is self._EMPTY:
            self._key = key
            self._value = value
        elif key < self._key:
            if self._left:
                self._left[key] = value
            else:
                self._left = type(self)(key, value)
        elif self._key < key:
            if self._right:
                self._right[key] = value
            else:
                self._right = type(self)(key, value)
        else:
            self._value = value

    def __delitem__(self, key):
        raise NotImplementedError

    # These are all optional

    def __contains__(self, key):
        pass

    def __len__(self):
        pass

    def __iter__(self):
        pass

    def __reversed__(self):
        pass

    def __str__(self):
        return str({
            (self._key, self._value): [
                str(self._left),
                str(self._right),
            ],
        })
