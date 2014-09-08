"""Class for caching build outputs."""

_absent = object()

class Cache:
    """A cache of the outputs of each stage of a build process."""
    def __init__(self):
        self.cache = {}

    def set(self, target, value):
        """Store a new value for the given `target`.

        A return value of True indicates that the value in the cache was
        updated.
        """
        old_value = self.get(target)
        self.cache[target] = value
        return value != old_value

    def get(self, target):
        """Return an up-to-date cached value for `target` else `_absent`."""

        return self.cache.get(target, _absent)

