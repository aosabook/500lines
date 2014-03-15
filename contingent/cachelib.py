
_absent = object()

class Cache:
    """A cache of the outputs of each stage of a build process."""

    def __init__(self, graph):
        self.graph = graph
        self._results = {}
        self._todo = set()

    def __setitem__(self, key, value):
        self._todo.discard(key)
        old_value = self._results.get(key, _absent)
        if (old_value is not _absent) and (value == old_value):
            return
        self._results[key] = value
        self._todo.update(self.graph.targets_of(key))

    def todo(self):
        return set(self._todo)
