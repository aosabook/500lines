
_absent = object()

class Cache:
    """A cache of the outputs of each stage of a build process."""

    def __init__(self, graph):
        self.graph = graph
        self.results = {}

    def __setitem__(self, key, value):
        results = self.results
        old_value = results.get(key, _absent)
        if (old_value is not _absent) and (value == old_value):
            return
        results[key] = value
        for target in self.graph.targets_of(key):
            results.pop(target, None)

    def missing(self):
        return self.graph.nodes() - set(self.results)
