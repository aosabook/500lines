
_absent = object()

class Cache:
    """A cache of the outputs of each stage of a build process."""

    def __init__(self, graph):
        self.graph = graph
        self._results = {}
        self._todo = set()

    def __setitem__(self, target, value):
        """Store a new value for the given `target`.

        This removes `target` from the current to-do list, if it is
        listed there.  And if this new `value` is different from the
        currently cached value of the target, then all targets of which
        `target` is an immediate dependency are added to the to-do list.

        """
        self._todo.discard(target)
        old_value = self._results.get(target, _absent)
        if (old_value is not _absent) and (value == old_value):
            return
        self._results[target] = value
        self._todo.update(self.graph.targets_of(target))

    def get(self, target):
        """Return an up-to-date cached value for `target` else `_absent`."""

        if target in self._todo:
            return _absent
        return self._results.get(target, _absent)

    def todo(self):
        """Return the targets that currently need to be rebuilt."""

        return set(self._todo)
