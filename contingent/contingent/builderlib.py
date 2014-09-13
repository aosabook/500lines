from contextlib import contextmanager

from .cachelib import _absent
from .graphlib import Graph

class Builder:
    def __init__(self, cache, compute):
        self.compute = compute
        self.graph = Graph()
        self.cache = cache
        self.target_stack = []
        self.todo = set()

    def get(self, dependency):
        """Return the value of a particular target.

        This is the center of our entire operation!  The fact that
        someone needs this dependency recomputed right now does two
        crucial things.  First, it lets us build an edge from this
        dependency to the target whose recompute is currently underway,
        so that the graph will tell us to recompute that target when
        this value changes in the future.  Second, it gives us the
        choice to simply return the value of the dependency immediately
        if it is already in our cache, which prevents our whole scheme
        from being O(n^2).

        """
        if self.target_stack:
            self.graph.add_edge(dependency, self.target_stack[-1])
        value = self.cache.get(dependency)
        if dependency in self.todo or value is _absent:
            value = self.recompute(dependency)
        return value

    def set(self, target, value):
        """Inform the cache that a target is being updated.

        This gives the cache the opportunity to compare the new value
        against the old one to determine whether the tasks downstream
        from the target should be added to the to-do list.

        """
        self.graph.clear_dependencies_of(target)
        self.cache[target] = value

    def recompute(self, target):
        """Recompute the given target from scratch.

        We throw out everything we learned about the target during its
        last run, because that information is out of date: we will now
        relearn what inputs it needs by watching it run all over again.

        This removes `target` from the current to-do list, if it is
        listed there.  And if the target's new `value` is different from
        the currently cached value, then all targets of which `target`
        is an immediate dependency are added to the to-do list.

        """
        self.graph.clear_dependencies_of(target)
        self.target_stack.append(target)
        try:
            value = self.compute(target, self.get)
        finally:
            self.target_stack.pop()

        self.todo.discard(target)
        if self.cache.set(target, value):
            self.todo.update(self.graph.targets_of(target))

        return value

    def rebuild(self):
        """Rebuild everything.

        If nothing has changed recently, the cache's to-do list will be
        empty, and this call is a no-op.  Otherwise we start running
        through the to-do list over and over until things stop changing.

        """
        while self.todo:
            for target in self.graph.consequences_of(self.todo, include=True):
                self.get(target)

    @contextmanager
    def consequences(self):
        yield
        self.rebuild()
