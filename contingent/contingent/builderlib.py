from contextlib import contextmanager

from .cachelib import Cache, _absent
from .graphlib import Graph

class Builder:
    def __init__(self, compute):
        self.compute = compute
        self.graph = Graph()
        self.cache = Cache(self.graph)
        self.target_stack = []

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
        if value is _absent:
            value = self.recompute(dependency)
            # self.cache[dependency] = value
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

        """
        self.graph.clear_dependencies_of(target)
        self.target_stack.append(target)
        try:
            value = self.compute(target, self.get)
        finally:
            self.target_stack.pop()
        self.cache[target] = value
        return value

    def rebuild(self, verbose=True):
        """Rebuild everything.

        If nothing has changed recently, the cache's to-do list will be
        empty, and this call is a no-op.  Otherwise we start running
        through the to-do list over and over until things stop changing.

        """
        todo = self.cache.todo()
        while todo:
            todo = list(todo)
            for target in self.graph.consequences_of(todo, include=True):
                if verbose:
                    print(target)
                self.get(target)
            todo = self.cache.todo()

    @contextmanager
    def consequences(self):
        yield
        self.rebuild()
