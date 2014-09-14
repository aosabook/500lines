from contextlib import contextmanager

from .graphlib import Graph

_not_available = object()

class Builder:
    def __init__(self, callback):
        self.task_callback = callback
        self.graph = Graph()
        self.cache = {}
        self.task_stack = []
        self.todo_list = set()

    def get(self, task):
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
        if self.task_stack:
            self.graph.add_edge(task, self.task_stack[-1])

        value = self._get_from_cache(task)

        if value is _not_available:
            self.graph.clear_dependencies_of(task)
            value = self._have_build_process_invoke_task(task)
            self.set(task, value)
        return value

    def _have_build_process_invoke_task(self, task):
        """Ask the outside world to compute the output of `task`.

        We place the task ID on our internal stack for the duration of
        its run, so that any tasks it kicks off get recorded as its
        input tasks.

        """
        self.task_stack.append(task)
        try:
            return self.task_callback(task, self.get)
        finally:
            self.task_stack.pop()

    def _get_from_cache(self, task):

        """Return the output of the given `task`.

        If we do not have a current, valid cached value for `task`,
        returns the singleton `_not_available` instead.

        """
        if task in self.todo_list:
            return _not_available
        if task not in self.cache:
            return _not_available
        return self.cache[task]

    def set(self, task, value):

        """Inform the cache that a target is being updated.

        This gives the cache the opportunity to compare the new value
        against the old one to determine whether the tasks downstream
        from the target should be added to the to-do list.

        """
        self.todo_list.discard(task)
        if (task not in self.cache) or (self.cache[task] != value):
            self.cache[task] = value
            self.todo_list.update(self.graph.targets_of(task))

    def invalidate(self, task):
        """Mark `task` as needing re-computation.

        """
        self.todo_list.add(task)

    def rebuild(self):
        """Rebuild everything.

        If nothing has changed recently, the cache's to-do list will be
        empty, and this call is a no-op.  Otherwise we start running
        through the to-do list over and over until things stop changing.

        """
        while self.todo_list:
            tasks = self.graph.consequences_of(self.todo_list, include=True)
            for task in tasks:
                self.get(task)
