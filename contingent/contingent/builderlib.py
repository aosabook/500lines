from contextlib import contextmanager

from .graphlib import Graph

_absent = object()

class Builder:
    def __init__(self, compute):
        self.compute = compute
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
        @contextmanager
        def current_task(task):
            if self.task_stack:
                self.graph.add_edge(task, self.task_stack[-1])
            self.task_stack.append(task)
            try:
                yield
            finally:
                self.task_stack.pop()

        with current_task(task):
            if (task not in self.todo_list) and (task in self.cache):
                return self.cache[task]

            self.graph.clear_dependencies_of(task)
            new_value = self.compute(task, self.get)

            self.set(task, new_value)
            return new_value

        # ---------------------------------

        @contextmanager
        def current_task(task):
            if self.task_stack:
                self.graph.add_edge(task, self.task_stack[-1])
            self.task_stack.append(task)
            try:
                yield
            finally:
                self.task_stack.pop()

        # assume: .cache is only good values; .old is old values

        with current_task(task):
            if task in self.cache:
                return self.cache[task]

            self.graph.clear_dependencies_of(task)
            new_value = self.compute(task, self.get)

            self.set(task, new_value)
            return new_value

            # if (task not in self.old) or (self.old[task] != new_value):
            #     self.cache[task] = self.old[task] = new_value
            #     for target in self.graph.targets_of(task):
            #         self.cache.pop(target, None)  # discard?

            # return new_value

        # ---------------------------------

        @contextmanager
        def current_task(task):
            if self.task_stack:
                self.graph.add_edge(task, self.task_stack[-1])
            self.task_stack.append(task)
            try:
                yield
            finally:
                self.task_stack.pop()

        with current_task(task):
            in_cache = task in self.cache
            cache_value_good = task not in self.todo

            if in_cache and cache_value_good:
                return self.cache[task]

            self.graph.clear_dependencies_of(task)
            new_value = self.compute(task, self.get)
            # self.todo.discard(task)
            if (not in_cache) or (new_value != self.cache[task]):
                self.cache[task] = new_value
                self.todo.update(self.graph.targets_of(task))

            return new_value

            # self.task_stack.pop()


        # possible impl:

        old_value = self.cache.get(dependency, _absent)

        if dependency not in self.todo:
            if old_value is not _absent:
                return old_value

        value = self.recompute(dependency)
        target = dependency
        self.todo.discard(target)

        if (old_value is _absent) or (value != old_value):
            self.cache[target] = value
            self.todo.update(self.graph.targets_of(target))

        return value

        # return self.recompute(dependency)

        # if dependency in self.todo:
        #     value = self.recompute(dependency)
        # else:
        #     value = self.cache.get(dependency, _absent)
        #     if value is _absent:
        #         value = self.recompute(dependency)
        # return value

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

        return

        # -----------------------------------

        # self.graph.clear_dependencies_of(target)

        unchanged = (task in self.old) and (self.old[task] == value)
        self.cache[task] = self.old[task] = value

        if unchanged:
            return

        for target in self.graph.targets_of(task):
            self.cache.pop(target, None)  # discard?

        # self.cache[target] = value   # WARNING: this used to recompute todo!

    def OLD_recompute(self, target):
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
        self.task_stack.append(target)
        try:
            value = self.compute(target, self.get)
        finally:
            self.task_stack.pop()

        # return here?

        self.todo.discard(target)
        old_value = self.cache.get(target, _absent)
        if value is not _absent:
            if value == old_value:
                return value

        self.cache[target] = value
        self.todo.update(self.graph.targets_of(target))
        return value

        # if self.cache.set(target, value):
        #     self.todo.update(self.graph.targets_of(target))

        #if self.stale_values
        # if old_value != value:
        #     stale_targets = self.graph.targets_of(target)
        #     for t in stale_targets:
        #         value = self.cached_values.pop(t)

        return value

    # def compute_todo_list(self):
    #     return set(self.old) - set(self.cache)

    def invalidate(self, task):
        """Mark `task` as needing re-computation."""
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

        # while self.todo:
        #     for target in self.graph.consequences_of(self.todo, include=True):
        #         self.get(target)

    # @contextmanager
    # def consequences(self):
    #     yield
    #     self.rebuild()
