
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
        """Return the output value of a particular task.

        This operation involves two phases.

        First, the fact that we are being asked about `task` at this
        exact moment is evidence that whatever other task is currently
        underway must be using this `task` as one of its inputs.  So if
        the stack of currently-executing tasks is not simply empty, then
        we draw an edge between this task and the top task on the stack.

        Then we need to figure out a return value.  If there is a
        still-valid output value for `task` already in our cache, we
        return it immediately.  Otherwise we need to re-invoke the task.
        To correctly re-learn its inputs, we clear all of its current
        incoming edges, and then place it on top of the stack for the
        duration of its run.  As it goes looking for its inputs, and
        thus causes this method to be re-invoked for them, we will wind
        up building the edges necessary to keep the task up-to-date in
        the future.

        """
        if self.task_stack:
            self.graph.add_edge(task, self.task_stack[-1])

        value = self._get_from_cache(task)

        if value is _not_available:
            self.graph.clear_inputs_of(task)
            self.task_stack.append(task)
            try:
                value = self.task_callback(task, self.get)
            finally:
                self.task_stack.pop()
            self.set(task, value)

        return value

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
        """Add the output `value` of `task` to our cache of outputs.

        This gives us the opportunity to compare the new value against
        the old one that had previously been returned by the task, to
        determine whether the tasks downstream from `task` must be added
        to the to-do list for re-computation.

        """
        self.todo_list.discard(task)
        if (task not in self.cache) or (self.cache[task] != value):
            self.cache[task] = value
            self.todo_list |= self.graph.immediate_consequences_of(task)

    def invalidate(self, task):
        """Mark `task` as requiring re-computation on the next `rebuild()`.

        There are two ways that code preparing for a call to `rebuild()`
        can signal that the value we have cached for a given task is no
        longer valid.  The first is to run the task manually and then
        use `set()` to unilaterally install the new value in our cache.
        The other is to call this method to simply invalidate the `task`
        and let `rebuild()` itself call it when it next runs.

        """
        self.todo_list.add(task)

    def rebuild(self):
        """Repeatedly rebuild every out-of-date task until all are current.

        If nothing has changed recently, our to-do list will be empty,
        and this call will return immediately.  Otherwise we take the
        tasks in the current to-do list, along with every consequence
        anywhere downstream of them, and call `get()` on every single
        one to force re-computation of the tasks that are either already
        invalid or that become invalid as the first few in the list are
        recomputed.

        Unless there are cycles in the task graph, this will eventually
        return.

        """
        while self.todo_list:
            tasks = self.graph.recursive_consequences_of(self.todo_list, True)
            for task in tasks:
                self.get(task)
