"""Provide a Project of related tasks that can be rebuilt when inputs change.

"""
from functools import wraps
from .graphlib import Graph

_not_available = object()

class Project:
    """A collection of tasks that are related as inputs and consequences."""

    def __init__(self):
        self.graph = Graph()
        self.cache = {}
        self.task_stack = []
        self.todo_list = set()
        self.trace = None

    def start_tracing(self):
        """Start recording every task that is invoked by this project."""
        self.trace = []

    def stop_tracing(self):
        """Stop recording task invocations, and return the trace as text."""

        text = '\n'.join(
            '{}{} {}{}'.format(
                '. ' * depth,
                'calling' if not_available else 'returning cached',
                function.__name__,
                repr(args)[:-2] + ')' if len(args) == 1 else repr(args),
            ) for (depth, not_available, (function, args)) in self.trace)

        self.trace = None
        return text

    def task(self, task_function):
        """Decorate a function that defines one of the tasks for this project.

        The `task_function` should be a function that the programmer
        wants to add to this project.  This decorator will return a
        wrapped version of the function.  Each time the wrapper is
        called with a particular argument list, it checks our internal
        cache of previous calls to find out if we already know what this
        function returns for those particular arguments.  If we already
        know, then the wrapper skips the function call itself and simply
        returns the cached value.

        If the cache does not have an up-to-date return value, then the
        wrapper invokes `task_function` and saves its return value to
        the cache for future use before returning it to the caller.

        If it must invoke the `task_function`, then the wrapper places
        the task atop the current stack of executing tasks.  This makes
        sure that if `task_function` invokes any further tasks that we
        can remember that it used their return values, and that it will
        need to be re-invoked again in the future if any of those other
        tasks change their return value.

        """
        @wraps(task_function)
        def wrapper(*args):
            try:
                hash(args)
            except TypeError as e:
                raise ValueError('arguments to project tasks must be immutable'
                                 ' and hashable, not the {}'.format(e))

            task = (task_function, args)

            if self.task_stack:
                self.graph.add_edge(task, self.task_stack[-1])

            value = self._get_from_cache(task)

            if self.trace is not None:
                tup = (len(self.task_stack), value is _not_available, task)
                self.trace.append(tup)

            if value is _not_available:
                self.graph.clear_inputs_of(task)
                self.task_stack.append(task)
                try:
                    value = task_function(*args)
                finally:
                    self.task_stack.pop()
                self.set(task, value)

            return value

        wrapper.wrapped = task_function
        wrapper.__lt__ = task_function.__name__.__lt__
        return wrapper

    def _get_from_cache(self, task):
        """Return the output of the given `task`.

        If we do not have a current, valid cached value for `task`,
        returns the singleton `_not_available` instead.

        """
        if task in self.todo_list:
            return _not_available
        return self.cache.get(task, _not_available)

    def set(self, task, value):
        """Add the output `value` of `task` to our cache of outputs.

        This gives us the opportunity to compare the new value against
        the old one that had previously been returned by the task, to
        determine whether the tasks that use `task` as input must be
        added to the to-do list for re-computation.

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
            for function, args in tasks:
                function(*args)
