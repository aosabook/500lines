"""Provide a Project of related tasks that can be rebuilt when inputs change.

"""
from contextlib import contextmanager
from collections import namedtuple
from functools import wraps
from .graphlib import Graph

_unavailable = object()

class Project:
    """A collection of tasks that are related as inputs and consequences."""

    def __init__(self):
        self._graph = Graph()
        self._graph.sort_key = task_key
        self._cache = {}
        self._cache_on = True
        self._task_stack = []
        self._todo = set()
        self._trace = None

    def start_tracing(self):
        """Start recording every task that is invoked by this project."""
        self._trace = []

    def stop_tracing(self, verbose=False):
        """Stop recording task invocations, and return the trace as text.

        By default, the trace only shows those tasks that were invoked,
        because no up-to-date return value was available for them in the
        cache.  But if the optional argument `verbose` is true then the
        trace will also include tasks which experienced a cache hit, not
        of a miss, and therefore did not need to be re-invoked.

        """
        text = '\n'.join(
            '{}{} {}'.format(
                '. ' * depth,
                'calling' if not_available else 'returning cached',
                task)
            for (depth, not_available, task) in self._trace
            if verbose or not_available)

        self._trace = None
        return text

    def _add_task_to_trace(self, task, return_value):
        """Add a task to the currently running task trace."""
        tup = (len(self._task_stack), return_value is _unavailable, task)
        self._trace.append(tup)

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
        sure that if `task_function` invokes any further tasks, we can
        remember that it used their return values and that it will need
        to be re-invoked again in the future if any of those other tasks
        changes its return value.

        """
        @wraps(task_function)
        def wrapper(*args):
            task = Task(wrapper, args)

            if self._task_stack:
                self._graph.add_edge(task, self._task_stack[-1])

            return_value = self._get_from_cache(task)
            if self._trace is not None:
                self._add_task_to_trace(task, return_value)

            if return_value is _unavailable:
                self._graph.clear_inputs_of(task)
                self._task_stack.append(task)
                try:
                    return_value = task_function(*args)
                finally:
                    self._task_stack.pop()
                self.set(task, return_value)

            return return_value

        return wrapper

    def _get_from_cache(self, task):
        """Return the output of the given `task`.

        If we do not have a current, valid cached value for `task`,
        returns the singleton `_unavailable` instead.

        """
        if not self._cache_on:
            return _unavailable
        if task in self._todo:
            return _unavailable
        return self._cache.get(task, _unavailable)

    @contextmanager
    def cache_off(self):
        """Context manager that forces tasks to really be invoked.

        Even if the project has already cached the output of a
        particular task, re-running the task inside of this context
        manager will make the project re-invoke the task::

            with project.cache_off():
                my_task()

        """
        original_value = self._cache_on
        self._cache_on = False
        try:
            yield
        finally:
            self._cache_on = original_value

    def set(self, task, return_value):
        """Add the `return_value` of `task` to our cache of return values.

        This gives us the opportunity to compare the new value against
        the old one that had previously been returned by the task, to
        determine whether the tasks that themselves use `task` as input
        must be added to the to-do list for re-computation.

        """
        self._todo.discard(task)
        if (task not in self._cache) or (self._cache[task] != return_value):
            self._cache[task] = return_value
            self._todo.update(self._graph.immediate_consequences_of(task))

    def invalidate(self, task):
        """Mark `task` as requiring recomputation on the next `rebuild()`.

        There are two ways that code preparing for a call to `rebuild()`
        can signal that the value we have cached for a given task is no
        longer valid.  The first is to run the task manually and then
        use `set()` to unilaterally install the new value in our cache.
        The other is to call this method to simply invalidate the `task`
        and let `rebuild()` itself call it when it next runs.

        """
        self._todo.add(task)

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
        while self._todo:
            tasks = self._graph.recursive_consequences_of(self._todo, True)
            for function, args in tasks:
                function(*args)

# Helper functions.

def task_key(task):
    """Return a sort key for a given task."""
    function, args = task
    return function.__name__, args

class Task(namedtuple('Task', ('task_function', 'args'))):
    """Turn a call to a function into a task 2-tuple.

    Given a task function and an argument list, returns a task 2-tuple
    that encapsulates the call as a single object. `Project` uses these
    task objects for consequence tracking and caching.

    Raises `ValueError` if `args` is not hashable.

    """
    __slots__ = ()

    def __new__(cls, task_function, args):
        try:
            hash(args)
        except TypeError as e:
            raise ValueError('arguments to project tasks must be immutable'
                             ' and hashable, not the {}'.format(e))

        return super().__new__(cls, task_function, args)

    def __repr__(self):
        "Produce a “syntactic,” source-like representation of the task."

        return '{}({})'.format(self.task_function.__name__,
                               ', '.join(repr(arg) for arg in self.args))
