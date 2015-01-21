"""A directed graph of tasks that use one another as inputs."""

from collections import defaultdict

class Graph:
    """A directed graph of the relationships among build tasks.

    A task can be identified by any hashable value that is eligible to
    act as a Python dictionary key.  If the user has a preferred order
    for tasks when the graph is otherwise agnostic about output order,
    they may set the ``sort_key`` attribute of their ``Graph`` instance
    to a ``sorted()`` key function.

    """
    sort_key = None

    def __init__(self):
        self._inputs_of = defaultdict(set)
        self._consequences_of = defaultdict(set)

    def sorted(self, nodes, reverse=False):
        """Try sorting `nodes`, else return them in iteration order.

        Graph methods that return a list of tasks but do not care about
        their order can use this method to impose a user-selected order
        instead.  In particular, doctests benefit from the imposition of
        a gratuitous stable order on sequences that otherwise lack any
        stable order from one run to the next.  This method tries to use
        this Graph's ``sort_key`` function to order the given `nodes`.
        If sorting does not succeed, then the nodes are returned in
        their natural iteration order instead.

        """
        nodes = list(nodes)  # grab nodes in one pass, in case it's a generator
        try:
            nodes.sort(key=self.sort_key, reverse=reverse)
        except TypeError:
            pass
        return nodes

    def add_edge(self, input_task, consequence_task):
        """Add an edge: `consequence_task` uses the output of `input_task`."""
        self._consequences_of[input_task].add(consequence_task)
        self._inputs_of[consequence_task].add(input_task)

    def remove_edge(self, input_task, consequence_task):
        """Remove an edge."""
        self._consequences_of[input_task].remove(consequence_task)
        self._inputs_of[consequence_task].remove(input_task)

    def inputs_of(self, task):
        """Return the tasks that are inputs to `task`."""
        return self.sorted(self._inputs_of[task])

    def clear_inputs_of(self, task):
        """Remove all edges leading to `task` from its previous inputs."""
        input_tasks = self._inputs_of.pop(task, ())
        for input_task in input_tasks:
            self._consequences_of[input_task].remove(task)

    def tasks(self):
        """Return all task identifiers."""
        return self.sorted(set(self._inputs_of).union(self._consequences_of))

    def edges(self):
        """Return all edges as ``(input_task, consequence_task)`` tuples."""
        return [(a, b) for a in self.sorted(self._consequences_of)
                       for b in self.sorted(self._consequences_of[a])]

    def immediate_consequences_of(self, task):
        """Return the tasks that use `task` as an input."""
        return self.sorted(self._consequences_of[task])

    def recursive_consequences_of(self, tasks, include=False):
        """Return the topologically-sorted consequences of the given `tasks`.

        Returns an ordered list of every task that can be reached by
        following consequence edges from the given `tasks` down to the
        tasks that use them as inputs.  The order of the returned list
        is chosen so that all of the inputs to a consequence precede it
        in the list.  This means that if you run through the list
        executing tasks in the given order, that tasks should find that
        the inputs they need (or at least that they needed last time)
        are already computed and available.

        If the flag `include` is true, then the `tasks` themselves will
        be correctly sorted into the resulting sequence.  Otherwise they
        will be omitted.

        """
        def visit(task):
            visited.add(task)
            consequences = self._consequences_of[task]
            for consequence in self.sorted(consequences, reverse=True):
                if consequence not in visited:
                    yield from visit(consequence)
                    yield consequence

        def generate_consequences_backwards():
            for task in self.sorted(tasks, reverse=True):
                yield from visit(task)
                if include:
                    yield task

        visited = set()
        return list(generate_consequences_backwards())[::-1]
