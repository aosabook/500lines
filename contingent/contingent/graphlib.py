"""A directed graph of tasks that use one another as inputs."""

from collections import defaultdict

class Graph:
    """A directed graph of the relationships among build tasks.

    A task can be identified by any hashable value that is eligible to
    be enrolled as a dictionary key.

    """
    def __init__(self):
        self._inputs_of = defaultdict(set)
        self._consequences_of = defaultdict(set)

    def add_edge(self, input_task, consequence_task):
        """Add an edge: `consequence_task` uses the output of `input_task`."""
        self._consequences_of[input_task].add(consequence_task)
        self._inputs_of[consequence_task].add(input_task)

    def clear_inputs_of(self, task):
        """Remove all edges leading to `task` from its previous inputs."""
        input_tasks = self._inputs_of.pop(task, ())
        for input_task in input_tasks:
            self._consequences_of[input_task].remove(task)

    def all_tasks(self):
        """Return all task identifiers."""
        return set(self._inputs_of) | set(self._consequences_of)

    def immediate_consequences_of(self, task):
        """Return the tasks that use `task` as an input."""
        return set(self._consequences_of[task])

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
            for consequence in try_sorting(consequences, reverse=True):
                if consequence not in visited:
                    yield from visit(consequence)
                    yield consequence

        def generate_consequences_backwards():
            for task in tasks:
                yield from visit(task)
                if include:
                    yield task

        visited = set()
        return list(generate_consequences_backwards())[::-1]

    def as_graphviz(self, nodes=()):
        """Render this graph as a block of graphviz code.

        TODO: might not work any more?

        """
        nodes = set(nodes) or self.nodes()
        lines = ['digraph { graph [rankdir=LR]; node [shape=rect penwidth=0'
                 ' style=filled fillcolor="#d6d6d6"];']
        for dependency, targets in try_sorting(self._targets.items()):
            if dependency in nodes:
                for target in try_sorting(targets):
                    if target in nodes:
                        lines.append('"{}" -> "{}"'.format(dependency, target))
        return '\n'.join(lines + ['}'])


def try_sorting(sequence, reverse=False):
    """Attempt to sort a sequence, accepting failure gracefully."""
    sequence = list(sequence)
    try:
        sequence.sort(reverse=reverse)
    except TypeError:
        pass
    return sequence
