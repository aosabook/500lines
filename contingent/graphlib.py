"""Simple directed graph implementation."""

from collections import defaultdict

class Graph(object):
    """A directed graph of build targets and their dependencies."""

    def __init__(self):
        self._dependencies = defaultdict(set)
        self._targets = defaultdict(set)

    def add_edge(self, dependency, target):
        """Add an edge recording that `dependency` is needed by `target`."""

        self._targets[dependency].add(target)
        self._dependencies[target].add(dependency)

    def remove_edge(self, dependency, target):
        """Remove the edge stating that `dependency` is needed by `target`."""

        self._targets[dependency].remove(target)
        self._dependencies[target].remove(dependency)

    def clear_dependencies_of(self, target):
        for dependency in list(self._dependencies[target]):
            self.remove_edge(dependency, target)

    def nodes(self):
        """Return all nodes that are either dependencies or targets."""

        return set(self._dependencies) | set(self._targets)

    def targets_of(self, dependency):
        """Return the targets to rebuild if `dependency` changes."""

        return set(self._targets[dependency])

    def consequences_of(self, dependencies, include=False):
        """Return topologically-sorted consequences for changed `dependencies`.

        Returns an ordered sequence listing every target that is
        downstream from the given `dependencies`.  The order will be
        chosen so that targets always follow all of their dependencies.
        If the flag `include` is true then the `dependencies` themselves
        will be correctly sorted into the resulting sequence.

        """
        g = self._generate_consequences_backwards(dependencies, include)
        return list(g)[::-1]

    def _generate_consequences_backwards(self, dependencies, include):
        def visit(dependency):
            visited.add(dependency)
            for target in try_sorting(self._targets[dependency], reverse=True):
                if target not in visited:
                    yield from visit(target)
                    yield target
        visited = set()
        for dependency in dependencies:
            yield from visit(dependency)
            if include:
                yield dependency

    def as_graphviz(self, nodes=()):
        """Render this graph as a block of graphviz code."""

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
    sequence = list(sequence)
    try:
        sequence.sort(reverse=reverse)
    except TypeError:
        pass
    return sequence
