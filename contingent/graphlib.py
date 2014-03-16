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

    def set_dependencies_of(self, target, dependencies):
        """Replace incoming `target` edges with edges from `dependencies`."""

        new_dependencies = set(dependencies)
        dset = self._dependencies[target]
        for old_dependency in dset - new_dependencies:
            self.remove_edge(old_dependency, target)
        for new_dependency in new_dependencies - dset:
            self.add_edge(new_dependency, target)

    def nodes(self):
        """Return all nodes that are either dependencies or targets."""

        return set(self._dependencies) | set(self._targets)

    def targets_of(self, dependency):
        """Return the targets to rebuild if `dependency` changes."""

        return set(self._targets[dependency])

    def consequences_of(self, dependencies):
        """Return topologically-sorted consequences for changed `dependencies`.

        Returns an ordered sequence listing every target that is
        downstream from the given `dependencies`.  The order will be
        chosen so that targets always follow all of their dependencies.

        """
        return list(self._generate_consequences_backwards(dependencies))[::-1]

    def _generate_consequences_backwards(self, dependencies):
        def visit(dependency):
            visited.add(dependency)
            for target in sorted(self._targets[dependency], reverse=True):
                if target not in visited:
                    yield from visit(target)
                    yield target
        visited = set()
        for dependency in dependencies:
            yield from visit(dependency)

    def as_graphviz(self, nodes=[]):
        """Generate lines of graphviz ``dot`` code that draw this graph."""

        nodes = set(nodes)
        lines = ['digraph { node [shape=rect penwidth=0 style=filled'
                 ' fillcolor="#d6d6d6"];']
        for dependency, targets in try_sorting(self._targets.items()):
            if (not nodes) or (dependency in nodes):
                for target in try_sorting(targets):
                    if (not nodes) or (target in nodes):
                        lines.append('"{}" -> "{}"'.format(dependency, target))
        return '\n'.join(lines + ['}'])

def try_sorting(sequence):
    sequence = list(sequence)
    try:
        sequence.sort()
    except TypeError:
        pass
    return sequence
