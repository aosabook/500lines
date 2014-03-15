"""Simple directed graph implementation."""

from collections import defaultdict

class Graph(object):
    def __init__(self):
        self._dependencies = defaultdict(set)
        self._targets = defaultdict(set)

    def add_edge(self, dependency, target):
        self._targets[dependency].add(target)
        self._dependencies[target].add(dependency)

    def remove_edge(self, dependency, target):
        self._targets[dependency].remove(target)
        self._dependencies[target].remove(dependency)

    def nodes(self):
        return set(self._dependencies) | set(self._targets)

    def targets_of(self, dependency):
        return set(self._targets[dependency])

    def consequences_of(self, dependencies):
        return list(self.generate_consequences_backwards(dependencies))[::-1]

    def generate_consequences_backwards(self, dependencies):
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
        for dependency, targets in sorted(self._targets.items()):
            if (not nodes) or (dependency in nodes):
                for target in sorted(targets):
                    if (not nodes) or (target in nodes):
                        lines.append('"{}" -> "{}"'.format(dependency, target))
        return '\n'.join(lines + ['}'])
