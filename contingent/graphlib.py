"""Simple directed graph implementation."""

from collections import defaultdict

class Graph(object):
    def __init__(self):
        self.dependencies = defaultdict(set)
        self.targets = defaultdict(set)

    def add_edge(self, dependency, target):
        self.targets[dependency].add(target)
        self.dependencies[target].add(dependency)

    def remove_edge(self, dependency, target):
        self.targets[dependency].remove(target)
        self.dependencies[target].remove(dependency)

    def consequences_of(self, dependencies):
        return list(self.generate_consequences_backwards(dependencies))[::-1]

    def generate_consequences_backwards(self, dependencies):
        def visit(dependency):
            visited.add(dependency)
            for target in sorted(self.targets[dependency], reverse=True):
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
        for dependency, targets in sorted(self.targets.items()):
            if (not nodes) or (dependency in nodes):
                for target in sorted(targets):
                    if (not nodes) or (target in nodes):
                        lines.append('"{}" -> "{}"'.format(dependency, target))
        return '\n'.join(lines + ['}'])
