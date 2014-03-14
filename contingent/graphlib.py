"""Simple directed graph implementation."""

from collections import defaultdict

class Graph(object):
    def __init__(self):
        self.dependencies = defaultdict(set)
        self.targets = defaultdict(set)

    def add_edge(self, dependency, target):
        self.targets[dependency].add(target)
        self.dependencies[target].add(dependency)

    def consequences_of(self, dependency):
        return list(self.generate_consequences_backwards(dependency))[::-1]

    def generate_consequences_backwards(self, dependency):
        def visit(dependency):
            visited.add(dependency)
            for target in sorted(self.targets[dependency], reverse=True):
                if target not in visited:
                    yield from visit(target)
                    yield target
        visited = set()
        yield from visit(dependency)
