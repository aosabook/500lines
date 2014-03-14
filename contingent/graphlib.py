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
        return self.targets[dependency]
