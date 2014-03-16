from cachelib import Cache, _absent
from graphlib import Graph


class Builder:
    def __init__(self, compute):
        self.compute = compute
        self.graph = Graph()
        self.cache = Cache(self.graph)
        self.stack = []

    def get(self, target):
        if self.stack:
            self.stack[-1].add(target)
        value = self.cache.get(target)
        if value is _absent:
            value = self.recompute(target)
        return value

    def recompute(self, target):
        new_dependencies = set()
        self.stack.append(new_dependencies)
        try:
            value = self.compute(target, self.get)
        finally:
            self.stack.pop()
        self.graph.set_dependencies_of(target, new_dependencies)
        return value
