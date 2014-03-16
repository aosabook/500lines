from cachelib import Cache, _absent
from contextlib import contextmanager
from graphlib import Graph


class Builder:
    def __init__(self, compute):
        self.compute = compute
        self.graph = Graph()
        self.cache = Cache(self.graph)
        self.stack = []

    def get(self, target):
        if self.stack:
            self.graph.add_edge(target, self.stack[-1])
        value = self.cache.get(target)
        if value is _absent:
            value = self.recompute(target)
            self.cache[target] = value
        return value

    def set(self, target, value):
        self.graph.clear_dependencies_of(target)
        if self.stack:
            self.graph.add_edge(self.stack[-1], target)
        self.cache[target] = value

    def recompute(self, target):
        print('Recomputing', target)
        self.graph.clear_dependencies_of(target)
        self.stack.append(target)
        try:
            value = self.compute(target, self.get)
        finally:
            self.stack.pop()
        return value

    def rebuild(self):
        todo = self.cache.todo()
        while todo:
            todo = list(todo)
            for target in self.graph.consequences_of(todo, include=True):
                self.get(target)
            todo = self.cache.todo()

    @contextmanager
    def consequences(self):
        yield
        self.rebuild()
