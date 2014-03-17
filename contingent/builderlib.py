from cachelib import Cache, _absent
from contextlib import contextmanager
from graphlib import Graph


class Builder:
    def __init__(self, compute):
        self.compute = compute
        self.graph = Graph()
        self.cache = Cache(self.graph)
        self.target_stack = []

    def get(self, dependency):
        if self.target_stack:
            self.graph.add_edge(dependency, self.target_stack[-1])
        value = self.cache.get(dependency)
        if value is _absent:
            value = self.recompute(dependency)
            self.cache[dependency] = value
        return value

    def set(self, target, value):
        self.graph.clear_dependencies_of(target)
        if self.target_stack:
            self.graph.add_edge(self.target_stack[-1], target)
        self.cache[target] = value

    def recompute(self, target):
        self.graph.clear_dependencies_of(target)
        self.target_stack.append(target)
        try:
            value = self.compute(target, self.get)
        finally:
            self.target_stack.pop()
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
