"""Experiment in dependency detection."""

# Three stages:
# 1. Being able to rebuild everything beneath a value that has changed.
# 2. Caching downstream values to avoid rebuilds that are not necessary.
# 3. Using the cache to speed things up?

# key = (<object>, 'attribute'[, args_tuple])
#
# down[key1] = [key2, ...]
# up[key2] = [key1, ...]
# cache[key] = previously_computed_value

from collections import defaultdict, deque
from contextlib import contextmanager
from functools import wraps
from inspect import ismethod
from pprint import pprint

_emptyset = frozenset()
_unavailable = object()

class Graph(object):

    def __init__(self):
        self.antecedents = defaultdict(set)
        self.consequences = defaultdict(set)
        self.cache = {}
        self.stack = []

    def add(self, thing):
        thing._graph = self

    def link(self, key):
        if self.stack:
            caller = self.stack[-1]
            self.consequences[key].add(caller)
            self.antecedents[caller].add(key)

    def push(self, key):
        self.link(key)
        self.stack.append(key)

    def pop(self):
        self.stack.pop()

    def topologically_sorted(self):
        visited_keys = set()
        finished_keys = set()
        stack = []
        #for key,  in self.down

    def run_consequences_of(self, key):
        todo = {key}
        while todo:
            key = todo.pop()
            old_value = self.cache.pop(key, _unavailable)

            if len(key) == 2:
                obj, attr_name = key
                new_value = getattr(obj, attr_name)
            else:
                obj, method_name, args = key
                new_value = getattr(obj, method_name)(*args)

            if new_value == old_value:
                continue

            self.cache[key] = new_value

            consequences = self.consequences.get(key, _emptyset)
            todo |= consequences
            pprint(consequences)


class Thing(object):

    # def __setattr__(self, name, value):
    #     self.__dict__[name] = value

    def __getattribute__(self, name):

        if name.startswith('_'):
            return object.__getattribute__(self, name)

        graph = self._graph
        cache = graph.cache

        key = (self, name)
        graph.link(key)

        value = cache.get(key, _unavailable)
        if value is not _unavailable:
            return value

        print '  ' * len(graph.stack), 'getting', name

        graph.push(key)
        try:
            value = object.__getattribute__(self, name)
        finally:
            graph.pop()

        if not ismethod(value) or value.im_self is not self:
            cache[key] = value
            return value

        method = value

        @wraps(method)
        def wrapper(*args, **kw):
            key = (self, name, args)
            graph.link(key)

            value = cache.get(key, _unavailable)
            if value is not _unavailable:
                return value

            graph.push(key)
            try:
                value = method(*args, **kw)
            finally:
                graph.pop()

            cache[key] = value
            return value

        return wrapper
