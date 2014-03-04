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

from collections import defaultdict
from contextlib import contextmanager
from functools import wraps
from inspect import ismethod
from pprint import pprint

_emptyset = frozenset()
_unavailable = object()

class Graph(object):

    def __init__(self):
        self.antecedent_edges = defaultdict(set)
        self.consequence_edges = defaultdict(set)
        self.cache = {}
        self.stack = []
        self.change_sets = []

    def add(self, thing):
        thing._graph = self

    def link(self, key):
        if self.stack:
            caller = self.stack[-1]
            self.consequence_edges[key].add(caller)
            self.antecedent_edges[caller].add(key)

    def push(self, key):
        self.link(key)
        self.stack.append(key)

    def pop(self):
        self.stack.pop()

    def setattr_happened(self, obj, name, value):
        key = (obj, name)
        if not self.change_sets:
            return
        self.change_sets[-1].add(key)

    @contextmanager
    def consequences(self):
        """Watch a block of code update data, then run the consequences."""
        changes = set()
        self.change_sets.append(changes)
        try:
            yield
        finally:
            self.change_sets.pop()
        print
        print 'Changes:', changes
        self.run_consequences_of(changes)

    def run_consequences_of(self, keys):
        consequence_keys = self.topologically_sort_consequences(keys)
        todo = set(keys)

        for key in reversed(consequence_keys):
            if key not in todo:
                continue
            pprint(key)

            todo.remove(key)
            old_value = self.cache.pop(key, _unavailable)

            if len(key) == 2:
                obj, attr_name = key
                new_value = getattr(obj, attr_name)
            else:
                obj, method_name, args = key
                new_value = getattr(obj, method_name)(*args)

            if (old_value is not _unavailable) and (new_value == old_value):
                continue

            self.cache[key] = new_value

            consequence_keys = self.consequence_edges.get(key, _emptyset)
            todo |= consequence_keys

    def topologically_sort_consequences(self, keys):
        """List `keys` and their consequences, in topological order."""
        visited = set()
        result = []

        def visit(parent_key):
            visited.add(parent_key)
            for key in self.consequence_edges[parent_key]:
                if key not in visited:
                    visit(key)
            result.append(parent_key)

        for key in keys:
            visit(key)

        return result


class Thing(object):

    def __setattr__(self, name, value):
        if not name.startswith('_'):
            graph = getattr(self, '_graph', _unavailable)
            if graph is not _unavailable:
                graph.setattr_happened(self, name, value)
        return object.__setattr__(self, name, value)

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

        #print '  ' * len(graph.stack), 'getting', name

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
