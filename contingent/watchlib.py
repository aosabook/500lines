"""Experiment in dependency detection."""

from functools import wraps
from inspect import ismethod

class Graph(object):

    def __init__(self):
        self.nodes = []
        self.stack = []

    def add(self, node):
        self.nodes.append(node)
        node._graph = self

class Node(object):

    # def __setattr__(self, name, value):
    #     self.__dict__[name] = value

    def __getattribute__(self, name):
        if name.startswith('_'):
            return object.__getattribute__(self, name)
        graph = self._graph
        print '  ' * len(graph.stack), 'getting', name
        value = object.__getattribute__(self, name)
        if not ismethod(value) or not isinstance(value.im_self, Node):
            return value
        method = value
        graph = self._graph
        @wraps(method)
        def wrapper(*args, **kw):
            graph.stack.append('todo')
            try:
                return method(*args, **kw)
            finally:
                graph.stack.pop()
        return wrapper

    def watch(self, function):
        @wraps(function)
        def wrapper(*args, **kw):
            self.start(function, args, kw)
            try:
                value = function(*args, **kw)
            finally:
                self.end(function, args, kw)
            return value
        return wrapper

    def start(self, function, args, kw):
        key = (function, args)
        if self.keys:
            edge = (self.keys[-1], key)
            self.edges.append(edge)
        self.keys.append(key)

    def end(self, function, args, kw):
        self.keys.pop()
