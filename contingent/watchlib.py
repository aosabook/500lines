"""Experiment in dependency detection."""

from functools import wraps

class Watcher(object):

    def __init__(self):
        self.edges = []
        self.keys = []

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
