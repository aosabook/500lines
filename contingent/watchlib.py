"""Experiment in dependency detection."""

from functools import wraps

class Watcher(object):

    def __init__(self):
        self.depth = 0

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
        print ' ' * self.depth, 'starting', function.__name__
        self.depth += 1

    def end(self, function, args, kw):
        print ' ' * self.depth, 'ending', function.__name__
        self.depth -= 1
