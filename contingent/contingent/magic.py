"""Magically detect dependencies between methods and attributes."""

from . import utils
from functools import wraps
from inspect import ismethod


class Base:
    """Base class for build processes that use methods and attributes."""

    def __getattribute__(self, name):
        """Instead of always computing attributes, ask our builder for them."""

        if name.startswith('_'):
            return object.__getattribute__(self, name)
        target = (self, name)
        return self._builder.get(target)

    def __setattr__(self, name, value):
        """Perform a normal setattr, plus let the builder know about it."""

        if not name.startswith('_'):
            target = (self, name)
            self._builder.set(target, value)
        return object.__setattr__(self, name, value)


def compute(target, get):
    if isinstance(target[1], str):
        obj, attribute_name = target
        value = object.__getattribute__(obj, attribute_name)
        if not ismethod(value):
            return value
        method = value
        @wraps(method)
        def wrapper(*args):
            target = (method, args)
            return get(target)
        return wrapper
    else:
        method, args = target
        return method(*args)


class Filesystem(Base):
    """Dependency magic for opening files and watching for changes."""

    def __init__(self):
        self._paths = []

    def read(self, path, mode='r'):
        """Return the contents of the file at `path`."""
        self._paths.append(path)
        with open(path, mode) as f:
            return f.read()

    def _wait(self):
        """Wait for any previously-read files to change, and re-read them."""
        changed_paths = utils.wait_on(self._paths)
        for path in changed_paths:
            bound_method = object.__getattribute__(self, 'read')
            self._builder.cache._todo.add((bound_method, (path,)))
            self.read(path)
