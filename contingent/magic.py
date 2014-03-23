"""Magically detect dependencies between methods and attributes."""

import os
import time
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

    def wait(self):
        """Wait for any previously-read files to change, and re-read them."""
        changed_paths = []
        start = time.time()
        while not changed_paths:
            time.sleep(1.0)
            print('-')
            changed_paths = [path for path in self._paths
                             if os.stat(path).st_mtime > start]
        for path in changed_paths:
            # ?
            self.read(path)
