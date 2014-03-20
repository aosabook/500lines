"""Rough experiment from which to derive the design of `contingent`."""

from functools import wraps
from inspect import ismethod


class Base:
    def __getattribute__(self, name):
        if name.startswith('_'):
            return object.__getattribute__(self, name)
        target = (self, name)
        return self._builder.get(target)

    def __setattr__(self, name, value):
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
