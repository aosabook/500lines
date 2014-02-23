"""
Scope analysis.
TODO: Python has a symbol-table module already -- use that, to start.
We'll want to write our own again once we've fleshed out more.
"""

import ast

def top_scope(t):
    scope_map = {}
    top_level = Scope(scope_map)
    top_level.visit(t)
    scope_map['global'] = top_level
    return scope_map, top_level

class Scope(ast.NodeVisitor):

    def __init__(self, scope_map, defs=(), uses=()):
        self.scope_map = scope_map
        self.defs = set(defs)
        self.uses = set(uses)

    def scope(self, name):
        # XXX crude approximation of Python's actual scope rules
        if name in self.defs and self is not self.scope_map['global']:
            return 'local'
        elif name in self.scope_map['global'].defs:
            return 'global'
        else:
            return 'unknown'

    def visit_FunctionDef(self, t):
        self.defs.add(t.name)
        subscope = Scope(self.scope_map, [t.name] + t.args.args)
        for stmt in t.body: subscope.visit(stmt)
        self.scope_map[t] = subscope
        self.uses.update(subscope.uses - subscope.defs) # maybe these nonlocals should be tracked separately?

    def visit_Assign(self, t):
        assert 1 == len(t.targets) and isinstance(t.targets[0], ast.Name)
        self.visit(t.value)
        self.defs.add(t.targets[0].id)

    def visit_Name(self, t):
        self.uses.add(t.id)
