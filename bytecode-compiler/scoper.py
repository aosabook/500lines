"""
Scope analysis.
TODO: Python has a symbol-table module already -- use that, to start.
We'll want to write our own again once we've fleshed out more.
"""

import ast
from symtable import symtable
from _symtable import (CELL, FREE, LOCAL, GLOBAL_IMPLICIT, GLOBAL_EXPLICIT,
                       SCOPE_MASK, SCOPE_OFF, TYPE_FUNCTION)

def top_scope(t, source, loud=False):
    top_level = Scope(symtable(source, 'filename', 'exec'))
    top_level.visit(t)
    if loud: show(top_level.st)
    return top_level

def show(st, indent=''):
    print(indent, st.get_type(), st.get_name())
    indent += '  '
    for sym in sorted(st.get_symbols(), key=lambda sym: sym.get_name()):
        print(indent, '|', sym.get_name(), properties(sym))
    for ch in st.get_children():
        show(ch, indent)

def properties(sym):
    return ' '.join(prop for prop in ('referenced', 'imported', 'parameter',
                                      'global', 'declared_global', 'local',
                                      'free', 'assigned', 'namespace')
                    if getattr(sym, 'is_' + prop)())

def find_child(st, name):
    for child in st.get_children():
        if child.get_name() == name:
            return child
    assert False

class Scope(ast.NodeVisitor):

    def __init__(self, st, defs=(), uses=()):
        self.st = st
        self.children = {}      # XXX what if multiple defs with same name?
        self.defs = set(defs)
        self.uses = set(uses)

    def scope(self, name):
        flags = self.st._table.symbols.get(name)
        if flags is None:
            return 'name'
        x = (flags >> SCOPE_OFF) & SCOPE_MASK
        if   x == GLOBAL_EXPLICIT:
            return 'global'
        elif x == LOCAL and self.st._table.type == TYPE_FUNCTION:
            return 'fast'
        elif x == FREE or x == CELL:
            assert False
        else:
            return 'name'

    def get_child(self, ast_node):
        return self.children[ast_node.name]

    def visit_ClassDef(self, t):
        self.defs.add(t.name)
        for expr in t.bases: self.visit(expr)
        self.children[t.name] = subscope = Scope(find_child(self.st, t.name))
        for stmt in t.body: subscope.visit(stmt)
        self.uses.update(subscope.uses - subscope.defs)

    def visit_FunctionDef(self, t):
        self.defs.add(t.name)
        self.children[t.name] = subscope = Scope(find_child(self.st, t.name),
                                                 t.args.args)
        for stmt in t.body: subscope.visit(stmt)
        self.uses.update(subscope.uses - subscope.defs) # maybe these nonlocals should be tracked separately?

    def visit_Assign(self, t):
        self.visit(t.value)
        # XXX self.defs.add(t.targets[0].id)

    def visit_Name(self, t):
        self.uses.add(t.id)
