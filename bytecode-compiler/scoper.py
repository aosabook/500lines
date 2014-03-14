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
    return top_level

def show(st, indent):
    print(indent, st.get_type(), st.get_name())
    indent += '  '
    for sym in sorted(st.get_symbols(), key=lambda sym: sym.get_name()):
        print(indent, '| %-8s %-20s %s' % (access(st, sym.get_name()),
                                           sym.get_name(),
                                           properties(sym)))
    for ch in st.get_children():
        show(ch, indent)

def properties(sym):
    return ' '.join(prop for prop in ('referenced', 'imported', 'parameter',
                                      'global', 'declared_global', 'local',
                                      'free', 'assigned', 'namespace')
                    if getattr(sym, 'is_' + prop)())

def access(st, name):
    flags = st._table.symbols.get(name)
    x = (flags >> SCOPE_OFF) & SCOPE_MASK if flags else None
    if   x == GLOBAL_EXPLICIT:
        return 'global'
    elif x == LOCAL and st._table.type == TYPE_FUNCTION:
        return 'fast'
    elif x in (CELL, FREE):
        return 'deref'
    else:
        return 'name'

class Scope(ast.NodeVisitor):

    def __init__(self, st, defs=(), uses=()):
        self.st = st
        self.children = {}

    def dump(self, indent=''):
        show(self.st, '')

    def scope(self, name):
        return access(self.st, name)

    def get_cellvars(self):
        return tuple(name for name,flags in self.st._table.symbols.items()
                     if CELL == ((flags >> SCOPE_OFF) & SCOPE_MASK))

    def get_freevars(self):
        return tuple(name for name,flags in self.st._table.symbols.items()
                     if FREE == ((flags >> SCOPE_OFF) & SCOPE_MASK))

    def get_child(self, t):
        return self.children[id(t)]

    def visit_ClassDef(self, t):
        for expr in t.bases: self.visit(expr)
        self.children[id(t)] = subscope = Scope(find_child(self.st, t.name))
        for stmt in t.body: subscope.visit(stmt)

    def visit_FunctionDef(self, t):
        self.children[id(t)] = subscope = Scope(find_child(self.st, t.name),
                                                t.args.args)
        for stmt in t.body: subscope.visit(stmt)

def find_child(st, name):
    for child in st.get_children():
        if child.get_name() == name:
            return child
    assert False

if __name__ == '__main__':
    import sys
    filename = sys.argv[1]
    source = open(filename).read()
    t = ast.parse(source)
    top_scope(t, source).dump()
