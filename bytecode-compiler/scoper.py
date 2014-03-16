"""
Analyze variable scope.
"""

import ast

def top_scope(t, loudness=0):
    top = Scope(t)
    top.visit(t)
    top.analyze(set())
    return top

def get_type(t):
    if   isinstance(t, ast.Module):
        return 'module'
    elif isinstance(t, ast.ClassDef):
        return 'class'
    elif isinstance(t, ast.FunctionDef): # XXX and Lambda?
        return 'function'
    else:
        assert False

class Scope(ast.NodeVisitor):

    def __init__(self, t, defs=()):
        self.t = t
        self.children = []
        self.defs = set(defs)
        self.uses = set()

    def dump(self, indent=''):
        print(indent, get_type(self.t), getattr(self.t, 'name', '<nameless>'))
        indent += '  '
        for name in sorted(self.defs | self.uses):
            print(indent, '| %-8s %s' % (self.access(name), name))
        for ch in self.children:
            ch.dump(indent)

    def analyze(self, parent_defs):
        # XXX we're currently assuming classes are never nested in anything
        self.maskvars = self.defs if get_type(self.t) == 'function' else set()
        for child in self.children:
            child.analyze(parent_defs | self.maskvars)
        child_freevars = set().union(*[child.freevars for child in self.children])
        self.cellvars = tuple(child_freevars & self.maskvars)
        self.freevars = tuple(parent_defs & ((self.uses | child_freevars) - self.maskvars))
        self.derefvars = self.cellvars + self.freevars

    def access(self, name):
        return ('deref' if name in self.derefvars else
                'fast' if name in self.maskvars else
                'name')

    def get_child(self, t):
        for child in self.children:
            if child.t is t:
                return child
        assert False

    def visit_ClassDef(self, t):
        self.defs.add(t.name)
        for expr in t.bases: self.visit(expr)
        subscope = Scope(t)
        self.children.append(subscope)
        for stmt in t.body: subscope.visit(stmt)

    def visit_FunctionDef(self, t):
        self.defs.add(t.name)
        subscope = Scope(t, [arg.arg for arg in t.args.args])
        self.children.append(subscope)
        for stmt in t.body: subscope.visit(stmt)

    def visit_Import(self, t):
        for alias in t.names:
            self.defs.add(alias.asname or alias.name.split('.')[0])

    def visit_ImportFrom(self, t):
        for alias in t.names:
            self.defs.add(alias.asname or alias.name)

    def visit_Name(self, t):
        if   isinstance(t.ctx, ast.Load):  return self.uses.add(t.id)
        elif isinstance(t.ctx, ast.Store): return self.defs.add(t.id)
        else: assert False

if __name__ == '__main__':
    import sys
    import check_subset

    filename = sys.argv[1]
    source = open(filename).read()
    t = ast.parse(source)
    try:
        check_subset.check_conformity(t)
    except AssertionError:
        print(filename, "doesn't conform.")
        sys.exit(1)
    print(filename)
    top_scope(t, source).dump()
