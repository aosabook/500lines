"""
Check if a program conforms to our Python subset.
"""

import ast

def check_conformity(t):
    Checker().visit(t)

class Checker(ast.NodeVisitor):

    def __init__(self, in_loop=False):
        self.in_loop = in_loop

    def generic_visit(self, t):
        "Any node type we don't know about is an error."
        assert False, t

    def __call__(self, t):
        if isinstance(t, list):
            for child in t:
                self.visit(child)
        elif isinstance(t, ast.AST):
            self.visit(t)
        else:
            assert False

    def visit_Module(self, t):
        self(t.body)

    def visit_FunctionDef(self, t):
        self.check_identifier(t.name)
        self.check_arguments(t.args)
        Checker(in_loop=False)(t.body)
        assert not t.decorator_list
        assert not t.returns

    def visit_ClassDef(self, t):
        self.check_identifier(t.name)
        self(t.bases)
        assert not t.keywords
        assert not t.starargs
        assert not t.kwargs
        assert not t.decorator_list
        Checker(in_loop=False)(t.body)

    def visit_Return(self, t):
        if t.value is not None:
            self(t.value)

    def visit_Assign(self, t):
        self(t.targets)
        self(t.value)

    def visit_For(self, t):
        self(t.target)
        self(t.iter)
        Checker(in_loop=True)(t.body)
        assert not t.orelse

    def visit_While(self, t):
        self(t.test)
        Checker(in_loop=True)(t.body)
        assert not t.orelse

    def visit_If(self, t):
        self(t.test)
        self(t.body)
        self(t.orelse)

    def visit_Raise(self, t):
        self(t.exc)
        assert not t.cause

    def visit_Import(self, t):
        self(t.names)

    def visit_ImportFrom(self, t):
        self.check_identifier(t.module)
        self(t.names)

    def visit_alias(self, t):
        self.check_identifier(t.name)
        if t.asname is not None: 
            self.check_identifier(t.asname)

    def visit_Expr(self, t):
        self(t.value)

    def visit_Pass(self, t):
        pass

    def visit_Break(self, t):
        assert self.in_loop

    def visit_BoolOp(self, t):
        assert type(t.op) in self.ops_bool
        self(t.values)
    ops_bool = {ast.And, ast.Or}

    def visit_BinOp(self, t):
        assert type(t.op) in self.ops2
        self(t.left)
        self(t.right)
    ops2 = {ast.Pow,       ast.Add,
            ast.LShift,    ast.Sub,
            ast.RShift,    ast.Mult,
            ast.BitOr,     ast.Mod,
            ast.BitAnd,    ast.Div,
            ast.BitXor,    ast.FloorDiv}

    def visit_UnaryOp(self, t):
        assert type(t.op) in self.ops1
        self(t.operand)
    ops1 = {ast.UAdd,  ast.Invert,
            ast.USub,  ast.Not}

    visit_IfExp = visit_If

    def visit_Dict(self, t):
        for k, v in zip(t.keys, t.values):
            self(v)
            self(k)

    def visit_Set(self, t):
        self(t.elts)

    def visit_Compare(self, t):
        self(t.left)
        assert 1 == len(t.ops)
        assert type(t.ops[0]) in self.ops_cmp
        assert len(t.ops) == len(t.comparators)
        self(t.comparators[0])
    ops_cmp = {ast.Eq,  ast.NotEq,  ast.Is,  ast.IsNot,
               ast.Lt,  ast.LtE,    ast.In,  ast.NotIn,
               ast.Gt,  ast.GtE}

    def visit_Call(self, t):
        self(t.func)
        self(t.args)
        assert not hasattr(t, 'keywords') or not t.keywords
        assert not hasattr(t, 'starargs') or not t.starargs
        assert not hasattr(t, 'kwargs') or not t.kwargs

    def visit_Num(self, t):
        pass

    def visit_Str(self, t):
        pass

    visit_Bytes = visit_Str

    def visit_Attribute(self, t):
        self(t.value)
        self.check_identifier(t.attr)
        if   isinstance(t.ctx, ast.Load):  pass
        elif isinstance(t.ctx, ast.Store): pass
        else: assert False

    def visit_Subscript(self, t):
        self(t.value)
        if isinstance(t.slice, ast.Index):
            if   isinstance(t.ctx, ast.Load):  pass
            elif isinstance(t.ctx, ast.Store): pass
            else: assert False
            self(t.slice.value)
        else:
            assert False

    def visit_NameConstant(self, t):
        pass

    def visit_Name(self, t):
        self.check_identifier(t.id)
        if   isinstance(t.ctx, ast.Load):  pass
        elif isinstance(t.ctx, ast.Store): pass
        else: assert False

    def visit_sequence(self, t):
        self(t.elts)
        # XXX make sure there are no stars in elts
        if   isinstance(t.ctx, ast.Load):  pass
        elif isinstance(t.ctx, ast.Store): pass
        else: assert False

    visit_List = visit_sequence
    visit_Tuple = visit_sequence

    def check_arguments(self, args):
        for arg in args.args: self.check_arg(arg)
        assert not args.vararg
        assert not hasattr(args, 'varargannotation')
        assert not args.kwonlyargs
        assert not args.kwarg
        assert not hasattr(args, 'kwargannotation')
        assert not args.defaults
        assert not args.kw_defaults

    def check_arg(self, arg):
        assert not arg.annotation

    def check_identifier(self, name):
        assert isinstance(name, str)
        # Not a private, mangled name:
        assert len(name) <= 2 or not name.startswith('__') or name.endswith('__')
