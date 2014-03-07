"""
Check if a program conforms to our Python subset.
"""

import ast

def check_conformity(t):
    Checker().visit(t)

class Checker(ast.NodeVisitor):

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
#        self(t.args)
        self(t.body)
        assert not t.decorator_list
        assert not t.returns

    def visit_ClassDef(self, t):
        assert not t.decorator_list
        self(t.bases)

    def visit_Import(self, t):
        self(t.names)

    def visit_Return(self, t):
        if t.value is not None:
            self(t.value)

    def visit_If(self, t):
        self(t.test)
        self(t.body)
        self(t.orelse)

    visit_IfExp = visit_If

    def visit_While(self, t):
        self(t.test)
        self(t.body)

    def visit_For(self, t):
        self(t.iter)
        t.target.id
        self(t.body)

    def visit_Raise(self, t):
        self(t.exc)

    def visit_Expr(self, t):
        self(t.value)

    def visit_Assign(self, t):
        self(t.value)
        self(t.targets)

    def visit_Call(self, t):
        self(t.func)
        self(t.args)

    def visit_sequence(self, t):
        if   isinstance(t.ctx, ast.Load):
            self(t.elts)
        elif isinstance(t.ctx, ast.Store):
            # XXX make sure there are no stars in elts
            self(t.elts)
        else:
            assert False

    visit_List = visit_sequence
    visit_Tuple = visit_sequence

    def visit_Set(self, t):
        self(t.elts)

    def visit_Dict(self, t):
        assert len(t.keys) <= 255
        for k, v in zip(t.keys, t.values):
            self(v)
            self(k)

    def visit_UnaryOp(self, t):
        self(t.operand)
        assert type(t.op) in self.ops1
    ops1 = {ast.UAdd,  ast.Invert,
            ast.USub,  ast.Not}

    def visit_BinOp(self, t):
        self(t.left)
        self(t.right)
        assert type(t.op) in self.ops2
    ops2 = {ast.Pow,       ast.Add,
            ast.LShift,    ast.Sub,
            ast.RShift,    ast.Mult,
            ast.BitOr,     ast.Mod,
            ast.BitAnd,    ast.Div,
            ast.BitXor,    ast.FloorDiv}

    def visit_Compare(self, t):
        assert 1 == len(t.ops)
        self(t.left)
        self(t.comparators[0])
        assert type(t.ops[0]) in self.ops_cmp
    ops_cmp = {ast.Eq,  ast.NotEq,  ast.Is,  ast.IsNot,
               ast.Lt,  ast.LtE,    ast.In,  ast.NotIn,
               ast.Gt,  ast.GtE}

    def visit_BoolOp(self, t):
        self(t.values)
        assert type(t.op) in self.ops_bool
    ops_bool = {ast.And,
                ast.Or}

    def visit_Pass(self, t):
        pass

    def visit_Break(self, t):
        pass # XXX

    def visit_Num(self, t):
        pass

    def visit_Str(self, t):
        pass

    visit_Bytes = visit_Str

    def visit_Attribute(self, t):
        self(t.value)
        if   isinstance(t.ctx, ast.Load):
            pass
        elif isinstance(t.ctx, ast.Store):
            pass
        else:
            assert False

    def visit_Subscript(self, t):
        if isinstance(t.slice, ast.Index):
            if   isinstance(t.ctx, ast.Load):  pass
            elif isinstance(t.ctx, ast.Store): pass
            else: assert False
            self(t.value)
            self(t.slice.value)
        else:
            assert False

    def visit_NameConstant(self, t):
        pass

    def visit_Name(self, t):
        if   isinstance(t.ctx, ast.Load):  pass
        elif isinstance(t.ctx, ast.Store): pass
        else: assert False
