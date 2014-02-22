"""
Byte-compile trivial programs with if, while, globals, calls,
function defs, and a bit more.
"""

import ast, collections, dis, types
from assembler import op, assemble

def bytecomp(t, f_globals):
    scope_map = {}
    top_level = Scope(scope_map)
    top_level.visit(t)
    scope_map['global'] = top_level
    return types.FunctionType(CodeGen(scope_map, top_level).compile(t), f_globals)

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

class CodeGen(ast.NodeVisitor):

    def __init__(self, scope_map, scope):
        self.scope_map = scope_map
        self.scope = scope
        self.constants = make_table()
        self.names     = make_table()
        self.varnames  = make_table()

    def compile_function(self, t):
        stmt0 = t.body[0]
        if not (isinstance(stmt0, ast.Expr) and isinstance(stmt0.value, ast.Str)):
            self.constants[None] # The doc comment starts the constant table.
        for arg in t.args.args:
            self.varnames[arg.arg] # argh, naming
        return self.compile(t.body, len(t.args.args))

    def compile(self, t, argcount=0):
        bytecode = [self.of(t), self.load_const(None), op.RETURN_VALUE]
        kwonlyargcount = 0
        nlocals = len(self.varnames)
        stacksize = 10          # XXX
        flags = 64  # XXX I don't understand the flags
        flags |= 2 if nlocals else 0  # this is just a guess
        filename = '<stdin>'
        name = 'the_name'
        firstlineno = 1
        lnotab = b''
        return types.CodeType(argcount, kwonlyargcount, nlocals, stacksize, flags,
                              assemble(bytecode),
                              collect(self.constants),
                              collect(self.names),
                              collect(self.varnames),
                              filename, name, firstlineno, lnotab,
                              freevars=(), cellvars=())

    def of(self, t):
        assert isinstance(t, list) or isinstance(t, ast.AST)
        return list(map(self.of, t)) if isinstance(t, list) else self.visit(t)

    def load_const(self, constant):
        return op.LOAD_CONST(self.constants[constant])

    def generic_visit(self, t):
        assert False, t

    def visit_Module(self, t):
        return self.of(t.body)

    def visit_Import(self, t):
        return self.of(t.names)

    def visit_alias(self, t):
        return [self.load_const(0),
                self.load_const(None), # XXX not for 'importfrom'
                op.IMPORT_NAME(self.names[t.name]),
                self.store(t.asname or t.name.split('.')[0])]
        
    def visit_FunctionDef(self, t):
        assert not t.decorator_list
        code = CodeGen(self.scope_map, self.scope_map[t]).compile_function(t)
        return [self.load_const(code), op.MAKE_FUNCTION(0), self.store(t.name)]

    def visit_Return(self, t):
        return [self.of(t.value) if t.value else self.load_const(None),
                op.RETURN_VALUE]

    def visit_If(self, t):
        return {0: [self.of(t.test), op.POP_JUMP_IF_FALSE(1),
                    self.of(t.body), op.JUMP_FORWARD(2)],
                1: [self.of(t.orelse)],
                2: []}

    def visit_While(self, t):
        return {0: [op.SETUP_LOOP(3)],
                1: [self.of(t.test), op.POP_JUMP_IF_FALSE(2),
                    self.of(t.body), op.JUMP_ABSOLUTE(1)],
                2: [op.POP_BLOCK],
                3: []}

    def visit_Expr(self, t):
        return [self.of(t.value), op.POP_TOP]

    def visit_Assign(self, t):
        assert 1 == len(t.targets) and isinstance(t.targets[0], ast.Name)
        return [self.of(t.value), self.store(t.targets[0].id)]

    def visit_Call(self, t):
        return [self.of(t.func), self.of(t.args), op.CALL_FUNCTION(len(t.args))]

    def visit_List(self, t):
        return [self.of(t.elts), op.BUILD_LIST(len(t.elts))]

    def visit_Tuple(self, t):
        return [self.of(t.elts), op.BUILD_TUPLE(len(t.elts))]

    def visit_Dict(self, t):
        return [op.BUILD_MAP(len(t.keys)),
                [[self.of(v), self.of(k), op.STORE_MAP]
                 for k, v in zip(t.keys, t.values)]]

    def visit_UnaryOp(self, t):
        return [self.of(t.operand), self.ops1[type(t.op)]]
    ops1 = {ast.UAdd: op.UNARY_POSITIVE,  ast.Invert: op.UNARY_INVERT,
            ast.USub: op.UNARY_NEGATIVE,  ast.Not:    op.UNARY_NOT}

    def visit_BinOp(self, t):
        return [self.of(t.left), self.of(t.right), self.ops2[type(t.op)]]
    ops2 = {ast.Add:  op.BINARY_ADD,              ast.Pow:    op.BINARY_POWER,
            ast.Sub:  op.BINARY_SUBTRACT,         ast.LShift: op.BINARY_LSHIFT,
            ast.Mult: op.BINARY_MULTIPLY,         ast.RShift: op.BINARY_RSHIFT,
            ast.Mod:  op.BINARY_MODULO,           ast.BitOr:  op.BINARY_OR,
            ast.Div:  op.BINARY_TRUE_DIVIDE,      ast.BitAnd: op.BINARY_AND,
            ast.FloorDiv: op.BINARY_FLOOR_DIVIDE, ast.BitXor: op.BINARY_XOR}

    def visit_Pass(self, t):
        return []

    def visit_Break(self, t):
        return [op.BREAK_LOOP]

    def visit_Num(self, t):
        return self.load_const(t.n)

    def visit_Str(self, t):
        return self.load_const(t.s)

    def visit_Attribute(self, t):
        return [self.of(t.value), op.LOAD_ATTR(self.names[t.attr])]

    def visit_Subscript(self, t):
        return [self.of(t.value), self.of(t.slice)]

    def visit_Index(self, t):
        return [self.of(t.value), op.BINARY_SUBSCR]

    def visit_Name(self, t):
        level = self.scope.scope(t.id)
        # TODO: check if it's a constant like None
        if level == 'local':    return op.LOAD_FAST(self.varnames[t.id])
        elif level == 'global': return op.LOAD_GLOBAL(self.names[t.id])
        else:                   return op.LOAD_NAME(self.names[t.id])

    def store(self, name):
        level = self.scope.scope(name)
        # XXX global is not getting detected
        if level == 'local':    return op.STORE_FAST(self.varnames[name])
        elif level == 'global': return op.STORE_GLOBAL(self.names[name])
        else:                   return op.STORE_NAME(self.names[name])

def make_table():
    table = collections.defaultdict(lambda: len(table))
    return table

def collect(table):
    return tuple(sorted(table, key=table.get))


if __name__ == '__main__':

    def diss(code):
        codepp(code)
        dis.dis(code)
        for c in code.co_consts:
            if isinstance(c, types.CodeType):
                print()
                print('------', c, '------')
                diss(c)

    def codepp(code):
        for k in dir(code):
            if k.startswith('co_'):
                print(k, getattr(code, k))

    eg_ast = ast.parse("""
import math
print(['m', 'n'][0])
(pow, len)
{'a': 42, 'b': 55}
{}
None
ga = 2+3
def f(a):
    "doc comment"
    while a:
        pass
        if a - 1:
            print(a, 137)
        a = a - 1
    return pow(2, 16)
    return
print(f(ga))
t = True
while t:
    break
print(-math.sqrt(2))
""")
    try:
        import astpp
    except ImportError:
        astpp = ast
    print(astpp.dump(eg_ast))
    f = bytecomp(eg_ast, globals())
    diss(f.__code__)
    f()   # It's alive!
