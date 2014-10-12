import ast, collections, dis, types, os, sys
from functools import reduce
from itertools import chain
from stack_effect import stack_effect
from check_subset import check_conformity

def assemble(assembly):
    return bytes(iter(assembly.encode(0, dict(assembly.resolve(0)))))

def plumb_depths(assembly):
    depths = [0]
    assembly.plumb(depths, {})
    return max(depths)

def make_lnotab(assembly):
    firstlineno, lnotab = None, []
    byte, line = 0, None
    for next_byte, next_line in assembly.line_nos(0):
        if firstlineno is None:
            firstlineno = line = next_line
        elif line < next_line:
            while byte+255 < next_byte:
                lnotab.extend([255, 0])
                byte = byte+255
            while line+255 < next_line:
                lnotab.extend([next_byte-byte, 255])
                byte, line = next_byte, line+255
            if (byte, line) != (next_byte, next_line):
                lnotab.extend([next_byte-byte, next_line-line])
                byte, line = next_byte, next_line
    return firstlineno or 1, bytes(lnotab)

class Assembly:
    def __add__(self, other): return Chain(self, other)
    length = 0
    def resolve(self, start): return ()
    def encode(self, start, addresses): return b''
    def line_nos(self, start): return ()
    def plumb(self, depths, labeled_depths): pass

no_op = Assembly()

class SetLineNo(Assembly):
    def __init__(self, line): self.line = line
    def line_nos(self, start): return ((start, self.line),)

class Label(Assembly):
    def resolve(self, start):
        return ((self, start),)
    def plumb(self, depths, labeled_depths):
        if self in labeled_depths: depths.append(labeled_depths[self])
        else: labeled_depths[self] = depths[-1]

def concat(assemblies):
    return reduce(Chain, assemblies, no_op)

class Chain(Assembly):
    def __init__(self, assembly1, assembly2):
        self.assembly1 = assembly1
        self.assembly2 = assembly2
        self.length = assembly1.length + assembly2.length
    def resolve(self, start):
        return chain(self.assembly1.resolve(start),
                     self.assembly2.resolve(start + self.assembly1.length))
    def encode(self, start, addresses):
        return chain(self.assembly1.encode(start, addresses),
                     self.assembly2.encode(start + self.assembly1.length, addresses))
    def line_nos(self, start):
        return chain(self.assembly1.line_nos(start),
                     self.assembly2.line_nos(start + self.assembly1.length))
    def plumb(self, depths, labeled_depths):
        self.assembly1.plumb(depths, labeled_depths)
        self.assembly2.plumb(depths, labeled_depths)

class Insn(Assembly):
    def __init__(self, encoded):
        assert isinstance(encoded, bytes) and encoded
        self.encoded = encoded
        self.length = len(encoded)
    def encode(self, start, addresses):
        return self.encoded
    def plumb(self, depths, labeled_depths):
        e = self.encoded
        effect = (stack_effect(e[0]) if len(e) == 1
                  else stack_effect(e[0], e[1] + 256*e[2]))
        depths.append(depths[-1] + effect)

class JumpInsn(Assembly):
    length = 3
    def __init__(self, opcode, label):
        self.opcode = opcode
        self.label = label
    def encode(self, start, addresses):
        base = 0 if self.opcode in dis.hasjabs else start+3
        return insn_encode(self.opcode, addresses[self.label] - base)
    def plumb(self, depths, labeled_depths):
        labeled_depths[self.label] = depths[-1] + jump_stack_effect(self.opcode)
        depths.append(depths[-1] + stack_effect(self.opcode))

def insn_encode(opcode, arg):
    return bytes([opcode, arg % 256, arg // 256])

def jump_stack_effect(opcode):
    return jump_stack_effects.get(opcode, stack_effect(opcode))
jump_stack_effects = {dis.opmap['FOR_ITER']: -1,
                      dis.opmap['JUMP_IF_TRUE_OR_POP']: 0,
                      dis.opmap['JUMP_IF_FALSE_OR_POP']: 0}

def denotation(opcode):
    if opcode < dis.HAVE_ARGUMENT:
        return Insn(bytes([opcode]))
    elif opcode in dis.hasjrel or opcode in dis.hasjabs:
        return lambda label: JumpInsn(opcode, label)
    else:
        return lambda arg: Insn(insn_encode(opcode, arg))

class Opcodes: pass
op = Opcodes()
for name, opcode in dis.opmap.items():
    setattr(op, name, denotation(opcode))

def desugar(t):
    return ast.fix_missing_locations(Expander().visit(t))

class Function(ast.FunctionDef): # from FunctionDef so that ast.get_docstring works. Ugh!
    _fields = ('name', 'args', 'body')

load, store = ast.Load(), ast.Store()

class Expander(ast.NodeTransformer):

    def visit_Lambda(self, t):
        t = self.generic_visit(t)
        result = Function('<lambda>', t.args, [ast.Return(t.body)])
        return ast.copy_location(result, t) # TODO do this automatically for every visit method?

    def visit_FunctionDef(self, t):
        t = self.generic_visit(t)
        fn = Function(t.name, t.args, t.body)
        result = ast.Assign([ast.Name(t.name, store)], fn)
        for d in reversed(t.decorator_list):
            result = ast.Call(d, [result], [], None, None)
        return ast.copy_location(result, t)

    def visit_ListComp(self, t):
        t = self.generic_visit(t)
        result_append = ast.Attribute(ast.Name('.result', load), 'append', load)
        body = ast.Expr(ast.Call(result_append, [t.elt], [], None, None))
        for loop in reversed(t.generators):
            for test in reversed(loop.ifs):
                body = ast.If(test, [body], [])
            body = ast.For(loop.target, loop.iter, [body], [])
        fn = [ast.Assign([ast.Name('.result', store)], ast.List([], load)),
              body,
              ast.Return(ast.Name('.result', load))]
        result = ast.Call(Function('<listcomp>', no_args, fn),
                          [], [], None, None)
        return ast.copy_location(result, t)

    def visit_Assert(self, t):
        t = self.generic_visit(t)
        result = ast.If(ast.UnaryOp(ast.Not(), t.test),
                        [ast.Raise(ast.Call(ast.Name('AssertionError', load),
                                            [] if t.msg is None else [t.msg],
                                            [], None, None),
                                   None)],
                        [])
        return ast.copy_location(result, t)

no_args = ast.arguments([], None, [], None, [], [])

def top_scope(t):
    top = Scope(t, ())
    top.visit(t)
    top.analyze(set())
    return top

class Scope(ast.NodeVisitor):
    def __init__(self, t, defs):
        self.t = t
        self.children = []
        self.defs = set(defs)
        self.uses = set()

    def analyze(self, parent_defs):
        self.maskvars = self.defs if isinstance(self.t, Function) else set()
        for child in self.children:
            child.analyze(parent_defs | self.maskvars)
        child_uses = set([var for child in self.children for var in child.freevars])
        uses = self.uses | child_uses
        self.cellvars = tuple(child_uses & self.maskvars)
        self.freevars = tuple(parent_defs & (uses - self.maskvars))
        self.derefvars = self.cellvars + self.freevars

    def access(self, name):
        return ('deref' if name in self.derefvars else
                'fast'  if name in self.maskvars  else
                'name')

    def get_child(self, t):
        for child in self.children:
            if child.t is t:
                return child
        assert False

    def visit_ClassDef(self, t):
        self.defs.add(t.name)
        for expr in t.bases: self.visit(expr)
        subscope = Scope(t, ())
        self.children.append(subscope)
        for stmt in t.body: subscope.visit(stmt)

    def visit_Function(self, t):
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
        if   isinstance(t.ctx, ast.Load):  self.uses.add(t.id)
        elif isinstance(t.ctx, ast.Store): self.defs.add(t.id)
        else: assert False

def byte_compile(module_name, filename, t, f_globals):
    t = desugar(t)
    check_conformity(t)
    top_level = top_scope(t)
    code = CodeGen(filename, top_level).compile(t, module_name, 0)
    return types.FunctionType(code, f_globals)

class CodeGen(ast.NodeVisitor):
    def __init__(self, filename, scope):
        self.filename  = filename
        self.scope     = scope
        self.constants = make_table()
        self.names     = make_table()
        self.varnames  = make_table()

    def compile_class(self, t):
        self.set_docstring(t)
        assembly = (self.load('__name__') + self.store('__module__')
                    + self.load_const(t.name) + self.store('__qualname__') # XXX
                    + self(t.body) + self.load_const(None) + op.RETURN_VALUE)
        return self.make_code(assembly, t.name, 0)

    def set_docstring(self, t):
        self.load_const(ast.get_docstring(t))
        
    def compile_function(self, t):
        self.set_docstring(t)
        for arg in t.args.args:
            self.varnames[arg.arg]
        return self.compile(t.body, t.name, len(t.args.args))

    def compile(self, t, name, argcount):
        assembly = self(t) + self.load_const(None) + op.RETURN_VALUE
        return self.make_code(assembly, name, argcount)

    def make_code(self, assembly, name, argcount):
        kwonlyargcount = 0
        nlocals = len(self.varnames)
        stacksize = plumb_depths(assembly)
        firstlineno, lnotab = make_lnotab(assembly)
        flags = (  (0x02 if nlocals                  else 0)
                 | (0x10 if self.scope.freevars      else 0)
                 | (0x40 if not self.scope.derefvars else 0))
        return types.CodeType(argcount, kwonlyargcount,
                              nlocals, stacksize, flags, assemble(assembly),
                              self.collect_constants(),
                              collect(self.names), collect(self.varnames),
                              self.filename, name, firstlineno, lnotab,
                              self.scope.freevars, self.scope.cellvars)

    def __call__(self, t):
        assert isinstance(t, (ast.AST, list))
        return concat(map(self, t)) if isinstance(t, list) else self.visit(t)

    def visit(self, t):
        if not hasattr(t, 'lineno'): return ast.NodeVisitor.visit(self, t)
        else: return SetLineNo(t.lineno) + ast.NodeVisitor.visit(self, t)

    def generic_visit(self, t):
        assert False, t

    def visit_Module(self, t):
        self.set_docstring(t)
        return self(t.body)

    def visit_Function(self, t):
        code = CodeGen(self.filename, self.scope.get_child(t)).compile_function(t)
        return self.make_closure(code, t.name)

    def visit_ClassDef(self, t):
        code = CodeGen(self.filename, self.scope.get_child(t)).compile_class(t)
        return (op.LOAD_BUILD_CLASS + self.make_closure(code, t.name)
                                    + self.load_const(t.name)
                                    + self(t.bases)
                + op.CALL_FUNCTION(2 + len(t.bases))
                + self.store(t.name))

    def make_closure(self, code, name):
        if code.co_freevars:
            return (concat([op.LOAD_CLOSURE(self.cell_index(name))
                            for name in code.co_freevars])
                    + op.BUILD_TUPLE(len(code.co_freevars))
                    + self.load_const(code) + self.load_const(name) + op.MAKE_CLOSURE(0))
        else:
            return self.load_const(code) + self.load_const(name) + op.MAKE_FUNCTION(0)

    def cell_index(self, name):
        return self.scope.derefvars.index(name)

    def visit_Return(self, t):
        return ((self(t.value) if t.value else self.load_const(None))
                + op.RETURN_VALUE)

    def visit_Assign(self, t):
        def compose(left, right): return op.DUP_TOP + left + right
        return self(t.value) + reduce(compose, map(self, t.targets))

    def visit_For(self, t):
        loop, end, after = Label(), Label(), Label()
        return (         op.SETUP_LOOP(after) + self(t.iter) + op.GET_ITER
                + loop + op.FOR_ITER(end) + self(t.target)
                       + self(t.body) + op.JUMP_ABSOLUTE(loop)
                + end  + op.POP_BLOCK
                + after)

    def visit_While(self, t):
        loop, end, after = Label(), Label(), Label()
        return (         op.SETUP_LOOP(after)
                + loop + self(t.test) + op.POP_JUMP_IF_FALSE(end)
                       + self(t.body) + op.JUMP_ABSOLUTE(loop)
                + end  + op.POP_BLOCK
                + after)

    def visit_If(self, t):
        orelse, after = Label(), Label()
        return (           self(t.test) + op.POP_JUMP_IF_FALSE(orelse)
                         + self(t.body) + op.JUMP_FORWARD(after)
                + orelse + self(t.orelse)
                + after)

    visit_IfExp = visit_If

    def visit_Raise(self, t):
        return self(t.exc) + op.RAISE_VARARGS(1)

    def visit_Import(self, t):
        return concat([self.import_name(0, None, alias.name)
                       + self.store(alias.asname or alias.name.split('.')[0])
                       for alias in t.names])

    def visit_ImportFrom(self, t):
        fromlist = tuple([alias.name for alias in t.names])
        return (self.import_name(t.level, fromlist, t.module)
                + concat([op.IMPORT_FROM(self.names[alias.name])
                          + self.store(alias.asname or alias.name)
                         for alias in t.names])
                + op.POP_TOP)

    def import_name(self, level, fromlist, name):
        return (self.load_const(level)
                + self.load_const(fromlist)
                + op.IMPORT_NAME(self.names[name]))

    def visit_Expr(self, t):
        return self(t.value) + op.POP_TOP

    def visit_Pass(self, t):
        return no_op

    def visit_Break(self, t):
        return op.BREAK_LOOP

    def visit_BoolOp(self, t):
        op_jump = self.ops_bool[type(t.op)]
        def compose(left, right):
            after = Label()
            return left + op_jump(after) + right + after
        return reduce(compose, map(self, t.values))
    ops_bool = {ast.And: op.JUMP_IF_FALSE_OR_POP,
                ast.Or:  op.JUMP_IF_TRUE_OR_POP}

    def visit_UnaryOp(self, t):
        return self(t.operand) + self.ops1[type(t.op)]
    ops1 = {ast.UAdd: op.UNARY_POSITIVE,  ast.Invert: op.UNARY_INVERT,
            ast.USub: op.UNARY_NEGATIVE,  ast.Not:    op.UNARY_NOT}

    def visit_BinOp(self, t):
        return self(t.left) + self(t.right) + self.ops2[type(t.op)]
    ops2 = {ast.Pow:    op.BINARY_POWER,  ast.Add:  op.BINARY_ADD,
            ast.LShift: op.BINARY_LSHIFT, ast.Sub:  op.BINARY_SUBTRACT,
            ast.RShift: op.BINARY_RSHIFT, ast.Mult: op.BINARY_MULTIPLY,
            ast.BitOr:  op.BINARY_OR,     ast.Mod:  op.BINARY_MODULO,
            ast.BitAnd: op.BINARY_AND,    ast.Div:  op.BINARY_TRUE_DIVIDE,
            ast.BitXor: op.BINARY_XOR,    ast.FloorDiv: op.BINARY_FLOOR_DIVIDE}

    def visit_Compare(self, t):
        [operator], [right] = t.ops, t.comparators
        cmp_index = dis.cmp_op.index(self.ops_cmp[type(operator)])
        return self(t.left) + self(right) + op.COMPARE_OP(cmp_index)
    ops_cmp = {ast.Eq: '==', ast.NotEq: '!=', ast.Is: 'is', ast.IsNot: 'is not',
               ast.Lt: '<',  ast.LtE:   '<=', ast.In: 'in', ast.NotIn: 'not in',
               ast.Gt: '>',  ast.GtE:   '>='}

    def visit_Set(self, t):
        return self(t.elts) + op.BUILD_SET(len(t.elts))

    def visit_Dict(self, t):
        return (op.BUILD_MAP(min(0xFFFF, len(t.keys)))
                + concat([self(v) + self(k) + op.STORE_MAP
                          for k, v in zip(t.keys, t.values)]))

    def visit_Call(self, t):
        assert len(t.args) < 256 and len(t.keywords) < 256
        return (self(t.func) + self(t.args) + self(t.keywords)
                + op.CALL_FUNCTION((len(t.keywords) << 8) | len(t.args)))

    def visit_keyword(self, t):
        return self.load_const(t.arg) + self(t.value)

    def visit_NameConstant(self, t): return self.load_const(t.value)
    def visit_Num(self, t):          return self.load_const(t.n)
    def visit_Str(self, t):          return self.load_const(t.s)
    visit_Bytes = visit_Str

    def load_const(self, constant):
        return op.LOAD_CONST(self.constants[constant, type(constant)])

    def collect_constants(self):
        return tuple([constant for constant,_ in collect(self.constants)])

    def visit_Attribute(self, t):
        sub_op = self.attr_ops[type(t.ctx)]
        return self(t.value) + sub_op(self.names[t.attr])
    attr_ops = {ast.Load: op.LOAD_ATTR, ast.Store: op.STORE_ATTR}

    def visit_Subscript(self, t):
        return self(t.value) + self(t.slice.value) + self.subscr_ops[type(t.ctx)]
    subscr_ops = {ast.Load: op.BINARY_SUBSCR, ast.Store: op.STORE_SUBSCR}

    def visit_Name(self, t):
        if   isinstance(t.ctx, ast.Load):  return self.load(t.id)
        elif isinstance(t.ctx, ast.Store): return self.store(t.id)
        else: assert False

    def load(self, name):
        access = self.scope.access(name)
        if   access == 'fast':  return op.LOAD_FAST(self.varnames[name])
        elif access == 'deref': return op.LOAD_DEREF(self.cell_index(name))
        elif access == 'name':  return op.LOAD_NAME(self.names[name])
        else: assert False

    def store(self, name):
        access = self.scope.access(name)
        if   access == 'fast':  return op.STORE_FAST(self.varnames[name])
        elif access == 'deref': return op.STORE_DEREF(self.cell_index(name))
        elif access == 'name':  return op.STORE_NAME(self.names[name])
        else: assert False

    def visit_List(self, t):  return self.visit_sequence(t, op.BUILD_LIST)
    def visit_Tuple(self, t): return self.visit_sequence(t, op.BUILD_TUPLE)

    def visit_sequence(self, t, build_op):
        if   isinstance(t.ctx, ast.Load):
            return self(t.elts) + build_op(len(t.elts))
        elif isinstance(t.ctx, ast.Store):
            return op.UNPACK_SEQUENCE(len(t.elts)) + self(t.elts)
        else:
            assert False

def make_table():
    table = collections.defaultdict(lambda: len(table))
    return table

def collect(table):
    return tuple(sorted(table, key=table.get))

def compile_file(filename, module_name):
    f = open(filename)
    source = f.read()
    f.close()
    return byte_compile(module_name, filename, ast.parse(source),
                        make_globals(module_name))

def make_globals(module_name):
    return dict(__package__=None, __spec__=None, __name__=module_name,
                __loader__=__loader__, __builtins__=__builtins__, __doc__=None)

if __name__ == '__main__':
    compile_file(sys.argv[1], '__main__')()
