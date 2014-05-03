import ast, collections, dis, types
from functools import reduce
from stack_effect import stack_effect
from check_subset import check_conformity

def jump_stack_effect(opcode):
    return jump_stack_effects.get(opcode, stack_effect(opcode))
jump_stack_effects = {dis.opmap['FOR_ITER']: -1,
                      dis.opmap['JUMP_IF_TRUE_OR_POP']: 0,
                      dis.opmap['JUMP_IF_FALSE_OR_POP']: 0}

def take_argument(opcode):
    code0 = encode(opcode, 0)
    if opcode in dis.hasjrel:
        return lambda label: (code0, [(label, lambda addr: addr, jump_stack_effect(opcode))], stack_effect(opcode), None)
    elif opcode in dis.hasjabs:
        return lambda label: (code0, [(label, lambda addr: 0, jump_stack_effect(opcode))], stack_effect(opcode), None)
    else:
        return lambda arg: (encode(opcode, arg), [], stack_effect(opcode, arg), None)

def encode(opcode, arg): return [opcode, arg % 256, arg // 256]

class Opcodes: pass
op = Opcodes()
for name, opcode in dis.opmap.items():
    defn = (take_argument(opcode) if dis.HAVE_ARGUMENT <= opcode
            else ([opcode], [], stack_effect(opcode), None))
    setattr(op, name, defn)

def set_lineno(line):
    return ([], [], 0, line)

def assemble(assembly):
    code = []
    max_depth = 0
    firstlineno, lnotab, cur_byte, cur_line = None, [], 0, None

    def flatten(assembly, refs, depth, depth_at_label):
        nonlocal max_depth, firstlineno, lnotab, cur_byte, cur_line
        if isinstance(assembly, tuple):
            my_code, my_linking, my_stack_effect, my_lineno = assembly

            if my_lineno is not None:
                if firstlineno is None:
                    firstlineno = cur_line = my_lineno
                elif my_lineno > cur_line: # XXX should be != ideally
                    byte_step = len(code) - cur_byte
                    line_step = my_lineno - cur_line
                    cur_byte, cur_line = len(code), my_lineno
                    while 255 < byte_step:
                        lnotab += [255, 0]
                        byte_step -= 255
                    while 255 < line_step:
                        lnotab += [byte_step, 255]
                        byte_step = 0
                        line_step -= 255
                    if (byte_step, line_step) != (0, 0):
                        lnotab += [byte_step, line_step]
                    
            for label, fixup, stack_effect in my_linking:
                refs.append((len(code) + 1, label, fixup))
                depth_at_label[label] = depth + stack_effect

            depth += my_stack_effect
            max_depth = max(max_depth, depth)

            code.extend(my_code)

        elif isinstance(assembly, list):
            for subassembly in assembly:
                depth = flatten(subassembly, refs, depth, depth_at_label)

        elif isinstance(assembly, dict):
            my_refs = []
            my_addresses = {}
            my_depth_at_label = {}
            for label, subassembly in sorted(assembly.items()):
                my_addresses[label] = len(code)
                depth = my_depth_at_label.get(label, depth)
                depth = flatten(subassembly, my_refs, depth, my_depth_at_label)
            for address, label, fixup in my_refs:
                target = my_addresses[label] - fixup(address+2)
                code[address+0] = target % 256
                code[address+1] = target // 256

        else:
            raise TypeError("Not an assembly", assembly)

        return depth

    refs = []
    depth_at_label = {}
    flatten(assembly, refs, 0, depth_at_label)
    assert not refs and not depth_at_label
    return bytes(tuple(code)), max_depth, firstlineno, bytes(lnotab)

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
        for d in reversed(t.decorator_list):  # TODO use reduce?
            result = ast.Call(d, [result], [], None, None)
        return ast.copy_location(result, t)

    def visit_ListComp(self, t):
        t = self.generic_visit(t)
        body = ast.Expr(ast.Call(ast.Attribute(ast.Name('.result', load), 'append', load),
                                 [t.elt], [], None, None))
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

def get_type(t):
    if   isinstance(t, ast.Module):
        return 'module'
    elif isinstance(t, ast.ClassDef):
        return 'class'
    elif isinstance(t, Function):
        return 'function'
    else:
        assert False

class Scope(ast.NodeVisitor):
    def __init__(self, t, defs):
        self.t = t
        self.children = []
        self.defs = set(defs)
        self.uses = set()

    def dump(self, indent):
        print(indent, get_type(self.t), getattr(self.t, 'name', '<nameless>'))
        indent = indent + '  '
        for name in sorted(self.defs | self.uses):
            print(indent, '| %-8s %s' % (self.access(name), name))
        for ch in self.children:
            ch.dump(indent)

    def analyze(self, parent_defs):
        self.maskvars = self.defs if get_type(self.t) == 'function' else set()
        for child in self.children:
            child.analyze(parent_defs | self.maskvars)
        child_freevars = set([var for child in self.children for var in child.freevars])
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
        if   isinstance(t.ctx, ast.Load):  return self.uses.add(t.id)
        elif isinstance(t.ctx, ast.Store): return self.defs.add(t.id)
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
        assembly = [self.load('__name__'), self.store('__module__'),
                    self.load_const(t.name), self.store('__qualname__'), # XXX
                    self(t.body), self.load_const(None), op.RETURN_VALUE]
        return self.make_code(assembly, t.name, 0)

    def set_docstring(self, t):
        self.load_const(ast.get_docstring(t))
        
    def compile_function(self, t):
        self.set_docstring(t)
        for arg in t.args.args:
            self.varnames[arg.arg] # argh, naming
        return self.compile(t.body, t.name, len(t.args.args))

    def compile(self, t, name, argcount):
        assembly = [self(t), self.load_const(None), op.RETURN_VALUE]
        return self.make_code(assembly, name, argcount)

    def make_code(self, assembly, name, argcount):
        kwonlyargcount = 0
        nlocals = len(self.varnames)
        bytecode, stacksize, firstlineno, lnotab = assemble(assembly)
        flags = (0x00 | (0x02 if nlocals else 0)
                      | (0x10 if self.scope.freevars else 0)
                      | (0x40 if not self.scope.derefvars else 0))
        constants = tuple([constant for constant,_ in collect(self.constants)])
        return types.CodeType(argcount, kwonlyargcount,
                              nlocals, stacksize, flags, bytecode,
                              constants,
                              collect(self.names), collect(self.varnames),
                              self.filename, name, firstlineno, lnotab,
                              self.scope.freevars, self.scope.cellvars)

    def __call__(self, t):
        assert isinstance(t, (ast.AST, list))
        return list(map(self, t)) if isinstance(t, list) else self.visit(t)

    def visit(self, t):
        lineno = getattr(t, 'lineno', None)
        assembly = ast.NodeVisitor.visit(self, t)
        return assembly if lineno is None else [set_lineno(t.lineno), assembly]

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
        return [op.LOAD_BUILD_CLASS, self.make_closure(code, t.name), 
                                     self.load_const(t.name),
                                     self(t.bases),
                op.CALL_FUNCTION(2 + len(t.bases)),
                self.store(t.name)]

    def make_closure(self, code, name):
        if not code.co_freevars:
            return [self.load_const(code),
                    self.load_const(name),
                    op.MAKE_FUNCTION(0)] # 0 = # of default args
        else:
            return [[op.LOAD_CLOSURE(self.cell_index(name))
                     for name in code.co_freevars],
                    op.BUILD_TUPLE(len(code.co_freevars)),
                    self.load_const(code),
                    self.load_const(name),
                    op.MAKE_CLOSURE(0)] # 0 = # of default args

    def cell_index(self, name):
        return self.scope.derefvars.index(name)

    def visit_Return(self, t):
        return [self(t.value) if t.value else self.load_const(None),
                op.RETURN_VALUE]

    def visit_Assign(self, t):
        def compose(left, right): return [op.DUP_TOP, left, right]
        return [self(t.value), reduce(compose, map(self, t.targets))]

    def visit_For(self, t):
        return {0: [op.SETUP_LOOP(3), self(t.iter), op.GET_ITER],
                1: [op.FOR_ITER(2), self(t.target),
                    self(t.body), op.JUMP_ABSOLUTE(1)],
                2: [op.POP_BLOCK],
                3: []}

    def visit_While(self, t):
        return {0: [op.SETUP_LOOP(3)],
                1: [self(t.test), op.POP_JUMP_IF_FALSE(2),
                    self(t.body), op.JUMP_ABSOLUTE(1)],
                2: [op.POP_BLOCK],
                3: []}

    def visit_If(self, t):
        return {0: [self(t.test), op.POP_JUMP_IF_FALSE(1),
                    self(t.body), op.JUMP_FORWARD(2)],
                1: [self(t.orelse)],
                2: []}

    visit_IfExp = visit_If

    def visit_Raise(self, t):
        return [self(t.exc), op.RAISE_VARARGS(1)]

    def visit_Import(self, t):
        return [[self.import_name(0, None, alias.name),
                 self.store(alias.asname or alias.name.split('.')[0])]
                for alias in t.names]

    def visit_ImportFrom(self, t):
        fromlist = tuple([alias.name for alias in t.names])
        return [self.import_name(t.level, fromlist, t.module),
                [[op.IMPORT_FROM(self.names[alias.name]),
                  self.store(alias.asname or alias.name)]
                 for alias in t.names],
                op.POP_TOP]

    def import_name(self, level, fromlist, name):
        return [self.load_const(level),
                self.load_const(fromlist),
                op.IMPORT_NAME(self.names[name])]

    def visit_Expr(self, t):
        return [self(t.value), op.POP_TOP]

    def visit_Pass(self, t):
        return []

    def visit_Break(self, t):
        return op.BREAK_LOOP

    def visit_BoolOp(self, t):
        op_jump = self.ops_bool[type(t.op)]
        def compose(left, right):
            return {0: [left, op_jump(1), right],
                    1: []}
        return reduce(compose, map(self, t.values))
    ops_bool = {ast.And: op.JUMP_IF_FALSE_OR_POP,
                ast.Or:  op.JUMP_IF_TRUE_OR_POP}

    def visit_UnaryOp(self, t):
        return [self(t.operand), self.ops1[type(t.op)]]
    ops1 = {ast.UAdd: op.UNARY_POSITIVE,  ast.Invert: op.UNARY_INVERT,
            ast.USub: op.UNARY_NEGATIVE,  ast.Not:    op.UNARY_NOT}

    def visit_BinOp(self, t):
        return [self(t.left), self(t.right), self.ops2[type(t.op)]]
    ops2 = {ast.Pow:    op.BINARY_POWER,  ast.Add:  op.BINARY_ADD,
            ast.LShift: op.BINARY_LSHIFT, ast.Sub:  op.BINARY_SUBTRACT,
            ast.RShift: op.BINARY_RSHIFT, ast.Mult: op.BINARY_MULTIPLY,
            ast.BitOr:  op.BINARY_OR,     ast.Mod:  op.BINARY_MODULO,
            ast.BitAnd: op.BINARY_AND,    ast.Div:  op.BINARY_TRUE_DIVIDE,
            ast.BitXor: op.BINARY_XOR,    ast.FloorDiv: op.BINARY_FLOOR_DIVIDE}

    def visit_Compare(self, t):
        [operator], [right] = t.ops, t.comparators
        cmp_index = dis.cmp_op.index(self.ops_cmp[type(operator)])
        return [self(t.left), self(right), op.COMPARE_OP(cmp_index)]
    ops_cmp = {ast.Eq: '==', ast.NotEq: '!=', ast.Is: 'is', ast.IsNot: 'is not',
               ast.Lt: '<',  ast.LtE:   '<=', ast.In: 'in', ast.NotIn: 'not in',
               ast.Gt: '>',  ast.GtE:   '>='}

    def visit_Set(self, t):
        return [self(t.elts), op.BUILD_SET(len(t.elts))]

    def visit_Dict(self, t):
        assert len(t.keys) < 256
        return [op.BUILD_MAP(len(t.keys)),
                [[self(v), self(k), op.STORE_MAP]
                 for k, v in zip(t.keys, t.values)]]

    def visit_Call(self, t):
        assert len(t.args) < 256 and len(t.keywords) < 256
        return [self(t.func), self(t.args), self(t.keywords),
                op.CALL_FUNCTION((len(t.keywords) << 8) | len(t.args))]

    def visit_keyword(self, t):
        return [self.load_const(t.arg), self(t.value)]

    def visit_Num(self, t):
        return self.load_const(t.n)

    def visit_Str(self, t):
        return self.load_const(t.s)

    visit_Bytes = visit_Str

    def visit_Attribute(self, t):
        if   isinstance(t.ctx, ast.Load):  sub_op = op.LOAD_ATTR
        elif isinstance(t.ctx, ast.Store): sub_op = op.STORE_ATTR
        else: assert False
        return [self(t.value), sub_op(self.names[t.attr])]

    def visit_Subscript(self, t):
        if isinstance(t.slice, ast.Index):
            if   isinstance(t.ctx, ast.Load):  sub_op = op.BINARY_SUBSCR
            elif isinstance(t.ctx, ast.Store): sub_op = op.STORE_SUBSCR
            else: assert False
            return [self(t.value), self(t.slice.value), sub_op]
        else:
            assert False

    def visit_NameConstant(self, t):
        return self.load_const(t.value)

    def load_const(self, constant):
        return op.LOAD_CONST(self.constants[constant, type(constant)])

    def visit_Name(self, t):
        if   isinstance(t.ctx, ast.Load):  return self.load(t.id)
        elif isinstance(t.ctx, ast.Store): return self.store(t.id)
        else: assert False

    def load(self, name):
        access = self.scope.access(name)
        if   access == 'fast':   return op.LOAD_FAST(self.varnames[name])
        elif access == 'deref':  return op.LOAD_DEREF(self.cell_index(name))
        elif access == 'global': return op.LOAD_GLOBAL(self.names[name])
        elif access == 'name':   return op.LOAD_NAME(self.names[name])
        else: assert False

    def store(self, name):
        access = self.scope.access(name)
        if   access == 'fast':   return op.STORE_FAST(self.varnames[name])
        elif access == 'deref':  return op.STORE_DEREF(self.cell_index(name))
        elif access == 'global': return op.STORE_GLOBAL(self.names[name])
        elif access == 'name':   return op.STORE_NAME(self.names[name])
        else: assert False

    def visit_List(self, t):
        return self.visit_sequence(t, op.BUILD_LIST)

    def visit_Tuple(self, t):
        return self.visit_sequence(t, op.BUILD_TUPLE)

    def visit_sequence(self, t, build_op):
        if   isinstance(t.ctx, ast.Load):
            return [self(t.elts), build_op(len(t.elts))]
        elif isinstance(t.ctx, ast.Store):
            return [op.UNPACK_SEQUENCE(len(t.elts)), self(t.elts)]
        else:
            assert False

def make_table():
    table = collections.defaultdict(lambda: len(table))
    return table

def collect(table):
    return tuple(sorted(table, key=table.get))
