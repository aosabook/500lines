-*- text -*-

> "Python is about having the simplest, dumbest compiler imaginable."
> -- Guido von Rossum, *Masterminds of Programming*

People write source code, machines run machine code. A compiler turns
one into the other -- how? The whiff of magic to this hasn't quite
gone away: you might spend a semester building one compiler for a much
simpler language than the one you wrote it in. Did you just call on a
big genie to make a small one?

To dispel the mystery, there are some great short compilers to
read. This chapter will add another toy example compiler to the
literature, this time trying to keep it real in an unusual way: being
able to compile itself, and not omitting the practical details of
features like debug support. Since something's gotta go, we'll skip
the whole topic of optimization.

Our source language and implementation language -- the language we
compile from, and the one we code the compiler in -- are a subset of
Python 3.4. (Deciding just what subset took some exploring.) The
target machine is the CPython bytecode virtual machine, also version
3.4.


## Abstract syntax trees

Let's start with an example: a module `greet.py` with this text:

    # in greet.py:
    name = 'Monty'
    print('Hello,', name)

Chapter XXX explains how to parse it into an abstract syntax tree
(AST); in this chapter we use `ast.parse` from Python's library.

    # in transcripts:
    >>> import ast, dis, astpp      # pip install astpp (by Alex Leone)
    >>> with open('greet.py') as f: module_text = f.read()
    ... 
    >>> module_ast = ast.parse(module_text)
    >>> print(astpp.dump(module_ast, include_attributes=True))
    Module(body=[
        Assign(targets=[
            Name(id='name', ctx=Store(), lineno=1, col_offset=0),
          ], value=Str(s='Monty', lineno=1, col_offset=7), lineno=1, col_offset=0),
        Expr(value=Call(func=Name(id='print', ctx=Load(), lineno=2, col_offset=0), args=[
            Str(s='Hello,', lineno=2, col_offset=6),
            Name(id='name', ctx=Load(), lineno=2, col_offset=16),
          ], keywords=[], starargs=None, kwargs=None, lineno=2, col_offset=0), lineno=2, col_offset=0),
      ])

So the module becomes an `ast.Module` whose body is a list of more
`ast` objects, in this case two, an `ast.Assign` and an `ast.Expr`,
and so on: a tree of objects. Each object has fields proper to its
type, plus a `lineno` (line number) and `col_offset` telling where in
the source text it was parsed from.

The AST node types and their fields are listed in `Parser/Python.asdl`
in the CPython source distribution.


## Bytecode

Compiling the module (with Python's built-in compiler, for now)
produces a code object, an internal Python type:

    # in transcripts:
    >>> module_code = compile(module_ast, 'greet.py', 'exec')
    >>> module_code
    <code object <module> at 0xffdd76b0, file "greet.py", line 1>

It has a bunch of attributes named starting with `co_`:

    # in examples:
    co_argcount       0
    co_cellvars       ()
    co_code           b'd\x00\x00Z\x00\x00e\x01\x00d\x01\x00e\x00\x00\x83\x02\x00\x01d\x02\x00S'
    co_consts         ('Monty', 'Hello,', None)
    co_filename       greet.py
    co_firstlineno    1
    co_flags          64
    co_freevars       ()
    co_kwonlyargcount 0
    co_lnotab         b'\x06\x01'
    co_name           <module>
    co_names          ('name', 'print')
    co_nlocals        0
    co_stacksize      3
    co_varnames       ()

[XXX redo this in Py3.4]

What happened to our program? Mostly it's encoded into `co_code`: a
sequence of bytecodes in a bytestring. We can render it more readable
by disassembly, using `dis.dis`:

    # in transcripts:
    >>> dis.dis(module_code)
      1           0 LOAD_CONST               0 ('Monty')
                  3 STORE_NAME               0 (name)

      2           6 LOAD_NAME                1 (print)
                  9 LOAD_CONST               1 ('Hello,')
                 12 LOAD_NAME                0 (name)
                 15 CALL_FUNCTION            2 (2 positional, 0 keyword pair)
                 18 POP_TOP
                 19 LOAD_CONST               2 (None)
                 22 RETURN_VALUE

On the left are source-code line numbers (1 and 2); down the middle go
bytecode instructions, each with the address it's encoded at; and on
the right is an optional argument to each instruction. So the first
line shows us the instruction at address 0: a `LOAD_CONST`
instruction, with 0 as an argument. (For a `LOAD_CONST` the argument
means an index into the `co_consts` attribute, whose 0th entry, above,
is indeed `'Monty'`.) This instruction appears in `co_code` as
`d\x00\x00`: one byte `d` which happens to mean `LOAD_CONST`, followed
by two bytes encoding the integer 0.

Since that first instruction took three bytes, the next (`STORE_NAME`)
appears at address 3. It also has 0 as its argument, but this time
indexing into `co_names`. (That is, at runtime the bytecode
interpreter will take the value it just loaded and store it in the
variable listed at index 0 of `co_names`: which is `name`.)

Those two bytecodes implemented the first line of source code (`name =
'Monty'`). The code for the second line follows at addresses 6 through
18, with some new wrinkles in instruction arguments: `CALL_FUNCTION`
has two, each of one byte; and `POP_TOP` has no argument, making the
whole instruction fit in a byte. 

Finally, the last two instructions (at 19 and 22) return from running
the module.

(What if you're not in Python 3.4? Even in 3.3, you'd see a slightly
different disassembly even for this tiny example. If you run this
chapter's compiler in 3.3, expect the generated code to crash the
interpreter, unless you're unlucky and get some weirder result like a
wrong answer.)


### Assembly: the interface

To create the code object we saw above, it'd help to be able to write
Python code that looks like the disassembly. Like this:

    # in examples.py:
    assembly_for_greet = (  SetLineNo(1)
                          + op.LOAD_CONST(0)
                          + op.STORE_NAME(0)
                          + SetLineNo(2)
                          + op.LOAD_NAME(1)
                          + op.LOAD_CONST(1)
                          + op.LOAD_NAME(0)
                          + op.CALL_FUNCTION(2)
                          + op.POP_TOP
                          + op.LOAD_CONST(0)
                          + op.RETURN_VALUE)

We'll come back to implementing the support code that lets us write
this; first let's see how it's used. `SetLineNo` is a
pseudo-instruction telling which source-line the following
instructions are compiled from; `op.LOAD_GLOBAL(0)` and the rest are
symbolic instructions. These instructions and pseudo-instructions are
all represented as Python objects which can be concatenated with
`+`. The result of `+` is *also* an object representing assembly code
(as in the Gang of Four 'composite' pattern). In generating the code
we'll build it in pieces to be strung together, more like

    # in examples.py:
    stmt1 = op.LOAD_CONST(0) + op.STORE_NAME(0)
    call = (op.LOAD_NAME(1) + op.LOAD_CONST(1) + op.LOAD_NAME(0)
            + op.CALL_FUNCTION(2))
    stmt2 = call + op.POP_TOP
    assembly_for_greet = (  SetLineNo(1) + stmt1
                          + SetLineNo(2) + stmt2
                          + op.LOAD_CONST(0) + op.RETURN_VALUE)
 
which produces the same bytecode: it doesn't matter how we chunk the
assembly.

A higher-level assembly language could've been made where instead of
`op.LOAD_CONST(0)` this example would have `op.LOAD_CONST('Monty')`,
leaving it to the assembler to turn `'Monty'` into an index into
`co_consts`. Likewise `op.STORE_NAME` would take a string argument,
and so on. Such a design would better suit a general-purpose bytecode
assembler; but this compiler will be ruthless in doing only what's
needed. (I wasn't sure it could fit readably in 500 lines at all.)
It's simple for the code generator to encode the arguments into
integers, more so than for the assembler.


## The spike: a working end-to-end model

Our goal is to reimplement `compile`, producing an equivalent code
object. We'll learn as we go about the details I've left unexplained
so far; this mirrors how I learned about Python bytecode while
developing the program. The sketchy and occasionally outdated
documentation on the bytecode virtual machine made me fall back on
disassembling examples out of the real Python compiler, and reading
the compiler's source. (The bytecode interpreter's source `ceval.c`
was too complex to clarify everything on its own.) I would rather have
coded this without peeking until I was done, on the grounds that you
learn more that way; but that way I bogged down in crashes that turned
out to hinge on mysteries like just what values must go into
`co_flags` under what conditions.

To face these uncertainties and start learning, let's make a complete
working system for a tiny core subset of the problem: just enough to
run our example.

    # in bytecompile.py:
    import ast, collections, dis, types, sys
    from functools import reduce
    from itertools import chain
    from stack_effect import stack_effect
    from check_subset import check_conformity

Here are all of the imports for the complete compiler to come,
although not all will be used in this first spike. Next we start at
the top, the entry point to compile and run a (micro-)Python source
file:

    def run(filename, module_name):
        compile_file(filename, module_name)()

    def compile_file(filename, module_name):
        f = open(filename)
        source = f.read()
        f.close()
        return compile_to_callable(module_name, filename, ast.parse(source))

    def compile_to_callable(module_name, filename, t):
        code = compile_to_code(module_name, filename, t)
        f_globals = dict(__package__=None, __spec__=None, __name__=module_name,
                         __loader__=__loader__, __builtins__=__builtins__,
                         __doc__=ast.get_docstring(t))
        return types.FunctionType(code, f_globals)

Compiling the module produces a function of no arguments, which we
call to run. The logic was split into two extra functions as
alternative entry points for testing. (This chapter will leave out the
automated tests.)

Why doesn't `compile_file` use a `with` statement? Because this
chapter's code will only use constructs implemented in this chapter.

Throughout the compiler, `t` (for 'tree') names an AST node. These
`t`'s appear everywhere.


### A visitor generates assembly code

Python's `ast` module defines visitor classes to help us walk through
ASTs.

    # in examples.py:
    class ExampleVisitor(ast.NodeVisitor):
        def visit_Module(self, t):
            return "(A module {})".format(' '.join(map(self.visit, t.body)))
        def visit_Expr(self, t):
            return "(An expression {})".format(self.visit(t.value))
        def visit_Num(self, t):
            return "(A number {})".format(t.n)

Back to the compiler:

    # in bytecompile.py:
    <<compile to code>>
    <<the assembler>>

    class CodeGen(ast.NodeVisitor):

        def compile_module(self, t, name):
            assembly = self(t.body) + self.load_const(None) + op.RETURN_VALUE
            return self.make_code(assembly, name, 0)

        def __call__(self, t):
            if isinstance(t, list): return concat(map(self, t)) 
            assembly = self.visit(t)
            return SetLineNo(t.lineno) + assembly if hasattr(t, 'lineno') else assembly

        def generic_visit(self, t):
            assert False, t

        def __init__(self, filename, scope):
            self.filename  = filename
            self.scope     = scope
            self.constants = make_table()
            self.names     = make_table()
            self.varnames  = make_table()

        def make_code(self, assembly, name, argcount):
            kwonlyargcount = 0
            nlocals = len(self.varnames)
            stacksize = plumb_depths(assembly)
            flags = (  (0x02 if nlocals                  else 0)
                     | (0x10 if self.scope.freevars      else 0)
                     | (0x40 if not self.scope.derefvars else 0))
            firstlineno, lnotab = make_lnotab(assembly)
            return types.CodeType(argcount, kwonlyargcount,
                                  nlocals, stacksize, flags, assemble(assembly),
                                  self.collect_constants(),
                                  collect(self.names), collect(self.varnames),
                                  self.filename, name, firstlineno, lnotab,
                                  self.scope.freevars, self.scope.cellvars)

        def load_const(self, constant):
            return op.LOAD_CONST(self.constants[constant, type(constant)])

        def collect_constants(self):
            return tuple([constant for constant,_ in collect(self.constants)])

    <<code generation 0>>
    <<code generation 1>>
    <<code generation 2>>

    def make_table():
        table = collections.defaultdict(lambda: len(table))
        return table

    def collect(table):
        return tuple(sorted(table, key=table.get))

    if __name__ == '__main__':
        sys.argv.pop(0)
        run(sys.argv[0], '__main__')

For v0:

    # in compile to code v0:
    def compile_to_code(module_name, filename, t):
        return CodeGen(filename, StubScope()).compile_module(t, module_name)

    class StubScope: freevars, cellvars, derefvars = (), (), ()

Code generation:

    # in code generation 0:
        def visit_Expr(self, t):
            return self(t.value) + op.POP_TOP

        def visit_Assign(self, t):
            def compose(left, right): return op.DUP_TOP + left + right
            return self(t.value) + reduce(compose, map(self, t.targets))

        def visit_Name(self, t):
            if   isinstance(t.ctx, ast.Load):  return self.load(t.id)
            elif isinstance(t.ctx, ast.Store): return self.store(t.id)
            else: assert False

    <<generate variable accesses>>

        def visit_NameConstant(self, t): return self.load_const(t.value)
        def visit_Num(self, t):          return self.load_const(t.n)
        def visit_Str(self, t):          return self.load_const(t.s)
        visit_Bytes = visit_Str

        def visit_Call(self, t):
            assert len(t.args) < 256 and len(t.keywords) < 256
            return (self(t.func) + self(t.args) + self(t.keywords)
                    + op.CALL_FUNCTION((len(t.keywords) << 8) | len(t.args)))

        def visit_keyword(self, t):
            return self.load_const(t.arg) + self(t.value)

and

    # in generate variable accesses v0:
        def load(self, name):  return op.LOAD_NAME(self.names[name])
        def store(self, name): return op.STORE_NAME(self.names[name])

The literate-programming chunks <<code generation 1>> and <<code
generation 2>> are empty until later versions of the compiler:

    # in code generation 1:

and

    # in code generation 2:



### Just enough assembly

    # in the assembler:
    <<assembly types and functions>>

    def denotation(opcode):
        if opcode < dis.HAVE_ARGUMENT:
            return Instruction(opcode, None)
        else:
            return lambda arg: Instruction(opcode, arg)

    op = type('op', (), dict([(name, denotation(opcode))
                              for name, opcode in dis.opmap.items()]))

A stub for now:

    # in assembly types and functions v0:
    def Instruction(opcode, arg):
        return bytes([opcode] if arg is None else [opcode, arg % 256, arg // 256])

    def SetLineNo(lineno): return b''

    def concat(assemblies): return b''.join(assemblies)

    def assemble(assembly): return assembly

    def plumb_depths(assembly): return 10

    def make_lnotab(assembly): return 1, b''

And now we can compile the example we started with, `greet.py`.


## Fleshing it out

For v1 we'll compile control structures and more operators. A symbolic
assembler helps with the control structures:

    # in assembly types and functions v1:
    class Assembly:
        def __add__(self, other):
            return Chain(self, other)
        length = 0
        def resolve(self, start):
            return ()
        def encode(self, start, addresses):
            return b''
        def line_nos(self, start):
            return ()
        def plumb(self, depths):
            pass

    class Instruction(Assembly):
        def __init__(self, opcode, arg):
            self.opcode = opcode
            self.arg    = arg
            self.length = 1 if arg is None else 3
        def encode(self, start, addresses):
            if   self.opcode in dis.hasjabs: arg = addresses[self.arg]
            elif self.opcode in dis.hasjrel: arg = addresses[self.arg] - (start+3)
            else:                            arg = self.arg
            if arg is None: return bytes([self.opcode])
            else:           return bytes([self.opcode, arg % 256, arg // 256])
        def plumb(self, depths):
            depths.append(depths[-1] + stack_effect(self.opcode, self.arg))

    class Label(Assembly):
        def resolve(self, start):
            return ((self, start),)

    class SetLineNo(Assembly):
        def __init__(self, line):
            self.line = line
        def line_nos(self, start):
            return ((start, self.line),)

    class Chain(Assembly):
        def __init__(self, assembly1, assembly2):
            self.part1 = assembly1
            self.part2 = assembly2
            self.length = assembly1.length + assembly2.length
        def resolve(self, start):
            return chain(self.part1.resolve(start),
                         self.part2.resolve(start + self.part1.length))
        def encode(self, start, addresses):
            return chain(self.part1.encode(start, addresses),
                         self.part2.encode(start + self.part1.length, addresses))
        def line_nos(self, start):
            return chain(self.part1.line_nos(start),
                         self.part2.line_nos(start + self.part1.length))
        def plumb(self, depths):
            self.part1.plumb(depths)
            self.part2.plumb(depths)

    no_op = Assembly()

    def concat(assemblies):
        return sum(assemblies, no_op)

    def assemble(assembly):
        return bytes(iter(assembly.encode(0, dict(assembly.resolve(0)))))

    def plumb_depths(assembly):
        depths = [0]
        assembly.plumb(depths)
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

We use it in now in compiling:

    # in code generation 1 v1:
        def visit_Dict(self, t):
            return (op.BUILD_MAP(min(0xFFFF, len(t.keys)))
                    + concat([self(v) + self(k) + op.STORE_MAP
                              for k, v in zip(t.keys, t.values)]))

        def visit_Subscript(self, t):
            return self(t.value) + self(t.slice.value) + self.subscr_ops[type(t.ctx)]
        subscr_ops = {ast.Load: op.BINARY_SUBSCR, ast.Store: op.STORE_SUBSCR}

        def visit_Attribute(self, t):
            sub_op = self.attr_ops[type(t.ctx)]
            return self(t.value) + sub_op(self.names[t.attr])
        attr_ops = {ast.Load: op.LOAD_ATTR, ast.Store: op.STORE_ATTR}

        def visit_List(self, t):  return self.visit_sequence(t, op.BUILD_LIST)
        def visit_Tuple(self, t): return self.visit_sequence(t, op.BUILD_TUPLE)

        def visit_sequence(self, t, build_op):
            if   isinstance(t.ctx, ast.Load):
                return self(t.elts) + build_op(len(t.elts))
            elif isinstance(t.ctx, ast.Store):
                return op.UNPACK_SEQUENCE(len(t.elts)) + self(t.elts)
            else:
                assert False

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

        def visit_Pass(self, t):
            return no_op

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

        def visit_If(self, t):
            orelse, after = Label(), Label()
            return (           self(t.test) + op.POP_JUMP_IF_FALSE(orelse)
                             + self(t.body) + op.JUMP_FORWARD(after)
                    + orelse + self(t.orelse)
                    + after)

        visit_IfExp = visit_If

        def visit_BoolOp(self, t):
            op_jump = self.ops_bool[type(t.op)]
            def compose(left, right):
                after = Label()
                return left + op_jump(after) + right + after
            return reduce(compose, map(self, t.values))
        ops_bool = {ast.And: op.JUMP_IF_FALSE_OR_POP,
                    ast.Or:  op.JUMP_IF_TRUE_OR_POP}

        def visit_While(self, t):
            loop, end, after = Label(), Label(), Label()
            return (         op.SETUP_LOOP(after)
                    + loop + self(t.test) + op.POP_JUMP_IF_FALSE(end)
                           + self(t.body) + op.JUMP_ABSOLUTE(loop)
                    + end  + op.POP_BLOCK
                    + after)

        def visit_For(self, t):
            loop, end, after = Label(), Label(), Label()
            return (         op.SETUP_LOOP(after) + self(t.iter) + op.GET_ITER
                    + loop + op.FOR_ITER(end) + self(t.target)
                           + self(t.body) + op.JUMP_ABSOLUTE(loop)
                    + end  + op.POP_BLOCK
                    + after)

## Functions and classes

Certain Python constructs introduce nested scopes. With them
implemented in v2, we'll be able to compile this compiler.

    # in compile to code v2:
    def compile_to_code(module_name, filename, t):
        t = desugar(t)
        check_conformity(t)
        return CodeGen(filename, top_scope(t)).compile_module(t, module_name)

    def desugar(t):
        return ast.fix_missing_locations(Expander().visit(t))

    class Expander(ast.NodeTransformer):  # XXX rename to Desugarer?

        def visit_Assert(self, t):
            t = self.generic_visit(t)
            result = ast.If(t.test,
                            [],
                            [ast.Raise(ast.Call(ast.Name('AssertionError', load),
                                                [] if t.msg is None else [t.msg],
                                                [], None, None),
                                       None)])
            return ast.copy_location(result, t)

        def visit_Lambda(self, t):
            t = self.generic_visit(t)
            result = Function('<lambda>', t.args, [ast.Return(t.body)])
            return ast.copy_location(result, t)

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
            no_args = ast.arguments([], None, [], None, [], [])
            result = ast.Call(Function('<listcomp>', no_args, fn),
                              [], [], None, None)
            return ast.copy_location(result, t)

    class Function(ast.FunctionDef): # from FunctionDef so that ast.get_docstring works.
        _fields = ('name', 'args', 'body')

    load, store = ast.Load(), ast.Store()

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

        def analyze(self, parent_defs):
            self.fastvars = self.defs if isinstance(self.t, Function) else set()
            for child in self.children:
                child.analyze(parent_defs | self.fastvars)
            child_uses = set([var for child in self.children for var in child.freevars])
            uses = self.uses | child_uses
            self.cellvars = tuple(child_uses & self.fastvars)
            self.freevars = tuple(parent_defs & (uses - self.fastvars))
            self.derefvars = self.cellvars + self.freevars

        def access(self, name):
            return ('deref' if name in self.derefvars else
                    'fast'  if name in self.fastvars  else
                    'name')

        def get_child(self, t):
            for child in self.children:
                if child.t is t:
                    return child
            assert False

and

    # in generate variable accesses v2:
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

and

    # in code generation 2 v2:
        def cell_index(self, name):
            return self.scope.derefvars.index(name)

        def visit_Return(self, t):
            return ((self(t.value) if t.value else self.load_const(None))
                    + op.RETURN_VALUE)

        def visit_Function(self, t):
            code = self.sprout(t).compile_function(t)
            return self.make_closure(code, t.name)

        def sprout(self, t):
            return CodeGen(self.filename, self.scope.get_child(t))

        def make_closure(self, code, name):
            if code.co_freevars:
                return (concat([op.LOAD_CLOSURE(self.cell_index(freevar))
                                for freevar in code.co_freevars])
                        + op.BUILD_TUPLE(len(code.co_freevars))
                        + self.load_const(code) + self.load_const(name) + op.MAKE_CLOSURE(0))
            else:
                return self.load_const(code) + self.load_const(name) + op.MAKE_FUNCTION(0)

        def compile_function(self, t):
            self.load_const(ast.get_docstring(t))
            for arg in t.args.args:
                self.varnames[arg.arg]
            assembly = self(t.body) + self.load_const(None) + op.RETURN_VALUE
            return self.make_code(assembly, t.name, len(t.args.args))

        def visit_ClassDef(self, t):
            code = self.sprout(t).compile_class(t)
            return (op.LOAD_BUILD_CLASS + self.make_closure(code, t.name)
                                        + self.load_const(t.name)
                                        + self(t.bases)
                    + op.CALL_FUNCTION(2 + len(t.bases))
                    + self.store(t.name))

        def compile_class(self, t):
            docstring = ast.get_docstring(t)
            assembly = (  self.load('__name__')      + self.store('__module__')
                        + self.load_const(t.name)    + self.store('__qualname__') # XXX need to mess with t.name?
                        + self.load_const(docstring) + self.store('__doc__')
                        + self(t.body) + self.load_const(None) + op.RETURN_VALUE)
            return self.make_code(assembly, t.name, 0)
