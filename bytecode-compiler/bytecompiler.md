-*- text -*-

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

    !!greet.py
    def greet(name):
        print('Hello,', name)

Chapter XXX explains how to parse it into an abstract syntax tree
(AST); in this chapter we use `ast.parse` from Python's library.

    !!example
    >>> import ast, dis, astpp      # astpp by Alex Leone
    >>> with open('greet.py') as f: module_text = f.read()
    ... 
    >>> module_ast = ast.parse(module_text)
    >>> print(astpp.dump(module_ast, include_attributes=True))
    Module(body=[
        FunctionDef(name='greet', args=arguments(args=[
            arg(arg='name', annotation=None),
          ], vararg=None, varargannotation=None, kwonlyargs=[], kwarg=None, kwargannotation=None, defaults=[], kw_defaults=[]), body=[
            Expr(value=Call(func=Name(id='print', ctx=Load(), lineno=2, col_offset=4), args=[
                Str(s='Hello,', lineno=2, col_offset=10),
                Name(id='name', ctx=Load(), lineno=2, col_offset=20),
              ], keywords=[], starargs=None, kwargs=None, lineno=2, col_offset=4), lineno=2, col_offset=4),
          ], decorator_list=[], returns=None, lineno=1, col_offset=0),
      ])

[XXX the above is too wide]

So the module becomes an `ast.Module` whose body is a list of more
`ast` objects, in this case just one, an `ast.FunctionDef`, whose body
is an `ast.Expr`, and so on: a tree of objects. Each object has fields
proper to its type, plus a `lineno` (line number) and `col_offset`
telling where in the source text it was parsed from.

The AST node types and their fields are listed in `Parser/Python.asdl`
in the CPython source distribution.


## Bytecode

Compiling the module (with Python's built-in compiler, for now)
produces a code object, an internal Python type:

    !!example
    >>> module_code = compile(module_ast, 'greet.py', 'exec')
    >>> module_code
    <code object <module> at 0xffdd7430, file "greet.py", line 1>

It has a bunch of attributes named starting with `co_`:

    !!example
    co_argcount       0
    co_cellvars       ()
    co_code           b'd\x00\x00d\x01\x00\x84\x00\x00Z\x00\x00d\x02\x00S'
    co_consts         (<code object greet at 0x7f8ada685700, file "greet.py", line 1>, 'greet', None)
    co_filename       greet.py
    co_firstlineno    1
    co_flags          64
    co_freevars       ()
    co_kwonlyargcount 0
    co_lnotab         b''
    co_name           <module>
    co_names          ('greet',)
    co_nlocals        0
    co_stacksize      2
    co_varnames       ()

[XXX the transcripts are no longer consistent in trivia like the
number 0x7f8ada685700. Do it over at the end to make it 100%
consistent.]

Our goal is to reimplement `compile`, producing an equivalent code
object. We'll dig in to these fields later, but `co_code` holds the
meat: a sequence of bytecodes in a bytestring. We can render it more
readable by disassembly, using `dis.dis`:

    !!example
    >>> dis.dis(module_code)
      1           0 LOAD_CONST               0 (<code object greet at 0x7f8ada685700, file "greet.py", line 1>)
                  3 LOAD_CONST               1 ('greet')
                  6 MAKE_FUNCTION            0
                  9 STORE_NAME               0 (greet)
                 12 LOAD_CONST               2 (None)
                 15 RETURN_VALUE

The leftmost column gives the source line number, 1; in the next
column is the name of a bytecode instruction after its offset in the
bytestring; the last column describes any arguments to the
instruction.  So the `LOAD_CONST` at offset 0, with argument 0,
appears in `co_code` as `d\x00\x00`. Each bytecode is one byte,
optionally followed by two bytes encoding an integer argument: in this
case, `d` is the ASCII code corresponding to `LOAD_CONST` (and we
don't expect the ASCII to have anything to do with the meaning; that's
just Python's standard rendering of bytestrings, which have many other
uses). `\x00\x00` are the two-byte binary encoding of the integer
0. This integer is taken by `LOAD_CONST` at runtime to index into the
`co_consts` field, which you can see above holds the code object that
`dis.dis` helpfully lists for us. In the same way, the next
instruction's argument, 1, encoded as `\x01\x00`, denotes the
`'greet'` at offset 1 of `co_consts`. `STORE_NAME`'s argument, 0,
indexes into `co_names`. On the other hand, `MAKE_FUNCTION`'s 0 just
means the function to be built will take no arguments (meaning Python
*function* arguments rather than bytecode *instruction* arguments).

When Python loads the module, its body is run: Python's inner
interpreter executes the above instructions in sequence. First
`LOAD_CONST` loads a certain code object, which we'll see next; then
`MAKE_FUNCTION` makes a function object out of it, and `STORE_NAME`
names it as `greet` in the current environment, the module's global
environment. So where's the code to the actual function? That's in
`<code object greet at 0x7f8ada685700>` and we can inspect it too:

    !!example
    co_argcount       1
    co_cellvars       ()
    co_code           b't\x00\x00d\x01\x00|\x00\x00\x83\x02\x00\x01d\x00\x00S'
    co_consts         (None, 'Hello,')
    co_filename       greet.py
    co_firstlineno    1
    co_flags          67
    co_freevars       ()
    co_kwonlyargcount 0
    co_lnotab         b'\x00\x01'
    co_name           greet
    co_names          ('print',)
    co_nlocals        1
    co_stacksize      3
    co_varnames       ('name',)

    !!dis
      2           0 LOAD_GLOBAL              0 (print)
                  3 LOAD_CONST               1 ('Hello,')
                  6 LOAD_FAST                0 (name)
                  9 CALL_FUNCTION            2 (2 positional, 0 keyword pair)
                 12 POP_TOP
                 13 LOAD_CONST               0 (None)
                 16 RETURN_VALUE

Here we see the `greet` function's body: it looks up `print`, passes
it two arguments, calls it, and finally returns `None` as the value.

(What if you're not in Python 3.4? Even in 3.3, you'd see a slightly
different disassembly even for this tiny example. If you run this
chapter's compiler in 3.3, expect the generated code to crash the
interpreter, unless you're unlucky and get some weirder result like a
wrong answer.)

To sum up, each code object supports a `co_code` bytestring plus
auxiliary data to interpret its instruction arguments and provide some
context: for example, `co_argcount` tells the interpreter to expect a
single argument to be passed to our `greet` function. [XXX too
redundant, I guess]


## The passes

I thought I'd write this without peeking at the CPython compiler until
I was done -- but I was wrong. The bytecode docs are sketchy and
occasionally out of date, and the interpreter `ceval.c` is too complex
to stand as clear documentation on its own. It's clearer what to do
(at least if you're me) if you disassemble examples of the CPython
compiler's output and read its source, with excursions into `ceval.c`,
`frameobject.c`, and others. Sometimes it even pays to read the
language specification.

There's a conventional pass structure for compilers which we can
follow, mostly mirroring CPython's:

  * Given an AST, we first 'desugar' it, replacing some of the nodes
    with equivalent code in terms of simpler node types. (The language
    minus certain features is less sweet but equally nutritious.)
    CPython doesn't have this pass, though some have suggested it
    should: optimizations would be easier to express as AST rewrites
    than bytecode rewrites. (For example: rewrite `2+3` to `5`.) I
    don't do optimizations, but wrote it this way because some node
    types are a little simpler and substantially more readable to
    compile to other node types than to bytecode.

  * We complain if the AST departs from the subset of Python we're
    going to implement. CPython lacks this pass, of course, and this
    chapter won't examine it. It's valuable in two ways: documenting
    what we claim to compile correctly, and keeping the user/developer
    from wasting time on apparent bugs on input it was never meant to
    support. [XXX add a call-out box with the ASDL grammar of Python
    ASTs, with our subset's omissions italicized or something]

  * Then we analyze the scope of variables: their definitions and uses
    in classes, functions, and function-like scopes such as lambda
    expressions. Python's built-in `symtable` module can do this, but
    we can't use it! It requires a source-code string instead of an
    AST, and ASTs don't come with a method to give us back source
    code. In the early development of this compiler I used `symtable`
    anyway, as scaffolding; the compiler then also had to take source
    code instead of an AST as input. (I would've wanted to write my
    own scope analyzer anyway, to make the compiler self-contained.)

  * With this info in hand we walk the AST corresponding to each scope
    to generate bytecode. Later we'll see this process split into
    generating symbolic assembly and then assembling it into a code
    object.

In code:

    def compile_to_code(module_name, filename, t):
        t = desugar(t)
        check_conformity(t)
        return CodeGen(filename, top_scope(t)).compile_module(t, module_name)

Throughout this compiler, `t` (for 'tree') names an AST node. These
`t`'s appear everywhere.


## Desugaring

Python's `ast` module defines visitor classes to help us walk ASTs and
transform them. The idea: for an AST node `t` of type `Foo`,
`visitor.visit(t)` gets routed to `visitor.visit_Foo(t)`. Your visitor
class can either define `visit_Foo` or follow the default behavior
which calls `visit` on each immediate sub-node of `t`.

[XXX that was perfunctory]

One type of visitor is a `NodeTransformer`. Here each visit method is
expected to return an AST node, which will be taken to replace the
node that was the argument. The default, if the sub-nodes weren't
changed, returns the same node unchanged. Desugaring uses such a
transformer:

[XXX yes, that was too brief too]

    def desugar(t):
        return ast.fix_missing_locations(Expander().visit(t))

    class Expander(ast.NodeTransformer):  # XXX rename to Desugarer?

For a start, we rewrite statements like `assert cookie, "Want
cookie!"` into `if not cookie: raise AssertionError("Want cookie!")`.
(Rather, it makes an if-then-else with the `raise` in the `else`
clause: this is slightly simpler.)

        def visit_Assert(self, t):
            t = self.generic_visit(t)
            result = ast.If(t.test,
                            [],
                            [ast.Raise(ast.Call(ast.Name('AssertionError', load),
                                                [] if t.msg is None else [t.msg],
                                                [], None, None),
                                       None)])
            return ast.copy_location(result, t)

(`generic_visit` provides the default behavior mentioned above: it
desugars any subexpressions first, recursively. `ast.copy_location`
interpolates a source-line number for any new nodes introduced.)

You might wonder if this is quite correct: what if the code being
compiled redefines `AssertionError`? But `assert` itself at runtime
uses whatever `AssertionError` is bound to.

What was the point of this visitor? Without it, we'd need to define a
`visit_Assert` in the code generator instead -- OK, fine. This would
then need to generate code to perform a test, a call, and a raise. To
do this without duplicating logic within the compiler, we'd define
code-generation functions for each of those, to be invoked by both the
new `visit_Assert` and by the code generator's `visit_If`,
`visit_Raise`, and so on. That would not be onerous, but if we're
going to have a desugaring pass at all, this is a good use for it:
it's easier and clearer.

Lambda expressions and function definitions get rewritten in terms of
a new AST node type we'll define, called `Function`:

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

This is as if we turned `def answer(): return 42` into `answer =
lambda: 42`, except that a lambda expression can't carry a
name. Reducing to just one type of function node will save work not
just in code generation but in scope analysis too, and give us a
simpler expansion for list comprehensions. This adding of a new node
type not in the official language, it feels like getting away with
something.

(The last lines implementing function decorators could've been left
out since we don't use decorators in this compiler.)

List comprehensions also create a `Function` node, holding the loop,
because the loop variables must be defined in their own scope, not
polluting the current scope as they did in Python 2.

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

The variable name `.result` can't clash with any name in the source
code (they never include a dot). `[n for n in numbers if n]` becomes a
call to a locally-defined function with the following body, except
that `result` has a name `.result` which can't occur in Python source
code, and thus can't clash with variables from the source:

    !!example.py
    result = []
    for n in numbers:
        if n:
            result.append(n)
    return result

(We see also that `.result` won't clash with other instances of
itself, because it's only used in the new function, locally.)

CPython generates more-efficient bytecode directly from the
comprehension, in a hairier way. Generator comprehensions would add
hair too; we won't need them.

`Expander` uses these helpers:

    class Function(ast.FunctionDef): # from FunctionDef so that ast.get_docstring works.
        _fields = ('name', 'args', 'body')

    load, store = ast.Load(), ast.Store()


## Analyzing scopes 

Scope analysis decides the runtime representation of variables:
'fast', 'deref', or neither. Consider:

    !!example.py
    def fn():
        f = "I'm fast"
        d = "I'm derefed"
        return g, f, d, lambda: d

`f` is a local variable used only locally within `fn`. It's 'fast': it
gets a numbered slot among the locals.

`d` is similar, but it also appears in a nested subscope, the `lambda`
expression. In the scope of `fn`, `d` is a 'deref' instead of fast. In
the lambda-expression scope, `d` is also a *free variable*: a deref
whose definition is at an enclosing scope. For code generation we need
to know the free variables so that, when the time comes to create the
lambda's function object, we'll pass it all the derefs it needs from
outside. [XXX expand?]

'g' has no local definition, and in fact no global one we can see
either. So it's neither fast nor deref. (CPython's compiler will
generate different instructions for variables known to be global --
instructions like `LOAD_GLOBAL` -- but our compiler won't. This should
be an easy improvement to make.)

A `class` scope doesn't get fast or deref variables -- only its
function-type subscopes do, such as its method definitions. Nested
classes are forbidden in this compiler, to avoid some of Python's dark
corners. So are `del` statements and explicit `nonlocal` and `global`
declarations.

Scope analysis takes two subpasses:

    def top_scope(t):
        top = Scope(t, ())
        top.visit(t)
        top.analyze(set())
        return top

The first creates subscopes for classes and functions and records the
*local* uses and definitions of variables. We must walk the AST to
collect this information, another job for an AST visitor. Unlike the
`NodeTransformer` last time, we'll leave the AST alone: our visit
methods should return `None` and fill out attributes of the `Scope`
instead. That's the protocol for a `NodeVisitor`:

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

The `analyze` subpass then deduces the *nonlocal* uses of variables:

        def analyze(self, parent_defs):
            self.fastvars = self.defs if isinstance(self.t, Function) else set()
            for child in self.children:
                child.analyze(parent_defs | self.fastvars)
            child_uses = set([var for child in self.children for var in child.freevars])
            uses = self.uses | child_uses
            self.cellvars = tuple(child_uses & self.fastvars)
            self.freevars = tuple(parent_defs & (uses - self.fastvars))
            self.derefvars = self.cellvars + self.freevars

[XXX some further explaining is needed, but the above intro gets us
close, right? ...]

A scope object also provides the above info to the code generator:

        def access(self, name):
            return ('deref' if name in self.derefvars else
                    'fast'  if name in self.fastvars  else
                    'name')

        def get_child(self, t):
            for child in self.children:
                if child.t is t:
                    return child
            assert False

[XXX contrast with CPython's more-complex process. Leaving out
GLOBAL. Contrast the symtable module's interface. What our Python
subset leaves out: nested classes, del statements. Question if we need
classes at all, given nested functions: what the compiler would look
like.]

[We could've done this in one pass, I think. But it's risky to depart
from CPython's scheme more, and wouldn't really save code here.]


## Assembly: the interface

Code generation happens next. Since it builds symbolic assembly, the
logic will be easier to follow if we get familiar with asssembly
first. Consider the `greet` function we disassembled above:

    !!dis
      2           0 LOAD_GLOBAL              0 (print)
                  3 LOAD_CONST               1 ('Hello,')
                  6 LOAD_FAST                0 (name)
                  9 CALL_FUNCTION            2 (2 positional, 0 keyword pair)
                 12 POP_TOP
                 13 LOAD_CONST               0 (None)
                 16 RETURN_VALUE

We could build this 'by hand' like so, using the assembly constructors
we'll see later:

    !!example.py
    assembly_for_greet = (SetLineNo(2)
                          + op.LOAD_GLOBAL(0)
                          + op.LOAD_CONST(1)
                          + op.LOAD_FAST(0)
                          + op.CALL_FUNCTION(2)
                          + op.POP_TOP
                          + op.LOAD_CONST(0)
                          + op.RETURN_VALUE)

`SetLineNo` is a pseudo-instruction telling which source-line the
following instructions are compiled from; `op.LOAD_GLOBAL(0)` and the
rest are symbolic instructions. These instructions and
pseudo-instructions are all represented as Python objects which can be
concatenated with `+`. The result of `+` is *also* an object
representing assembly code (as in the Gang of Four 'composite'
pattern). In generating the code we'll build it in pieces to be strung
together, more like

    !!example.py
    function_call = (op.LOAD_GLOBAL(0)
                     + op.LOAD_CONST(1)
                     + op.LOAD_FAST(0)
                     + op.CALL_FUNCTION(2))
    statement = function_call + op.POP_TOP
    assembly_for_greet = (SetLineNo(2)
                          + statement
                          + op.LOAD_CONST(0) + op.RETURN_VALUE)

and that produces the same bytecode: it doesn't matter how we chunk it
up.

Sometimes we'll want to concatenate a list of assembly code chunks:
for this we'll define `concat(assemblies)`, returning a composite.

There's one more type of assembly pseudo-instruction, the `Label` as a
target for jump instructions. It's these targets that make an
assembler a good idea: without them we could emit bytecode
directly. (My experimental first cut at a compiler worked that way,
and it'd have worked OK if not for those meddling kids, I mean if not
for some instructions that refer to absolute addresses; relative ones
are more composable.)

This assembler could have been designed to look higher-level, like
`op.LOAD_CONST('hello')` instead of `op.LOAD_CONST(0)`, the 0 being an
index into the code object's `co_consts`. That design would better
suit a general-purpose bytecode assembler; in this compiler it's
slightly simpler for the compiler to know about `co_consts`, etc., and
reusable parts just aren't a goal.


## Code generation

[XXX I think I want to reorder this, covering statements first instead
of expressions.]

The code generator is one big `NodeVisitor` class. Each part is
straightforward because the bytecodes and the AST nodes were designed
for each other, but there are dozens of node types. `CodeGen` objects
for each scope will eventually each create a code object:

    class CodeGen(ast.NodeVisitor):
        def __init__(self, filename, scope):
            self.filename  = filename
            self.scope     = scope
            self.constants = make_table()
            self.names     = make_table()
            self.varnames  = make_table()

(The table fields will become the code object's `co_consts`,
`co_names`, and `co_varnames` when complete.)

For each node type there's a visit method returning symbolic assembly
code. For example, `pass` and `break`:

        def visit_Pass(self, t):
            return no_op

        def visit_Break(self, t):
            return op.BREAK_LOOP

(`no_op` names an empty assembly with no instructions.)


### Compiling expressions

Variable names get compiled using the scopes we just analyzed:

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

        def cell_index(self, name):
            return self.scope.derefvars.index(name)

The tables like `self.varnames` are `defaultdict`s populated as
they're accessed.

Constants are also simple:

        def visit_NameConstant(self, t): return self.load_const(t.value)
        def visit_Num(self, t):          return self.load_const(t.n)
        def visit_Str(self, t):          return self.load_const(t.s)
        visit_Bytes = visit_Str

[XXX It'd make expository sense to treat the constants table here,
together with names and varnames, except for interjecting the global
`make_table`/`collect` in the middle of `CodeGen`...]

So far we've turned leaves of the expression tree into single
instructions of bytecode. What about bigger expressions? The simplest
of these use only unary operators like `not` and minus: `not -x` gets
compiled to

    !!dis
      1           0 LOAD_NAME                0 (x) 
                  3 UNARY_NEGATIVE       
                  4 UNARY_NOT            

`LOAD_NAME` produces a value, then `UNARY_NEGATIVE` acts on the value
(replacing it with its negative), then `UNARY_NOT` acts on *that*. In
general, the code for a UnaryOp expression consists of the code for
its operand followed by one instruction, a different one for each
operator:

        def visit_UnaryOp(self, t):
            return self(t.operand) + self.ops1[type(t.op)]
        ops1 = {ast.UAdd: op.UNARY_POSITIVE,  ast.Invert: op.UNARY_INVERT,
                ast.USub: op.UNARY_NEGATIVE,  ast.Not:    op.UNARY_NOT}

where `self(t.operand)`, as we'll see soon, means a recursive call to
this same visitor (`self`).

Binary operators get compiled the same way:

        def visit_BinOp(self, t):
            return self(t.left) + self(t.right) + self.ops2[type(t.op)]
        ops2 = {ast.Pow:    op.BINARY_POWER,  ast.Add:  op.BINARY_ADD,
                ast.LShift: op.BINARY_LSHIFT, ast.Sub:  op.BINARY_SUBTRACT,
                ast.RShift: op.BINARY_RSHIFT, ast.Mult: op.BINARY_MULTIPLY,
                ast.BitOr:  op.BINARY_OR,     ast.Mod:  op.BINARY_MODULO,
                ast.BitAnd: op.BINARY_AND,    ast.Div:  op.BINARY_TRUE_DIVIDE,
                ast.BitXor: op.BINARY_XOR,    ast.FloorDiv: op.BINARY_FLOOR_DIVIDE}

compiling, for example, `x-1` to

    !!dis
      1           0 LOAD_NAME                0 (x) 
                  3 LOAD_CONST               0 (1) 
                  6 BINARY_SUBTRACT

and `x*(x-1)` to

    !!dis
      1           0 LOAD_NAME                0 (x) 
                  3 LOAD_NAME                0 (x) 
                  6 LOAD_CONST               0 (1) 
                  9 BINARY_SUBTRACT        
                 10 BINARY_MULTIPLY      

How does this bytecode work? With a stack. `LOAD_NAME` and
`LOAD_CONST` each push a value onto the stack; `BINARY_SUBTRACT`
replaces the top two values with their difference, and so does
`BINARY_MULTIPLY` with their product. [XXX more explanation]

Comparisons, like `x<2`, take a slightly different form in the AST,
because Python treats a chain of comparisons specially: `1<x<2` is a
single expression as a single AST node holding a list of operators and
a list of their right operands. My subset of Python doesn't cover this
case, only binary comparisons like `x<2`. 

        def visit_Compare(self, t):
            [operator], [right] = t.ops, t.comparators
            cmp_index = dis.cmp_op.index(self.ops_cmp[type(operator)])
            return self(t.left) + self(right) + op.COMPARE_OP(cmp_index)
        ops_cmp = {ast.Eq: '==', ast.NotEq: '!=', ast.Is: 'is', ast.IsNot: 'is not',
                   ast.Lt: '<',  ast.LtE:   '<=', ast.In: 'in', ast.NotIn: 'not in',
                   ast.Gt: '>',  ast.GtE:   '>='}

All the expression types so far had a fixed number of subexpressions,
from none (names and constants), to two (binary ops). A set expression,
like `{'a', 'b', 'c'}`, can have any number of subexpressions. In the
bytecode, the values all go onto the stack, then `BUILD_SET` collects
them; it knows how many by an argument to the instruction, `3`.

    !!dis
      1           0 LOAD_CONST               0 ('a') 
                  3 LOAD_CONST               1 ('b') 
                  6 LOAD_CONST               2 ('c') 
                  9 BUILD_SET                3 

The code follows the same pattern we've seen:

        def visit_Set(self, t):
            return self(t.elts) + op.BUILD_SET(len(t.elts))

only this time we passed to `self` a *list* of subexpressions
(`t.elts`). So calls to `self` take either an AST (as before) or a
list of ASTs, and for a list we compile each subexpression and
concatenate all of the bytecodes produced:

        def __call__(self, t):
            if isinstance(t, list): return concat(map(self, t)) 
            assembly = self.visit(t)
            return SetLineNo(t.lineno) + assembly if hasattr(t, 'lineno') else assembly

Why funnel both cases into one method? Only for concision: just as the
ubiquitous AST tree-node argument is named `t` in this program, so the
ubiquitous recursive visit is named `self()`. Python's AST and
bytecodes were designed to make a list of ASTs naturally map to a
concatenated list of bytecode sequences in just this way. If these
lists only came up a few times, I'd rather handle them in a separate
method named, say, `visit_each`; but we're going to see them
everywhere.

In `CodeGen`, the purpose of visiting a node is to return assembly
code. (Contrast with `Scope`, which visits a node for the effect of
filling in the `Scope`'s fields with info about that node's subtree of
source code. [XXX delete parenthetical probably]) Each node has a
`lineno`: the line number of the source code it came from. These line
numbers get propagated into the assembly as `SetLineNo`
pseudo-instructions. [XXX plodding paragraph]

`ast.NodeVisitor`'s default implementation of `visit` then calls the
right one of the specific visit methods we've been defining -- or if
there isn't any for `t`'s type of node, it calls `generic_visit`:

        def generic_visit(self, t):
            assert False, t

Since `check_conformity` makes sure we never see an unexpected node
type, that "can't happen" -- but sometimes, of course, it did. Before
I overrode `generic_visit`, the default version succeeded silently,
making my mistakes harder to see.

Dict literals like `{'a': 'x', 'b': 'y'}` turn into code like

    !!dis
      1           0 BUILD_MAP                2
                  3 LOAD_CONST               0 ('x')
                  6 LOAD_CONST               1 ('a')
                  9 STORE_MAP           
                 10 LOAD_CONST               2 ('y')
                 13 LOAD_CONST               3 ('b')
                 16 STORE_MAP           

so:

        def visit_Dict(self, t):
            return (op.BUILD_MAP(min(0xFFFF, len(t.keys)))
                    + concat([self(v) + self(k) + op.STORE_MAP
                              for k, v in zip(t.keys, t.values)]))

The argument to `BUILD_MAP` gives the runtime a hint of the dict's
size. It's allowed to be wrong, since dicts can grow and shrink -- but
not to overflow the two bytes allotted to an argument in
bytecode. Thus the `min`. (A dict literal big enough to get clipped by
this `min` would likely use so many constants that references into the
constants table would overflow *their* two-byte encoding. Python
bytecode does have a means for extended-size arguments, which I didn't
implement; you'll get a runtime error during compiling, instead. So
why bother with the `min`? Because we may fix the extended-size
encoding issue later, and it's easier to make `BUILD_MAP` correct now
than to document the problem for later.)

Encoding of arguments affects `visit_Call` as well. First, a function
call like `f(x, y, key=42)` compiles to

    !!dis
      1           0 LOAD_NAME                0 (f)
                  3 LOAD_NAME                1 (x)
                  6 LOAD_NAME                2 (y)
                  9 LOAD_CONST               0 ('key')
                 12 LOAD_CONST               1 (42)
                 15 CALL_FUNCTION          258 (2 positional, 1 keyword pair)

The function and the arguments all go on the stack, then
`CALL_FUNCTION` takes them. It needs to know both how many positional
arguments and how many keyword arguments there are, to pop them off
and pass them to the function. These two numbers -- 2 and 1 in this
case -- are encoded together as individual bytes in the 2-byte
argument in the `CALL_FUNCTION` instruction.

        def visit_Call(self, t):
            assert len(t.args) < 256 and len(t.keywords) < 256
            return (self(t.func) + self(t.args) + self(t.keywords)
                    + op.CALL_FUNCTION((len(t.keywords) << 8) | len(t.args)))

        def visit_keyword(self, t):
            return self.load_const(t.arg) + self(t.value)

Again Python's bytecode format provides a way to encode bigger
arguments than we handle here; the `assert` guards against that
case. Why not check for it in `check_conformity()`, way back at the
top? That wouldn't be terrible, but it'd be checking against seemingly
arbitrary numbers in the midst of other code that checks against a
tree grammar. `check_conformity()` enforces the *language subset* we
claim to implement; here we enforce an implementation limit, at the
point where the reason is clear. (`check_conformity()` does screen out
some fancier call expressions like `f(*args, **kwargs)`.)

There are just a few more types of expression. A simple subscript
expression, `a[x]`, becomes

    !!dis
      1           0 LOAD_NAME                0 (a)
                  3 LOAD_NAME                1 (x)
                  6 BINARY_SUBSCR

Similarly, `a.x` becomes

    !!dis
      1           0 LOAD_NAME                0 (a)
                  3 LOAD_ATTR                1 (x)

But there's a new wrinkle: `a.x` or `a[x]` could appear on the
left-hand side of an assignment statement, like `a.x = 42`. The AST
node type for `a.x` is the same either way, but the node carries an
extra field `t.ctx` telling which kind of context it appears in
(`Load` or `Store`). (We already saw this field in `visit_Name()` but
glossed over it.) [XXX we also saw it in the scope-analysis
pass -- probably should explain it at that point.]

        def visit_Subscript(self, t):
            return self(t.value) + self(t.slice.value) + self.subscr_ops[type(t.ctx)]
        subscr_ops = {ast.Load: op.BINARY_SUBSCR, ast.Store: op.STORE_SUBSCR}

        def visit_Attribute(self, t):
            sub_op = self.attr_ops[type(t.ctx)]
            return self(t.value) + sub_op(self.names[t.attr])
        attr_ops = {ast.Load: op.LOAD_ATTR, ast.Store: op.STORE_ATTR}

A list or tuple can also appear in both load and store contexts. As a
load, `['a', 'b']` becomes

    !!dis
      1           0 LOAD_CONST               0 ('a')
                  3 LOAD_CONST               1 ('b')
                  6 BUILD_LIST               2

where 2 is the length of the list. Unlike with `BUILD_MAP` the length
must be exact.

        def visit_List(self, t):  return self.visit_sequence(t, op.BUILD_LIST)
        def visit_Tuple(self, t): return self.visit_sequence(t, op.BUILD_TUPLE)

        def visit_sequence(self, t, build_op):
            if   isinstance(t.ctx, ast.Load):
                return self(t.elts) + build_op(len(t.elts))
            elif isinstance(t.ctx, ast.Store):
                return op.UNPACK_SEQUENCE(len(t.elts)) + self(t.elts)
            else:
                assert False

Again a length greater than 0xFFFF can't be accommodated -- why don't
we check for that here? Because we're backstopped by a generic check
for *any* unencodeable argument; `visit_Call()` was not, because it
idiosyncratically encoded its two arguments into one field.

Control-flow expressions -- `and`, `or`, `if`-`else` -- will be dealt
with along with control-flow statements.


### Compiling simple statements

[Move this section above 'Compiling expressions'? It's simpler and
then there'd be a smoother transition between expressions and
control-flow. OTOH it'd break up our treatment of statements.]

Expressions get evaluated by statements of different kinds: the
assignment `y = x + 1`, the expression statement `print(42)`, and so
on. A statement ordinarily leaves the stack as it was, ready for the
next statement to run; so for an expression statement, after
evaluating the expression pushes some value onto the stack, it needs
to be removed. `POP_TOP` does this.

        def visit_Expr(self, t):
            return self(t.value) + op.POP_TOP

In the bytecode for `y = x + 1`, the popping is done by `STORE_NAME`
to `y`:

    !!dis
      1           0 LOAD_NAME                0 (x)
                  3 LOAD_CONST               0 (1)
                  6 BINARY_ADD
                  7 STORE_NAME               1 (y)

Remember that compiling an AST `Name` node depends on the context: in
load context we want code that *pushes* a value on the stack, and now
we see that in store context we want code that *consumes* a value,
popping it. This goes for all the node types that can appear in both
contexts. Now we can understand the `ast.Store` case in
`visit_sequence` above.

        def visit_Assign(self, t):
            def compose(left, right): return op.DUP_TOP + left + right
            return self(t.value) + reduce(compose, map(self, t.targets))

So why isn't `visit_Assign` just `return self(t.value) +
self(t.targets)`? An assignment may have multiple targets, like `a = b
= c = 0`. Before the value gets popped by the first store, `DUP_TOP`
for each extra target pushes an extra reference to it.

        def visit_Raise(self, t):
            return self(t.exc) + op.RAISE_VARARGS(1)

We handle only the most common form of `raise`: `raise foo`.

        def visit_Return(self, t):
            return ((self(t.value) if t.value else self.load_const(None))
                    + op.RETURN_VALUE)

We handle both `return` and `return foo`. (The no-argument `return` is
never used by this compiler.)

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

The `import` visitors are full of technical details I'll pass over. We
could instead have turned `import` statements into `__import__()`
calls and assignments, in the desugar pass; but that would take about
as much compiler code, to produce worse compiled code.


### Compiling control flow

Control-flow statements like `if`-`else` reduce to jumping around in
the bytecode. The expression statement

    !!example.py
    yes if ok else no

becomes

    !!dis
      1           0 LOAD_NAME                0 (ok)
                  3 POP_JUMP_IF_FALSE       12
                  6 LOAD_NAME                1 (yes)
                  9 JUMP_FORWARD             3 (to 15)
            >>   12 LOAD_NAME                2 (no)
            >>   15 POP_TOP

where `POP_JUMP_IF_FALSE` does what it says: pops the value left by
`ok`, tests it, and if it's false jumps to index 12. Otherwise
execution falls through to the usual next instruction, at 6.
`JUMP_FORWARD` likewise jumps, to skip `no` if we chose `yes`.

([XXX Footnote, I guess:] `POP_TOP` is not part of the code for the `if`
expression itself, it's code for the expression statement containing
the `if`. It's listed here because the `JUMP_FORWARD` jumps to
it. Every `if` appears in a context where more bytecode will follow --
a `RETURN_VALUE` instruction if nothing else.)

During code generation the compiler doesn't yet know the bytecode
index where a jump target will be. It invents a label for each target,
emits that instead, and leaves it to the next pass (the *assembler*)
to resolve all the labels.

(This design is not inevitable. In all the code so far, the target
goes after the jump, so if the compiler emitted code in last-to-first
order it'd always know the jump target. But loops have backward jumps
too. We could still do without an assembler if all jumps were relative
-- like "4 places forward" or "22 places back" -- but Python's
bytecode mixes relative and absolute. (I don't know why.) Early on I
explored a design more like a linker, pretending all references were
relative then amending the properly-absolute ones. With just a linker
and no assembler, at a cost of uglier code generation, the whole
system might be simpler and faster: you don't make up an intermediate
assembly encoding for bytecode, you just encode it. What decided me
was that an assembler can have more uses outside of this
compiler. Further, it accords with the CPython design -- it's less
likely to get borked by their changes.)

        def visit_If(self, t):
            orelse, after = Label(), Label()
            return (           self(t.test) + op.POP_JUMP_IF_FALSE(orelse)
                             + self(t.body) + op.JUMP_FORWARD(after)
                    + orelse + self(t.orelse)
                    + after)

        visit_IfExp = visit_If

The statement

    !!example.py
    if ok:
        yes
    else:
        no

compiles the same, so we use the same visitor. In the *statement*
visitor `visit_If`, `t.body` and `t.orelse` are lists of AST statement
nodes; in the expression visitor `visit_IfExp`, they are expression
nodes. This is the one place where we get more than convenience from
overloading `self()` to visit both nodes and node lists.

`and` and `or` expressions point out a new concern: keeping the stack
consistent across different branches of control. `print(x or y)` compiles to

    !!dis
      1           0 LOAD_NAME                0 (print)
                  3 LOAD_NAME                1 (x)
                  6 JUMP_IF_TRUE_OR_POP     12
                  9 LOAD_NAME                2 (y)
            >>   12 CALL_FUNCTION            1 (1 positional, 0 keyword pair)

`JUMP_IF_TRUE_OR_POP` only pops the tested value if it came out
falsey. At index 12, whichever way execution got there, the stack has
the same height: two entries.

        def visit_BoolOp(self, t):
            op_jump = self.ops_bool[type(t.op)]
            def compose(left, right):
                after = Label()
                return left + op_jump(after) + right + after
            return reduce(compose, map(self, t.values))
        ops_bool = {ast.And: op.JUMP_IF_FALSE_OR_POP,
                    ast.Or:  op.JUMP_IF_TRUE_OR_POP}

Since a `BoolOp` collects all of the subexpressions of an expression
like `a and b and c` (instead of Python representing this as a tree of
binary `BoolOp`s), we must `reduce` over the list.

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

Compiling `while` and `for`, for our subset, needs nothing new. (I
won't explain what `SETUP_LOOP` and friends do at runtime.) There's
one wrinkle: we handle `break` too (in the trivial `visit_Break` way
up at the top). What if a `break`'s outside of any loop? We should
complain. To know if it is, we'd have to track whether we're in a loop
or not. We track this in `check_conformity` instead, where it fits
cleanly.

For `continue` we'd have to track not just whether we're in a loop,
but also that loop's label to jump to. It wasn't worth it.


### Functions, classes, and modules

[XXX what if we moved this to the start of the code-generation
section? I think that'd be top-heavy. Another idea: come back to
compiling functions and classes after going all the way down through
the assembler for compiling function/class-free code. We ought to be
able to cover scope analysis together with codegen for functions and
classes. Yes, that sounds promising. Downside: order is less top-down,
breaking up the CodeGen visitor.]

A `Function` node is an expression desugared from a lambda expression,
function definition, or list comprehension. Compiling one takes two
steps: first, compile the whole nested scope into a separate code
object. Second, generate assembly code that will build a function
object out of this code object. [XXX yes, there was a disassembled
example of this near the start of this chapter, but we need to bring
it back, here, or the reader will be lost.]

        def visit_Function(self, t):
            code = self.sprout(t).compile_function(t)
            return self.make_closure(code, t.name)

The new `CodeGen` for this new code object requires a subscope of the
current scope -- previously computed by the scope analyzer, recovered
by `self.scope.get_child(t)`.

        def sprout(self, t):
            return CodeGen(self.filename, self.scope.get_child(t))

Building a function out of a code object depends on whether it has
free variables:

        def make_closure(self, code, name):
            if code.co_freevars:
                return (concat([op.LOAD_CLOSURE(self.cell_index(freevar))
                                for freevar in code.co_freevars])
                        + op.BUILD_TUPLE(len(code.co_freevars))
                        + self.load_const(code) + self.load_const(name) + op.MAKE_CLOSURE(0))
            else:
                return self.load_const(code) + self.load_const(name) + op.MAKE_FUNCTION(0)

We always add the function's code object to the constants table of our
current code object (with `load_const`). In the simple case of a
function whose variables are all local or global (no `co_freevars`),
we emit assembly that will push that code object and the function's
name on the stack and create a new function object, with
`MAKE_FUNCTION`.

[TODO: example disassembly again]

For a function with free variables the assembly has more to do: it
will collect the values of all the nonlocals into a tuple, then push
the code object and the name as before; then `MAKE_CLOSURE` expects
the tuple. The scope analyzer already arranged for the nested
function's nonlocals to be in the current scope as 'cell variables'.
[TODO review the docs/source on LOAD_CLOSURE -- IIRC it loads the
*mutable cell holding the variable's value*, instead of the value, so
that the nested function can mutate the variable.]

Back to compiling a function down to a code object: on a new `CodeGen`
we called `compile_function`:

        def compile_function(self, t):
            self.load_const(ast.get_docstring(t))
            for arg in t.args.args:
                self.varnames[arg.arg]
            assembly = self(t.body) + self.load_const(None) + op.RETURN_VALUE
            return self.make_code(assembly, t.name, len(t.args.args))

The docstring goes in the code object's constants table as the first
entry -- `None` if no docstring. This logic relies on this
`load_const` happening first, before any code generation, and on our
table preserving the order we add to it. (The actual `LOAD_CONST`
instruction is discarded, here.)

As with the docstring, the parameter names become the first elements
of the `varnames` table. (They're `defaultdict`s: fetching adds to
them, as necessary.)

We generate assembly that will run the module's body and return
`None`; then we assemble that into a code object.

On to class definitions. Like functions, they sprout a new `CodeGen`
to compile down to a code object; but the assembly that will build the
class object is a little fancier.

        def visit_ClassDef(self, t):
            code = self.sprout(t).compile_class(t)
            return (op.LOAD_BUILD_CLASS + self.make_closure(code, t.name)
                                        + self.load_const(t.name)
                                        + self(t.bases)
                    + op.CALL_FUNCTION(2 + len(t.bases))
                    + self.store(t.name))

The class definition's code object resembles an ordinary function's
with some magic local-variable definitions prepended. (The docstring
*doesn't* need to go first in the constants table for these.) Python's
class builder (`LOAD_BUILD_CLASS`) will populate the new class's
attributes from the locals dictionary as of the point this function
returns. This is why scope analysis must not classify the locals of a
class definition as 'fast'.

        def compile_class(self, t):
            docstring = ast.get_docstring(t)
            assembly = (  self.load('__name__')      + self.store('__module__')
                        + self.load_const(t.name)    + self.store('__qualname__') # XXX need to mess with t.name?
                        + self.load_const(docstring) + self.store('__doc__')
                        + self(t.body) + self.load_const(None) + op.RETURN_VALUE)
            return self.make_code(assembly, t.name, 0)

I was a little tempted to leave classes out. Python stopped needing
them once it gained proper nested functions -- in terms of expressive
power, if not familiarity and legacy. As it worked out, most of the
simplicity we'd gain by chopping out `class` we can get by forbidding
nested classes (nested in a `class` or a `def`).  [XXX was it really
'most'? I forget.]

The top-level `compile_to_code` function called the `CodeGen`
visitor's `compile_module` method. We're finally ready to see it:

        def compile_module(self, t, name):
            return self.compile_function(Function(name, ast.arguments(args=()), t.body))

Our `t` is a `Module` node. We generate code just like for a function
of no arguments with the same body -- that's how the module's body
will ultimately get run, by taking the code object, making a function
out of it, and calling that. (Hm, so why not transform modules into
functions right at the start, while desugaring? Because scope analysis
does distinguish modules and functions.)


### Making a code object

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

The code object is littered with fields:

  * The bytecode bytes themselves, from `assemble(assembly)`.

  * Redundant info too costly to compute at runtime, like `stacksize`.

  * Metadata like `lnotab`. `lnotab` tells what line number in the
    source file produced what part of the bytecode. (And don't blame
    the name on me.)

  * Tables of names or constants referred to by bytecode instructions.
    These tables take the form of tuples.

The entries to go into those last-mentioned tuples have been
accumulating in `defaultdict`s as we go. For `names` and `varnames`
the keys are the strings; but it gets trickier for
`constants`. Two constants may be equal but not the same...

(It depends on what the meaning of *is* is...)

It's not enough for two constants to compare as equal to be considered
the *same* constant ...

, and now we can collect them:


But the constants table has a wrinkle: some constants that are equal
are not identical, and to keep them from being coalesced we add their
type to the table's key:

        def load_const(self, constant):
            return op.LOAD_CONST(self.constants[constant, type(constant)])

        def collect_constants(self):
            return tuple([constant for constant,_ in collect(self.constants)])

(There are a few corner cases involving signed-zero floating-point
numbers that this addition still doesn't cover. They're caught
by `check_conformity`.)

    def make_table():
        table = collections.defaultdict(lambda: len(table))
        return table

    def collect(table):
        return tuple(sorted(table, key=table.get))


## Assembly

We still need to create assembly code -- instructions, labels,
`SetLineNo` pseudo-instructions, and so on -- and to compute three
functions of it: the stack size, the line-number table, and the
encoded bytecode. When you want multiple functions of multiple data
types, the usual way in Python is with classes, so let's do that,
starting with the simplest type of assembly fragment, the no-op:

    !!assembler.py
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

    no_op = Assembly()

`__add__` chains two assembly programs together into a longer one. In
all the places we've been composing code like `op.DUP_TOP + left +
right` we were calling this method. So does `concat`:

    !!assembler.py
    def concat(assemblies):
        return sum(assemblies, no_op)

The `length` of an assembly-code fragment counts how many bytes it
will need, encoded as bytecode. In this compiler it's constant, since
we don't support the extended-length argument format. (Suppose there
were a relative jump to an address over 65535 bytes away. The jump
instruction would need to occupy more than the usual 3 bytes; if we'd
assumed 3 bytes, this would imply cascading changes.) [XXX can you in
fact have extended jumps?]

The `resolve` and `encode` methods respectively resolve the address of
each label and use the resolved addresses to emit the bytecode bytes:

    !!assembler.py
    def assemble(assembly):
        return bytes(iter(assembly.encode(0, dict(assembly.resolve(0)))))

(Encoding and resolving both start at address 0.) 

The `plumb` method computes the stack depth at each point in the
assembly program, appending the individual depths to a list passed
in. With this information `plumb_depths` can return the total size
the stack will need:

    !!assembler.py
    def plumb_depths(assembly):
        depths = [0]
        assembly.plumb(depths)
        return max(depths)

[XXX make line_nos last in the code too?]

The last method, `line_nos`, generates the pairs (*address*,
*line_number*) that go into the line-number table. `make_lnotab`
consumes them and encodes them into a bytestring in a format imposed
by the VM interpreter. Each successive pair of bytes represents an
(address, line_number) pair as the differences from the previous
pair. Since a byte is between 0 and 255, there are two problems:

  * The difference could be negative. This can happen [XXX example]
    
  * The difference could exceed 255. In this case the entry must take
    up multiple successive byte-pairs, first increasing only the
    address part in each, and then increasing the line number.
    [XXX say something about why]

[XXX this line just for Markdown formatting, to make the snippet below
be code. sheesh.]

    !!assembler.py
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


### Assembly subclasses

A `Label` is just like `no_op` except for resolving to an address.

    !!assembler.py
    class Label(Assembly):
        def resolve(self, start):
            return ((self, start),)

So is a `SetLineNo` except for adding to the line-number table.

    !!assembler.py
    class SetLineNo(Assembly):
        def __init__(self, line):
            self.line = line
        def line_nos(self, start):
            return ((start, self.line),)

A `Chain` catenates two assembly-code fragments in sequence. It uses
`itertools.chain` to catenate the label resolutions, bytecodes, or
`lnotab` entries produced in the different methods.

    !!assembler.py
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

(I was a little surprised that no stack overflows bit me with this
code, at least not in compiling a program the size of the compiler
itself. I did not deliberately try to keep the chains balanced.)

[XXX here say something about the one-pass approach]

There are four kinds of bytecode instruction: absolute jumps, relative
jumps, and non-jumps with or without an argument. ('Jump' for our
purposes means any instruction taking an address argument.)

    !!assembler.py
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

[XXX deal with stack-effect exceptions where the standard module is
'wrong']

The depth of the stack after the instruction depends on the depth
before it: for example, a `LOAD_CONST` increases it by one. This logic
disregards jumps: after an unconditional jump, the next instruction,
which must be the target of some *other* jump, gets its stack depth
from there instead. In this compiler, though, it happens that the
straight-line stack depths are always right.


### Opcode definitions

Finally, we define `op` so that names like `op.POP_TOP` and
`op.CALL_FUNCTION(0)` work as we've been using them.

    !!assembler.py
    def denotation(opcode):
        if opcode < dis.HAVE_ARGUMENT:
            return Instruction(opcode, None)
        else:
            return lambda arg: Instruction(opcode, arg)

    op = type('op', (), dict([(name, denotation(opcode))
                              for name, opcode in dis.opmap.items()]))


## The top-level driver

At the top level, we compile a module from a source file:

    !!top.py
    import ast, collections, dis, types, sys
    from functools import reduce
    from itertools import chain
    from stack_effect import stack_effect
    from check_subset import check_conformity

    def compile_file(filename, module_name):
        f = open(filename)
        source = f.read()
        f.close()
        return compile_to_callable(module_name, filename, ast.parse(source))

Recall that a module is compiled to the code object for a no-argument function.

    def compile_to_callable(module_name, filename, t):
        code = compile_to_code(module_name, filename, t)
        f_globals = dict(__package__=None, __spec__=None, __name__=module_name,
                         __loader__=__loader__, __builtins__=__builtins__,
                         __doc__=ast.get_docstring(t))
        return types.FunctionType(code, f_globals)

And from the command line, given a filename, we compile and (using
`()`) run it.

    if __name__ == '__main__':
        sys.argv.pop(0)
        compile_file(sys.argv[0], '__main__')()

`pop` removes the initial `argv[0]` to leave the command-line
arguments the same as if we'd run Python on the source program
directly: thus (with this compiler in `compiler.py`) you can run
`python greet.py`, or `python compiler.py greet.py`, or `python
compiler.py compiler.py greet.py`...


## Reflections

XXX fill me in


## Further reading

Peter Norvig's _Paradigms of Artificial Intelligence Programming_,
despite the title, presents short compilers for Scheme, Prolog, and a
pattern-matching rewrite-rule language. They all go to some trouble to
produce efficient output. The code is of the highest quality, and not
much longer than this book's examples.

Niklaus Wirth's [_Compiler
Construction_](http://www.ethoberon.ethz.ch/WirthPubl/CBEAll.pdf)
details a simple compiler from Oberon-0 to RISC machine code. Wirth
and Gutknecht's [_Project Oberon: The Design of an Operating System, a
Compiler, and a Computer_](http://projectoberon.com/) elaborates it to
the full Oberon language it's written in, ending up at about 3000
lines.

Andrew Appel's _Modern Compiler Implementation in ML_ explains the
ideas behind fancier optimizing compilers.

CPython 2 has a `compiler` module in 4500 lines of Python, seemingly
included just for fun. For the compiler that's normally run see
`compile.c` and `symtable.c`; there's also the optimizer `peephole.c`.


[notes to self:]

In this chapter there's so much of this pattern:

    !!example
    The expression

    !!example
        blah

    !!example
    compiles to

    !!example
        1    BLAH
             GOOBER

    !!example
    This works like blah wooie wooie.

    !!example
        def visit_Blah(self, t):
            return op.BLAH + op.GOOBER

    !!example
    Here's an observation about the `BLAH` instruction or something.

that I wonder if some sort of table would streamline the
presentation. Maybe we could standardly put the example `blah` to the
left, next to its disassembly "1   BLAH     GOOBER", and that would at
least economize on stereotyped lines like "compiles to" here.


If there's time for a big rewrite (oh, sure) I'm leaning towards *not*
presenting the whole 500-line compiler, but smaller bits with more
about rejected alternatives.


Try harder to be systematic in language about phases? i.e. when we're
discussing the compiler vs. when the compiler compiles vs. when the
output bytecode runs. How about: 'I' wrote the compiler, 'we' compile
source to bytecode, 'it' will run? -- different actors and tenses


Maybe remark near the top, along these lines: to fully understand the
code you need to understand the details of Python's semantics, the AST
types, the code objects and the bytecode format, and instruction
execution.  Some of these have only sketchy and occasionally wrong
documentation, especially towards the end of that list. (I learned
them by disassembling examples and reading the CPython sources.) I'm
going to skip most of these details. And sometimes I won't mention
what parts of full Python get left out. [That aspect needs my
conscious judgement, though -- come back to that.]


I hate writing "AST nodes" so often -- it's jargony. Better ideas?


[XXX it's kind of bad to call a chunk of assembler code an 'assembly':
it's really a thing-to-be-assembled. Have a better name?]


XXX resync with compiler.py -- it's definitely out of sync now


To do: example of compiling the compiler


data types, another way of breaking things out:
  ast
  ast visitor
    ast transformer
  desugared ast
    Function
  Scope
  Assembly
    Instruction
    Label
    SetLineNo
    Chain
  op {name: Instruction or arg->Instruction}
  CodeType
  FunctionType
  line-number table
  constants tables
