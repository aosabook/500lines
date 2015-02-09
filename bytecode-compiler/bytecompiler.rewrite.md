> "Python is about having the simplest, dumbest compiler imaginable."
> --- Guido von Rossum in *Masterminds of Programming*

People write source code, machines run machine code. A compiler turns
one into the other---how? The whiff of magic to this hasn't quite
gone away: you might spend a semester building one compiler, for a
much simpler language than the one you wrote it in. Did you just call
on a big genie to make a small one?

To dispel the mystery, there are some great short compilers to
read. This chapter will add another toy example one to the literature,
this time trying to keep it real in an unusual way: being able to
compile itself, and not omitting the practical details of features
like debug support. Since something has to go, we'll skip the whole
topic of optimization.

Our source language and implementation language---the language we
compile from, and the one we code the compiler in---are a subset of
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
    >>> import ast, dis, astpp # (You can find astpp, by Alex Leone, on the web)
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

What happened to our program? Mostly it's encoded into `co_code`: a
sequence of bytecodes in a bytestring. Disassembly renders the
meaning, using `dis.dis`:

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
(as in the Gang of Four 'composite' pattern, or a monoid if that's how
you were raised). In generating the code we'll build it in pieces to
be strung together, more like

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
`op.LOAD_CONST(0)` this example would say `op.LOAD_CONST('Monty')`,
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
learn more that way; but I'd bog down in crashes that turned out to
hinge on mysteries like just what values must go into `co_flags` under
what conditions.

To face these uncertainties and start learning, let's make a complete
working system for a tiny core subset of the problem: just enough to
run our example.

    # in bytecompile.py:
    import ast, collections, dis, types, sys
    from functools import reduce
    from itertools import chain
    from check_subset import check_conformity

    <<the assembler>>
    <<tables>>
    <<the code generator>>
    <<compile and run a file>>

    if __name__ == '__main__':
        sys.argv.pop(0)
        run(sys.argv[0], '__main__')

Here is the whole compiler as a 'literate program': a part in angle
brackets stands for more code we'll see later. When we do, we'll show
it starting with either `# in the assembler v0:` (to fill in the
assembler, but only for version 0, this 'spike' version) or just
`# in the assembler:` (for all versions).

[XXX 'v0' would be more meaningful as 'the spike', and maybe
preferable, but my literate-programming software can work with just
numbered versions right now]

`pop` removes the initial `argv[0]` to leave the command-line
arguments the same as if we'd run Python on the source program
directly: thus (with this compiler in `bytecompile.py`) you can run
`python greet.py`, or `python bytecompile.py greet.py`, or (not yet)
`python bytecompile.py bytecompile.py greet.py`...

    # in compile and run a file:
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

    <<compile to code>>

Compiling the module produces a function of no arguments; to run the
module's code, we call that function (with `()`). The logic was split
into two further functions as alternative entry points for
testing. (This chapter will leave out the automated tests.)

Why doesn't `compile_file` use a `with` statement? Because this
chapter's code will only use constructs implemented in this chapter.

Throughout the compiler, `t` (for 'tree') names an AST node. These
`t`'s appear everywhere.


### Visitors

An AST is a recursive data structure, for which we're going to write a
few recursive functions. We might code one like

    # in examples.py:
    def example(t):
        if   isinstance(t, ast.Module):
            return "(A module {})".format(' '.join(map(example, t.body)))
        elif isinstance(t, ast.Expr):
            return "(An expression {})".format(example(t.value))
        elif isinstance(t, ast.Num):
            return "(A number {})".format(t.n)
        else:
            raise Error("I don't know how to handle this kind of node", t)

but the visitor classes in Python's `ast` module make our task a
little more convenient:

    class ExampleVisitor(ast.NodeVisitor):
        def visit_Module(self, t):
            return "(A module {})".format(' '.join(map(self.visit, t.body)))
        def visit_Expr(self, t):
            return "(An expression {})".format(self.visit(t.value))
        def visit_Num(self, t):
            return "(A number {})".format(t.n)

This is called like so:

    # in transcripts:
    >>> v = ExampleVisitor()
    >>> v.visit(ast.parse('5'))
    '(A module (An expression (A number 5)))'
    >>> print(astpp.dump(ast.parse('5')))
    Module(body=[
        Expr(value=Num(n=5)),
      ])

The visitor pattern helps in two ways: it lets us define
separately-callable methods one a time rather than inserting `elif`s
into a master function, and the `ast` module defines a convenient
generic traversal which we'll see later.

Instead of as functions or visitors, we might reasonably organize our
logic as methods right in the AST classes, except we're given those
classes as written already, and we'd have to code the generic
traversal ourselves. [XXX delete this paragraph? Probably doesn't add
enough]


### From a module AST to a code object

The compiler at its core is a visitor that returns assembly code. As
it walks through the tree, it remembers the names and constants it's
seen, so that the emitted instructions can refer to names and
constants by index. Finally it assembles the assembly code into a code
object. Let's create this code-generation visitor and set it to work:

    # in compile to code v0:
    def compile_to_code(module_name, filename, t):
        return CodeGen(filename, StubScope()).compile_module(t, module_name)

    class StubScope: freevars, cellvars, derefvars = (), (), ()

(For the first time, we're seeing stub code that will be superseded
later in this chapter in a fancier version of the compiler. All along,
this code has been within waving distance of my actual first spike,
but more polished, but also distorted some by foreknowledge: keeping
the parts to be rewritten grouped in just a few definitions.)

    # in the code generator:
    class CodeGen(ast.NodeVisitor):

        def __init__(self, filename, scope):
            self.filename  = filename
            self.scope     = scope
            self.constants = make_table()
            self.names     = make_table()
            self.varnames  = make_table()

        def compile_module(self, t, name):
            assembly = self(t.body) + self.load_const(None) + op.RETURN_VALUE
            return self.make_code(assembly, name, 0)

Recall the disassembly of a module, above: there was the body code,
then `LOAD_CONST` of `None`, then `RETURN_VALUE`. We turn that
assembly into a code object:

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

This code object is littered with fields:

  * The bytecode bytes themselves, from `assemble(assembly)`.

  * Redundant info too costly to compute at runtime, like `stacksize`.

  * Metadata like `lnotab`. `lnotab` tells what line number in the source file produced what part of the bytecode. (The name's not my fault!)

  * The above-mentioned tables for constants and names.

These tables are tuples in the code object, built from `defaultdict`s
that grow as we walk the tree:

    # in tables:
    def make_table():
        table = collections.defaultdict(lambda: len(table))
        return table

    def collect(table):
        return tuple(sorted(table, key=table.get))

For `names` and `varnames` the keys are the name strings; but it gets
trickier for `constants`. Equal names get the same slot in a names
table, but 'equal' constants might not: for example, `5 == 5.0` is
true, but `5` and `5.0` are nevertheless distinct. So we key on the
type as well as the value of the constant:

    # in the code generator:
        def load_const(self, constant):
            return op.LOAD_CONST(self.constants[constant, type(constant)])

        def collect_constants(self):
            return tuple([constant for constant,_ in collect(self.constants)])

(There's a further subtlety in comparing with signed floating-point
zeroes, which we'll punt on by expelling negative zeroes in constants
from the subset of the language we're going to handle. A
`check_conformity` module, not presented in this chapter, can be
called to make sure the input program is in our subset.)


### Expressions and the stack

Our first tiny subset has nothing but assignment and expression
statements, where expressions include only names, simple constants,
and function calls. A constant expression turns into just a
`LOAD_CONST`:

    # in code generation 0:
        def visit_NameConstant(self, t): return self.load_const(t.value)
        def visit_Num(self, t):          return self.load_const(t.n)
        def visit_Str(self, t):          return self.load_const(t.s)
        visit_Bytes = visit_Str

A name also produces a single instruction, but it depends on context:
a name can appear on the left-hand side of an assignment, not just in
an expression. Python ASTs use the same node-type for both, with a
`ctx` field to distinguish them:

        def visit_Name(self, t):
            if   isinstance(t.ctx, ast.Load):  return self.load(t.id)
            elif isinstance(t.ctx, ast.Store): return self.store(t.id)
            else: assert False

    <<generate variable accesses>>

When we get to compiling functions and classes, this part will get
fancier because names will live in different scopes, but for now
they're all global:

    # in generate variable accesses v0:
        def load(self, name):  return op.LOAD_NAME(self.names[name])
        def store(self, name): return op.STORE_NAME(self.names[name])

Now for function calls. A call like `f(x, y, key=42)` compiles to

    # in examples:
      1           0 LOAD_NAME                0 (f)
                  3 LOAD_NAME                1 (x)
                  6 LOAD_NAME                2 (y)
                  9 LOAD_CONST               0 ('key')
                 12 LOAD_CONST               1 (42)
                 15 CALL_FUNCTION          258 (2 positional, 1 keyword pair)

Evidently the load instructions stash their values somewhere for
`CALL_FUNCTION` to use at the end. That somewhere is the stack: a
growing and shrinking list of values. Each load appends to it, and
each call removes the right number of values from the end and replaces
them with one value, the result of the call. This scheme gives a
more-complex expression like `f(g(1), h())` a place for the
intermediate results to live:

    # in examples:
                                                          (the stack afterwards)
      1           0 LOAD_NAME                0 (f)        [f]
                  3 LOAD_NAME                1 (g)        [f, g]
                  6 LOAD_CONST               0 (1)        [f, g, 1]
                  9 CALL_FUNCTION            1            [f, g(1)]
                 12 LOAD_NAME                2 (h)        [f, g(1), h]
                 15 CALL_FUNCTION            0            [f, g(1), h()]
                 18 CALL_FUNCTION            2            [f(g(1), h())]

The assembly code for the full, compound call is a concatenation of
the assembly for its parts: if you compiled just `g(1)` or `h()`,
you'd get some of the same code above (except perhaps for the indices
assigned to the names `g` and `h`).

(In the virtual machine the stack is not implemented by an actual
Python list object, but by a low-level array of words in memory. It's
conventionally vertical instead of horizontal: a stack with a top we
'push' onto and 'pop' from, changing its 'depth'.)

    # in code generation 0:
        def visit_Call(self, t):
            assert len(t.args) < 256 and len(t.keywords) < 256
            return (self(t.func) + self(t.args) + self(t.keywords)
                    + op.CALL_FUNCTION((len(t.keywords) << 8) | len(t.args)))

        def visit_keyword(self, t):
            return self.load_const(t.arg) + self(t.value)

We recursively generate code for the function, the arguments, and the
keyword arguments (which in turn are built from the keyword name
`t.arg` and the value expression), then chain them together with
`CALL_FUNCTION` at the end.

As a technicality, the `CALL_FUNCTION` instruction's argument is a
two-byte integer (like all instruction arguments, when present) which
has to encode two individual bytes: the counts of keyword and
positional arguments. Python's bytecode format does give a way to
encode bigger numbers, but we punt on them. (Why not put the assertion
in `check_conformity`? That wouldn't be terrible, but it'd be checking
against seemingly arbitrary numbers in the midst of other code that
only checks against a tree grammar. `check_conformity` enforces the
*language subset* we claim to implement; here we enforce an
implementation limit, at the point where the reason is
clear. `check_conformity` does screen out fancier forms of calls like
`f(*args, **kwargs)`. From here on I'll pass over such subsetting
without comment.)

The recursive visits like `self(t.func)` and `self(t.args)` (and
`self(t.body)` back in `compile_module`) call this:

    # in the code generator:
        def __call__(self, t):
            if isinstance(t, list): return concat(map(self, t)) 
            assembly = self.visit(t)
            return SetLineNo(t.lineno) + assembly if hasattr(t, 'lineno') else assembly

It takes an AST node or a list of such nodes; when it's a list we get
back all the results concatenated. (Python's AST and bytecodes were
designed to make that simple chaining the right thing to do in most
cases, as we just saw for compiling the arguments of a call.) We
could've defined separate methods for visiting a list vs. a node, but
conflating them turns out to unify a bit of code to come.

Once we visit a node, producing its assembly code, we can annotate the
assembly with the node's source-line number. This spares us from
writing the same in every `visit_Whatever` method.

The method gets the magical `__call__` name for concision: we're going
to use it *all over*.

        def generic_visit(self, t):
            assert False, t

We've been using the default, superclass implementation of the `visit`
method: it looks at the type of the node visited and calls the right
`visit_Whatever` method. But if there is no such method,
`generic_visit` gets called. Since `check_conformity` will make sure
we never see an unexpected node type, that "can't happen"---but
during development, of course, sometimes it did. Before I overrode
`generic_visit`, the default implementation would succeed silently,
making my mistakes harder to see.

    # XXX I think we can get rid of these
    # 'noise' lines; come back to this.
    <<code generation 0>>
    <<code generation 1>>
    <<code generation 2>>


### Statements

Executing a statement should leave the stack unchanged. For an
expression statement (consisting of just an expression, typically a
call), we evaluate the expression and then remove its result from the
stack:

    # in code generation 0:
        def visit_Expr(self, t):
            return self(t.value) + op.POP_TOP

For an assignment, we again evaluate the expression, but then store it
in the target; all the store instructions also pop the stack.

        def visit_Assign(self, t):
            def compose(left, right): return op.DUP_TOP + left + right
            return self(t.value) + reduce(compose, map(self, t.targets))

The complication here deals with multiple assignments like `x = y =
42`: there's a *list* of targets (`x` and `y`). If we followed the
value code with two store instructions, the second would be stuck with
nothing to store, because the first popped it off; so before the first
one, we insert a `DUP_TOP` instruction, which will push an extra
reference to the value.

(We've already covered generating the store instructions, because the
targets are `Name` nodes, as we saw in `visit_Name`.)


### XXX can't we skip this?:

The literate-programming chunks <<code generation 1>> and <<code
generation 2>> are empty until later versions of the compiler:

    # in code generation 1:

and

    # in code generation 2:


### Just enough assembly

We still need to create assembly code---instructions, `SetLineNo`
pseudo-instructions, and concatenations---and to compute three
functions of it: the maximum stack depth, the line-number table, and
the encoded bytecode. The most direct and minimal stub represents an
assembly instruction as its final bytecode sequence, makes the
line-number table empty, and pretends the stack depth is 10---don't
try it with too-complex nested calls.

    # in assembly types and functions v0:
    def Instruction(opcode, arg):
        return bytes([opcode] if arg is None else [opcode, arg % 256, arg // 256])

    def concat(assemblies): return b''.join(assemblies)

    def SetLineNo(lineno): return b''

    def make_lnotab(assembly): return 1, b''

    def plumb_depths(assembly): return 10

    def assemble(assembly): return assembly

We fill in `op` so that `op.LOAD_NAME` and all the rest work.

    # in the assembler:
    <<assembly types and functions>>

    def denotation(opcode):
        if opcode < dis.HAVE_ARGUMENT:
            return Instruction(opcode, None)
        else:
            return lambda arg: Instruction(opcode, arg)

    op = type('op', (), dict([(name, denotation(opcode))
                              for name, opcode in dis.opmap.items()]))

And now it'll compile `greet.py`, the example we started with. Hurray!

    # in transcripts:
    $ python3 bytecompile0.py greet.py 
    Hello, Monty


## Fleshing it out

As we fill out this compiler with more visit methods for more AST node
types, we'll hit a new problem compiling control-flow constructs like
`if`-`else`. They reduce to jumping around in the bytecode. The
expression statement

    # in example.py:
    yes if ok else no

becomes

    # in examples:
      1           0 LOAD_NAME                0 (ok)
                  3 POP_JUMP_IF_FALSE       12
                  6 LOAD_NAME                1 (yes)
                  9 JUMP_FORWARD             3 (to 15)
            >>   12 LOAD_NAME                2 (no)
            >>   15 POP_TOP

where `POP_JUMP_IF_FALSE` does what it says: pops the value left by
`ok`, tests it, and if it's false jumps to index 12. Otherwise
execution falls through to the usual next instruction, at 6.
`JUMP_FORWARD` likewise jumps to index 15, to skip `no` if we chose
`yes`.

([XXX Footnote, I guess:] `POP_TOP` is not part of the code for the `if`
expression itself, it's code for the expression statement containing
the `if`. It's listed here because the `JUMP_FORWARD` jumps to
it. Every `if` appears in a context where more bytecode will follow --
a `RETURN_VALUE` instruction if nothing else.)

The new problem: during code generation the compiler may not yet know
the bytecode index where a jump target will be. Our answer: it invents
a label for each target, emits that instead, and leaves it to
`assemble` to resolve all the labels. 

(This design is not inevitable. Notice that the `JUMP_FORWARD`
argument is encoded here as 3, when the target is index 15: 3 means
the distance, taken from the instruction right after the jump (at
index 12), to the target. Since for `if`-`else` the target always
follows the jump, if we emitted code in last-to-first order we'd know
the target's distance at the time we emit the jump instruction. If all
jump instructions were encoded this way, we could always encode the
targets as we go along: just as the bytecode for a compound function
call is the concatenation of the bytecode for its parts, so we'd make
the bytecode for a compound statement by joining that of its parts.
Sadly for this dream, `POP_JUMP_IF_FALSE` here encodes its target as
the absolute address 12. I don't know why the instructions take
different encodings. I considered handling this with a design more
like a 'linker' than an assembler, but decided to stick closer to
CPython's design: it's less likely to get borked by their changes.)

    # in code generation 1 v1:
        def visit_If(self, t):
            orelse, after = Label(), Label()
            return (           self(t.test) + op.POP_JUMP_IF_FALSE(orelse)
                             + self(t.body) + op.JUMP_FORWARD(after)
                    + orelse + self(t.orelse)
                    + after)

        visit_IfExp = visit_If

(The visit method for if-expressions and if-statements is the same
because `t.body` is an expression node when `t` is an if-expression,
or a list of statement nodes when `t` is an if-statement, and `self()`
can take either.)


### Assembly

Let's make labels work, and fully implement the rest of the assembler:
computing stack depths and line-number tables. 

    # in assembly types and functions v1:
    def assemble(assembly):
        return bytes(iter(assembly.encode(0, dict(assembly.resolve(0)))))

Assembly code is now some kind of object we'll define (several kinds,
for instructions, labels, `SetLineNo`, and a couple more); to turn it
into bytecode, we first *resolve* the addresses of any labels it has
(starting at address 0), then *encode* it into bytes, using the
addresses now known: a 'two-pass assembler'.

    def plumb_depths(assembly):
        depths = [0]
        assembly.plumb(depths)
        return max(depths)

For the max stack depth, we ask `plumb` to compute the depth after
every instruction, appending them all to `depths`. (This uses more
space than needed, but turns out to be simple.)

Computing the line-number table calls on a `line_nos` method yielding
pairs of (bytecode address, source-code line-number). `make_lnotab`
consumes them and encodes them into a bytestring in a format imposed
by the VM interpreter. Each successive pair of encoded bytes
represents an (address, line_number) pair as the differences from the
previous pair. Since a byte is between 0 and 255, there are two
problems:

* The difference could be negative. This can happen [XXX example]
    
* The difference could exceed 255. In this case the entry must take up
  multiple successive byte-pairs, first increasing only the address
  part in each, and then increasing the line number. [XXX say
  something---a tiny bit?---about why]

[XXX stupid markdown formatting line, ignore me]

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

And `concat` can't be just `b''.join` anymore:

    def concat(assemblies):
        return sum(assemblies, no_op)

Assembly objects will need an `__add__` method for all the code we've
been seeing like `assembly + op.POP_TOP`. Python's `sum()` calls the
same method.


### Assembly types

The simplest assembly fragment is the no-op:

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

For `resolve`'s use, all our assembly objects hold a `length` counting
how many bytes of bytecode they'll become. In this compiler the
lengths are constant, since we don't support the extended-length
argument format. (Suppose there were a relative jump to an address
over 65535 bytes away. The jump instruction would need to occupy more
than the usual 3 bytes; if we'd assumed 3 bytes, this would imply
cascading changes.) [XXX can you in fact have extended jumps?]

A `Label` is just like `no_op`, except resolving to an address.

    class Label(Assembly):
        def resolve(self, start):
            return ((self, start),)

So is a `SetLineNo` except for adding to the line-number table.

    class SetLineNo(Assembly):
        def __init__(self, line):
            self.line = line
        def line_nos(self, start):
            return ((start, self.line),)

There are four kinds of bytecode instruction: absolute jumps, relative
jumps, and non-jumps with or without an argument. ('Jump' for our
purposes means any instruction taking an address argument.)

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

The depth of the stack after the instruction depends on the depth
before it: for example, a `LOAD_CONST` increases it by one. This logic
disregards jumps: after an unconditional jump, the next instruction,
which must be the target of some *other* jump, should get its stack
depth from there instead. In this compiler, though, it happens that
the straight-line stack depths are always right. (CPython's compiler,
less lucky or less trusting, has to trace through the jumps
propagating depths along every path, making sure they're consistent.)

The `dis` module supplies a `stack_effect` which *almost* does our
job: we want the effect as seen by the instruction following, but for
two types of jumps it perversely gives us the effect at the *jump
target*. For general use I'd design `stack_effect` to return a list of
the possible effects, usually of length 1; but we can just wrap their
function to patch it for this case:

    or_pop_ops = (dis.opmap['JUMP_IF_TRUE_OR_POP'],
                  dis.opmap['JUMP_IF_FALSE_OR_POP'])

    def stack_effect(opcode, oparg):
        if opcode in or_pop_ops:
            return -1
        else:
            if isinstance(oparg, Label): oparg = 0
            return dis.stack_effect(opcode, oparg)

(The resolved target of a label argument won't affect the result, for
the ops we use. `dis.stack_effect` requires `oparg` to be `None` or
an `int`.)

A `Chain` catenates two assembly-code fragments in sequence. It uses
`itertools.chain` to catenate the label resolutions, bytecodes, and
`lnotab` entries produced in the different methods.

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


### More code generation: expressions

Dict literals like `{'a': 'x', 'b': 'y'}` turn into code like

    # in examples:
      1           0 BUILD_MAP                2
                  3 LOAD_CONST               0 ('x')
                  6 LOAD_CONST               1 ('a')
                  9 STORE_MAP           
                 10 LOAD_CONST               2 ('y')
                 13 LOAD_CONST               3 ('b')
                 16 STORE_MAP           

so:

    # in code generation 1 v1:
        def visit_Dict(self, t):
            return (op.BUILD_MAP(min(0xFFFF, len(t.keys)))
                    + concat([self(v) + self(k) + op.STORE_MAP
                              for k, v in zip(t.keys, t.values)]))

The argument to `BUILD_MAP` gives the runtime a hint of the dict's
size. It's allowed to be wrong, since dicts can grow and shrink---but
not to overflow the two bytes allotted to an argument in
bytecode. Thus the `min`. ([XXX I think this digression should go,
too:] A dict literal big enough to get clipped by this `min` would
likely use so many constants that references into the constants table
would overflow *their* two-byte encoding. Python bytecode does have a
means for extended-size arguments, which again I didn't implement;
you'll get a runtime error during compiling instead. So why bother
with the `min`? Because we may fix the extended-size encoding issue
later, and it's easier to make `BUILD_MAP` correct now than to
document the problem for later.)

A simple subscript expression, `a[x]`, becomes

    # in examples:
      1           0 LOAD_NAME                0 (a)
                  3 LOAD_NAME                1 (x)
                  6 BINARY_SUBSCR

Similarly, `a.x` becomes

    # in examples:
      1           0 LOAD_NAME                0 (a)
                  3 LOAD_ATTR                1 (x)

Like name nodes, subscript nodes can appear on the left-hand side of
an assignment statement, like `a.x = 42`. They carry a `t.ctx` field
just like a name's.

    # in code generation 1 v1:
        def visit_Subscript(self, t):
            return self(t.value) + self(t.slice.value) + self.subscr_ops[type(t.ctx)]
        subscr_ops = {ast.Load: op.BINARY_SUBSCR, ast.Store: op.STORE_SUBSCR}

        def visit_Attribute(self, t):
            sub_op = self.attr_ops[type(t.ctx)]
            return self(t.value) + sub_op(self.names[t.attr])
        attr_ops = {ast.Load: op.LOAD_ATTR, ast.Store: op.STORE_ATTR}

A list or tuple can also appear in both load and store contexts. As a
load, `['a', 'b']` becomes

    # in examples:
      1           0 LOAD_CONST               0 ('a')
                  3 LOAD_CONST               1 ('b')
                  6 BUILD_LIST               2

where 2 is the length of the list. Unlike with `BUILD_MAP` the length
must be exact.

    # in code generation 1 v1:
        def visit_List(self, t):  return self.visit_sequence(t, op.BUILD_LIST)
        def visit_Tuple(self, t): return self.visit_sequence(t, op.BUILD_TUPLE)

        def visit_sequence(self, t, build_op):
            if   isinstance(t.ctx, ast.Load):
                return self(t.elts) + build_op(len(t.elts))
            elif isinstance(t.ctx, ast.Store):
                return op.UNPACK_SEQUENCE(len(t.elts)) + self(t.elts)
            else:
                assert False

A unary operator works just like a function call except it gets its
own instruction, for efficiency. `not -x` gets compiled to

    # in examples:
      1           0 LOAD_NAME                0 (x) 
                  3 UNARY_NEGATIVE       
                  4 UNARY_NOT            

so:

    # in code generation 1 v1:
        def visit_UnaryOp(self, t):
            return self(t.operand) + self.ops1[type(t.op)]
        ops1 = {ast.UAdd: op.UNARY_POSITIVE,  ast.Invert: op.UNARY_INVERT,
                ast.USub: op.UNARY_NEGATIVE,  ast.Not:    op.UNARY_NOT}

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

    # in examples:
      1           0 LOAD_NAME                0 (x) 
                  3 LOAD_CONST               0 (1) 
                  6 BINARY_SUBTRACT

Comparisons, like `x<2`, take a slightly different form in the AST,
because Python treats a chain of comparisons specially: `1<x<2` is a
single expression as a single AST node holding a list of operators and
a list of their right operands. Our subset of Python doesn't cover
this case, only binary comparisons like `x<2`.

    # in code generation 1 v1:
        def visit_Compare(self, t):
            [operator], [right] = t.ops, t.comparators
            cmp_index = dis.cmp_op.index(self.ops_cmp[type(operator)])
            return self(t.left) + self(right) + op.COMPARE_OP(cmp_index)
        ops_cmp = {ast.Eq: '==', ast.NotEq: '!=', ast.Is: 'is', ast.IsNot: 'is not',
                   ast.Lt: '<',  ast.LtE:   '<=', ast.In: 'in', ast.NotIn: 'not in',
                   ast.Gt: '>',  ast.GtE:   '>='}

`and` and `or` expressions point out a new concern: keeping the stack
consistent across different branches of control. `print(x or y)` compiles to

    # in examples:
      1           0 LOAD_NAME                0 (print)
                  3 LOAD_NAME                1 (x)
                  6 JUMP_IF_TRUE_OR_POP     12
                  9 LOAD_NAME                2 (y)
            >>   12 CALL_FUNCTION            1 (1 positional, 0 keyword pair)

`JUMP_IF_TRUE_OR_POP` only pops the tested value if it came out
falsey. At index 12, whichever way execution got there, the stack has
the same height: two entries.

    # in code generation 1 v1:
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


### More statements

        def visit_Pass(self, t):
            return no_op

        def visit_Raise(self, t):
            return self(t.exc) + op.RAISE_VARARGS(1)

We handle only the most common form of `raise`: `raise foo`.

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
calls and assignments, in the desugar pass we'll see soon; but that
would take about as much compiler code, to produce worse compiled
code.

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
won't explain what `SETUP_LOOP` and friends do at runtime.)

At this point we have a runnable program again, that can compile
nontrivial computations. We could flesh it out further with more node
types---`break` and `continue`, for example. But the biggest gain in
usefulness---compiling functions and classes---requires bigger
changes. With them implemented, we'll be able to compile this
compiler.


## Functions and classes

Our finished compiler will need more passes:

* Given an AST, we first 'desugar' it, replacing some of the nodes
  with equivalent code in terms of simpler node types. (The language
  minus certain features is less sweet but equally nutritious.)
  CPython doesn't have this pass, though some have suggested it
  should: optimizations would be easier to express as AST rewrites
  than bytecode rewrites. (For example: rewrite `2+3` to `5`.) I
  don't do optimizations, but wrote it this way because some node
  types are a little simpler and substantially more readable to
  compile to other node types than to bytecode.

* We complain if the AST departs from our subset of Python. CPython
  lacks this pass, of course, and this chapter won't examine it. It's
  valuable in two ways: documenting what we claim to compile
  correctly, and keeping the user/developer from wasting time on
  apparent bugs on input it was never meant to
  support. [XXX add a call-out box with the ASDL grammar of Python ASTs, with our subset's omissions italicized or something]

* Then we analyze the scope of variables: their definitions and uses
  in classes, functions, and function-like scopes such as lambda
  expressions. Python's built-in `symtable` module can do this, but
  we can't use it! It requires a source-code string instead of an
  AST, and ASTs don't come with a method to give us back source
  code. In the early development of this compiler I used `symtable`
  anyway, as scaffolding; the compiler then also had to take source
  code instead of an AST as input. (I would've wanted to write my
  own scope analyzer anyway, to make the compiler self-contained.)

* With this info in hand we generate bytecode as before.

[XXX stupid markdown formatting line, ignore me]

    # in compile to code v2:
    def compile_to_code(module_name, filename, t):
        t = desugar(t)
        check_conformity(t)
        return CodeGen(filename, top_scope(t)).compile_module(t, module_name)


### Desugaring

    def desugar(t):
        return ast.fix_missing_locations(Desugarer().visit(t))

Python's `ast` module defines another visitor class, for *transforming*
trees. It expects each visit method to return an AST node, which
will be taken to replace the node that was the argument. The default
behavior, if the sub-nodes weren't changed, returns the same node
unchanged. Desugaring uses such a transformer:

    class Desugarer(ast.NodeTransformer):

For a start, we rewrite statements like `assert cookie, "Want
cookie!"` into `if not cookie: raise AssertionError("Want cookie!")`.
(Rather, into an if-then-else with the `raise` in the `else`
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
calls whatever `AssertionError` is bound to.

What was the point of this visitor? Without it, we'd need to define a
`visit_Assert` in the code generator instead---OK, fine. This would
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

(The last lines, implementing function decorators, could be left
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

For instance, `[n for n in numbers if n]` becomes a call to a
locally-defined function having the following body, except that
`result` actually gets the name `.result`, which can't occur in real Python
source code and thus can't clash with variables from the source:

    # in example.py:
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

`Desugarer` uses these helpers:

    # in compile to code v2:
    class Function(ast.FunctionDef):
        _fields = ('name', 'args', 'body')

    load, store = ast.Load(), ast.Store()

(`Function` derives from `FunctionDef` for the sake of
`ast.get_docstring` on it.)


### Analyzing scopes 

Scope analysis decides the runtime representation of variables:
'fast', 'deref', or neither. Consider:

    # in example.py:
    def fn():
        f = "I'm a fast one."
        d = "I'm a deref."
        return g, f, d, lambda: d

`f` is a local variable used only locally within `fn`. It's 'fast': it
gets a numbered slot among the locals. Once our `scope` determines
this, our revised code generator can emit `LOAD_FAST` or `STORE_FAST`
to access the variable at that known slot:

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

        def cell_index(self, name):
            return self.scope.derefvars.index(name)

The 'deref' case is for variables like `d`, which is not global, yet
'less local' than `f` in that it's used from another scope, the lambda
expression's. `d` gets a numbered slot among `fn`'s locals as well,
but at runtime its slot will not simply hold `d`'s value: instead it
holds a *cell* holding the value. It's roughly as if we'd written

    # in example.py:
    def fn():
        f = "I'm a fast one."
        d = Cell("I'm a deref.")
        return g, f, d.cell_contents, lambda d=d: d.cell_contents

    class Cell:
        def __init__(self, value): self.cell_contents = value

after which every variable is local or global, never nonlocal. Now
with both `fn` and the lambda expression sharing a reference to `d`'s
cell, any *assignment* to `d` will change the value they both see.

Back in our example, `g` has no local definition, and in fact no
global one we can see either: `g` is neither fast nor
deref. (CPython's compiler will generate different instructions for
variables known to be global -- instructions like `LOAD_GLOBAL`---but
our compiler won't. This should be an easy improvement to make.)

Unlike a function scope, a class scope doesn't get fast or deref
variables---only its function-type subscopes do, such as its method
definitions. Nested classes are forbidden in this compiler, to avoid
some of Python's dark corners. So are `del` statements and explicit
`nonlocal` and `global` declarations.

Analyzing the scopes in a module takes two passes:

    # in compile to code v2:
    def top_scope(t):
        top = Scope(t, ())
        top.visit(t)
        top.analyze(set())
        return top

The first creates subscopes for classes and functions and records the
*local* uses and definitions of variables. We must walk the AST to
collect this information, another job for an AST visitor. Unlike the
`NodeTransformer` last time, we'll leave the AST alone: our visit
methods should instead fill out attributes of the `Scope`. That's the
protocol for a `NodeVisitor`:

    class Scope(ast.NodeVisitor):
        def __init__(self, t, defs):
            self.t = t
            self.children = []       # Enclosed sub-scopes
            self.defs = set(defs)    # Variables defined
            self.uses = set()        # Variables referenced

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

The `analyze` pass can then deduce the *non*local uses of
variables. It walks down accumulating the definitions from enclosing
scopes, then back up accumulating the references from enclosed ones:

        def analyze(self, parent_defs):
            self.local_defs = self.defs if isinstance(self.t, Function) else set()
            for child in self.children:
                child.analyze(parent_defs | self.local_defs)
            child_uses = set([var for child in self.children for var in child.freevars])
            uses = self.uses | child_uses
            self.cellvars = tuple(child_uses & self.local_defs)
            self.freevars = tuple(parent_defs & (uses - self.local_defs))
            self.derefvars = self.cellvars + self.freevars

A 'cell variable' is a deref defined in the current scope; a 'free
variable' must be taken from the enclosing scope instead. (In our
example, `d` is a free variable in the lambda expression but a cell
variable in the rest of `fn`.)

The code generator uses `cellvars` and `freevars`, plus these methods:

        def access(self, name):
            return ('deref' if name in self.derefvars else
                    'fast'  if name in self.local_defs else
                    'name')

        def get_child(self, t):
            for child in self.children:
                if child.t is t:
                    return child
            assert False

Does scope analysis need to precede code generation? If we turned it
around, first the code generator would generate symbolic assembly with
named variables, then we'd analyze the uses of variables in *that*,
and finally assemble bytecode, treating some of the `LOAD_NAME`s as
`LOAD_FAST`, etc. This could be more useful as part of a bytecode
rewriting system---disassembling, transforming, and reassembling, it'd
be nice to transform without worrying about the mechanics of cell and
free variables---and it might take roughly the same amount of
code. Being different from CPython's approach, it's riskier.


### Back to code generation

    # in code generation 2 v2:
        def visit_Return(self, t):
            return ((self(t.value) if t.value else self.load_const(None))
                    + op.RETURN_VALUE)

We handle both `return` and `return foo`. (The no-argument `return` is
never used by this compiler.)

A `Function` node is an expression desugared from a lambda expression,
function definition, or list comprehension. Compiling one takes two
steps: first, compile the whole nested scope into a separate code
object. Second, generate assembly code that will build a function
object out of this code object. [XXX disassembled example needed]

        def visit_Function(self, t):
            code = self.sprout(t).compile_function(t)
            return self.make_closure(code, t.name)

The new `CodeGen` for this new code object requires a subscope of the
current scope---previously computed by the scope analyzer, recovered
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
[TODO review the docs/source on LOAD_CLOSURE---IIRC it loads the
*mutable cell holding the variable's value*, instead of the value, so
that the nested function can mutate the variable.]

Back to compiling a function down to a code object: on a new `CodeGen`
instance, we called `compile_function`:

        def compile_function(self, t):
            self.load_const(ast.get_docstring(t))
            for arg in t.args.args:
                self.varnames[arg.arg]
            assembly = self(t.body) + self.load_const(None) + op.RETURN_VALUE
            return self.make_code(assembly, t.name, len(t.args.args))

A function's docstring is to go in the code object's constants table
as the first entry---`None` if no docstring. (That's where CPython
looks for it.) Our logic relies on this `load_const` happening first,
before any code generation, and on our table preserving the order we
add to it. (The actual `LOAD_CONST` instruction is discarded, here.)

As with the docstring, the parameter names become the first elements
of the `varnames` table. (Remember they're `defaultdict`s: fetching
adds to them as necessary.)

We generate assembly that will run the function's body and return
`None` (in case the body had no `return` of its own); then we assemble
it all into a code object.

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
*doesn't* start the constants table, for these.) Python's class
builder (`LOAD_BUILD_CLASS`) will populate the new class's attributes
from the locals dictionary as of the point this function returns. This
is why scope analysis must not classify the locals of a class
definition as 'fast'.

        def compile_class(self, t):
            docstring = ast.get_docstring(t)
            assembly = (  self.load('__name__')      + self.store('__module__')
                        + self.load_const(t.name)    + self.store('__qualname__') # XXX need to mess with t.name?
                        + self.load_const(docstring) + self.store('__doc__')
                        + self(t.body) + self.load_const(None) + op.RETURN_VALUE)
            return self.make_code(assembly, t.name, 0)

I was a little tempted to leave classes out. Python stopped needing
them once it gained proper nested functions---in terms of expressive
power, if not familiarity and legacy. As it worked out, most of the
simplicity we'd gain by chopping out `class` we can get by forbidding
nested classes (nested in a `class` or a `def`).

OK, so! Wind it all up and watch the tail-eating:

    # in transcripts:
    $ python3 bytecompile2.py bytecompile2.py bytecompile2.py greet.py 
    Hello, Monty


## But why compile?

We've taken considerable trouble to convert from one fairly-arbitrary
representation to another. We've cost ourselves not just the work and
complexity of translating, but also translating *back*: debuggers and
profilers must map what happens in bytecode to terms meaningful in the
source. Why not interpret programs directly in the first form, the
AST?

First, for the compact linear form of bytecode. An AST is fatter and
distributed in memory, interlinked by pointers; the size and the
pointer-chasing both would slow an interpreter down. One core job,
then, was mere rearrangement: taking a data structure (the AST) meant
for arbitrary viewing and changing, and laying it out just right for
the interpreter, who'll find each element ready to hand at the moment
it's needed---like, for us, reading a recipe and starting by laying
the ingredients and pans onto the counter in a sensible order.

Second, to precompute. We analyzed the scopes and how they used
variables, for the sake of finding, ahead of time, the place in the
runtime environment where a variable will live---letting the
interpreter skip looking up the name.

There's a third potential win, in rewriting the program as we compile
it---'optimization'. Perhaps the compiler could notice that `[i*2 for
i in range(10)]` would go faster as `list(range(0, 20, 2))`. This is
precomputation in a broader, open-ended sense (sometimes called the
Full Employment Theorem for Compiler Writers). But isn't it orthogonal
to translating source code to binary? Aren't there independent source-
and machine-code optimizers? Yes, but: dealing in source code only,
many machine operations can't be seen, making the choice of how
they're to be done inexpressible (or only by some extra convention);
then, over in machine code, the reasons and constraints behind the
choices are erased, sticking the optimizer with a sometimes-impossible
job of reinferring them. A compiler lives on the happy peak between,
both sources of power exposed.

Well, that sounds compelling. Maybe. But CPython doesn't optimize, to
first order. (PyPy's another story.) What if we ran the scope analysis
and performed a *generic* kind of rearrangement: that is,
re-represented the AST in a tight linear form, with the debug info
pushed off to the side? The code generator could look vaguely like

    # in bluesky.py:
    def visit_If(self, t):
        test, body, orelse = self(t.test), self(t.body), self(t.orelse)
        return [compact_ast.IF, len(test), len(body)] + test + body + orelse

(But generic: such stereotyped logic could be data-driven.) With this
'compact AST' you'd point to an AST node's representation via a
numeric offset into an array such as this method returns: so the
`t.test` passed in here becomes a subarray starting at index `3`,
`t.body` then follows starting from `3+array[1]`, and so on. This form
could be nearly as tight as bytecode (once we use bytes and not the
general integers which were quicker to explain), while still viewable
as just another form of AST, making the compiler and surrounding tools
all simpler. In numbers, how much better is the bytecode virtual
machine?

[XXX can we give a rough number?]

Exploring that question exceeds my scope here---but maybe not yours.


## Continuations

Where next? It could be fun to grow this to take in the code for a
CPython VM subset, like the one in this book (and reciprocally, till
they eat each other). I hope they needn't balloon too much. Add the
parser, and a life like Robinson Crusoe's starts to look attainable,
if still not quite to be envied.

An optimizer's yet unwritten. I can imagine one serving to prototype a
successor for CPython's peephole optimizer, someday. And how fast can
we compile? Faster than I did, that can't be hard.

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
included just for fun. For the compiler that's normally run, see
`compile.c` and `symtable.c`; there's also the optimizer `peephole.c`.

For much tighter tail-swallowing, try for a start John McCarthy's
classic one-page self-interpreter, in "A Micro-Manual for LISP---Not
the Whole Truth".
