"""
Test-drive the byte-compiler.
(You can do it directly with python compiler.py foo.py [arguments...],
but this way there's some debug scaffolding.)
"""

import ast, dis, sys, types
import compiler

loud = 0

if loud:
    report = print
else:
    def report(*args, **kwargs): pass

def run(module_name, filename, source):
    f = compile_toplevel(module_name, filename, source)
    f()   # It's alive!

def compile_toplevel(module_name, filename, source):
    t = ast.parse(source)
    try:
        import astpp
    except ImportError:
        astpp = ast
    report(astpp.dump(t))
    their_globals = compiler.make_globals(module_name)
    f = compiler.byte_compile(module_name, filename, t, their_globals)
    diss(f.__code__)
    return f

def diss(code):
    codepp(code)
    if loud: dis.dis(code)
    for c in code.co_consts:
        if isinstance(c, types.CodeType):
            report()
            report('------', c, '------')
            diss(c)

def codepp(code):
    for k in dir(code):
        if k.startswith('co_'):
            report(k, getattr(code, k))


if __name__ == '__main__':
    if len(sys.argv) >= 2:
        del sys.argv[0]
        filename = sys.argv[0]
        with open(filename) as f:
            source = f.read()
        run("XXX", filename, source)
    else:
        assert False
