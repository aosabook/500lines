"""
Test-drive the byte-compiler.
"""

import ast, dis, sys, types
import codegen

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
    f = codegen.byte_compile(module_name, filename, source, globals())
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
        run("XXX", filename, open(filename).read())
    else:
        assert False
