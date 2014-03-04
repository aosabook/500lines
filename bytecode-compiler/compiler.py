"""
Test-drive the byte-compiler.
"""

import ast, sys, types
import codegen

loud = 0

if loud:
    report = print
else:
    def report(*args, **kwargs): pass

def main(source, filename=None):
    f = compile_toplevel(source)
    f()   # It's alive!

def compile_toplevel(source):
    t = ast.parse(source)
    try:
        import astpp
    except ImportError:
        astpp = ast
    report(astpp.dump(t))
    f = codegen.byte_compile(source, globals())
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

    eg = """
#import math
print(['m', 'n'][0])
(pow, len)
{'a': 42, 'b': 55}
{}
None
ga = 2+3
def f(a):
    "doc comment"
    while a and True:
        pass
        if False or a != 1 or False:
            print(a, 137)
        a = a - 1
    return pow(2, 16)
    return
print(f(ga))
t = True
while t:
    break
for i in range(3):
    print(i)
#print(-math.sqrt(2))
raise Exception('hi')
"""
    if len(sys.argv) == 1:
        main(eg)
    elif len(sys.argv) >= 2:
        del sys.argv[0]
        filename = sys.argv[0]
        main(open(filename).read(), filename)
    else:
        assert False
