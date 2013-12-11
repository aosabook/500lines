"""
Byte-compile trivial programs like "print(2+3)".
"""

import ast
from collections import defaultdict
from dis import dis, opmap 

def make_table():
    table = defaultdict(lambda: len(table))
    return table

def collect(table):
    return tuple(sorted(table, key=table.get))

class Struct:
    def __init__(self, **kwargs):
        self.__dict__.update(kwargs)
opc = Struct(**opmap)   # in Py3.4, opc = Enum('opc', opmap)

def dummy(): pass
Function = type(dummy)
Code = type(dummy.__code__)

def encode(u):
    return (u % 256, u // 256)

class CodeGen(ast.NodeVisitor):

    def __init__(self):
        self.constants = make_table()
        self.names     = make_table()
        self.varnames  = make_table()

    def compile(self, node):
        bytecode = self.visit(node)
        bytecode = self.sequence([bytecode,
                                  ((opc.LOAD_CONST,) + encode(self.constants[None])
                                   + (opc.RETURN_VALUE,))])
        argcount = 0
        kwonlyargcount = 0
        nlocals = 0
        stacksize = 10          # XXX
        flags = 67
        filename = '<stdin>'
        name = 'the_name'
        firstlineno = 1
        lnotab = b''
        return Code(argcount, kwonlyargcount, nlocals, stacksize, flags,
                    bytes(bytecode),
                    collect(self.constants),
                    collect(self.names),
                    collect(self.varnames),
                    filename, name, firstlineno, lnotab,
                    freevars=(), cellvars=())

    def sequence(self, codes):  # TODO: use join method?
        result = ()
        for code in codes:
            if result: result += (opc.POP_TOP,)
            result += code
        return result

    def visits(self, nodes):
        return sum(map(self.visit, nodes), ())

    def visit_Num(self, node):
        return (opc.LOAD_CONST,) + encode(self.constants[node.n])

    def visit_Name(self, node):
        return (opc.LOAD_GLOBAL,) + encode(self.names[node.id])  # XXX LOAD_NAME in general

    def visit_BinOp(self, node):
        binops = {ast.Add: opc.BINARY_ADD}
        return self.visit(node.left) + self.visit(node.right) + (binops[type(node.op)],)

    def visit_Call(self, node):
        return (self.visit(node.func) + self.visits(node.args) + (opc.CALL_FUNCTION,) + encode(len(node.args)))

    def visit_Expr(self, node):
        return self.visit(node.value)

    def visit_Module(self, node):
        return self.sequence(map(self.visit, node.body))

ast5 = ast.parse("""
print(2+3, 137)
print(pow(2, 16))
""")
code5 = CodeGen().compile(ast5)
dis(code5)

f = Function(code5, globals())
f()   # It's alive!
