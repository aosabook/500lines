"""
Expand out syntactic sugar. Defines new AST subtypes.
"""

import ast

def desugar(t):
    return ast.fix_missing_locations(Expander().visit(t))

class Function(ast.FunctionDef): # from FunctionDef so that ast.get_docstring works. Ugh!
    _fields = ('name', 'args', 'body')

load, store = ast.Load(), ast.Store()

class Expander(ast.NodeTransformer):

    def visit_Lambda(self, t):
        t = self.generic_visit(t)
        # TODO: prefix parent-scope name?
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

if __name__ == '__main__':
    import sys

    filename = sys.argv[1]
    source = open(filename).read()
    t = ast.parse(source)
    print(ast.dump(desugar(t)))
