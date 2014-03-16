"""
Expand out syntactic sugar.
"""

import ast

def desugar(t):
    t = Expander().visit(t)
    ast.fix_missing_locations(t)
    return t

class Expander(ast.NodeTransformer):
    def visit_Assert(self, t):
        result = ast.If(ast.UnaryOp(ast.Not(), t.test),
                        [ast.Raise(ast.Call(ast.Name('AssertionError', ast.Load()),
                                            [] if t.msg is None else [t.msg],
                                            [], None, None),
                                   None)],
                        [])
        return ast.copy_location(result, t)

