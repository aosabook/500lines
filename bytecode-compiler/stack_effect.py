"""
PyCompile_OpcodeStackEffect(int opcode, int oparg) from
Python/compile.c This is dis.stack_effect in Python 3.4, but I'm
making a version of it available here for convenience in
development. This should not be needed in the released code.
"""

import dis
class Op(): pass
op = Op()
op.__dict__.update(dis.opmap)

def stack_effect(opcode, oparg=None):
    if opcode in effect_map:
        return effect_map[opcode]
    elif opcode == op.UNPACK_SEQUENCE:
        return oparg-1;
    elif opcode == op.UNPACK_EX:
        return (oparg&0xFF) + (oparg>>8)
    elif opcode == op.BUILD_SET:
        return 1-oparg
    elif opcode == op.RAISE_VARARGS:
        return -oparg
    elif opcode == op.CALL_FUNCTION:
        return -NARGS(oparg)
    elif opcode in (op.CALL_FUNCTION_VAR, op.CALL_FUNCTION_KW):
        return -NARGS(oparg)-1
    elif opcode == op.CALL_FUNCTION_VAR_KW:
        return -NARGS(oparg)-2
    elif opcode == op.MAKE_FUNCTION:
        return -1 -NARGS(oparg) - ((oparg >> 16) & 0xffff)
    elif opcode == op.MAKE_CLOSURE:
        return -2 - NARGS(oparg) - ((oparg >> 16) & 0xffff)
    elif opcode == op.BUILD_SLICE:
        if (oparg == 3):
            return -2
        else:
            return -1
    else:
        assert False, dis.opname[opcode]

def NARGS(o): return (((o) % 256) + 2*(((o) // 256) % 256))

effect_map = {
    op.POP_TOP: -1,

    op.NOP: 0, # (this was left out of compile.c -- presumably an oversight)

    op.ROT_TWO: 0,
    op.ROT_THREE: 0,
    op.DUP_TOP: 1,
    op.DUP_TOP_TWO: 2,

    op.UNARY_POSITIVE: 0,
    op.UNARY_NEGATIVE: 0,
    op.UNARY_NOT: 0,
    op.UNARY_INVERT: 0,

    op.SET_ADD: -1,
    op.LIST_APPEND: -1,
    op.MAP_ADD: -2,

    op.BINARY_POWER: -1,
    op.BINARY_MULTIPLY: -1,
    op.BINARY_MODULO: -1,
    op.BINARY_ADD: -1,
    op.BINARY_SUBTRACT: -1,
    op.BINARY_SUBSCR: -1,
    op.BINARY_FLOOR_DIVIDE: -1,
    op.BINARY_TRUE_DIVIDE: -1,
    op.INPLACE_FLOOR_DIVIDE: -1,
    op.INPLACE_TRUE_DIVIDE: -1,
    op.INPLACE_ADD: -1,
    op.INPLACE_SUBTRACT: -1,
    op.INPLACE_MULTIPLY: -1,
    op.INPLACE_MODULO: -1,

    op.STORE_SUBSCR: -3,
    op.STORE_MAP: -2,
    op.DELETE_SUBSCR: -2,

    op.BINARY_LSHIFT: -1,
    op.BINARY_RSHIFT: -1,
    op.BINARY_AND: -1,
    op.BINARY_XOR: -1,
    op.BINARY_OR: -1,
    op.INPLACE_POWER: -1,

    op.GET_ITER: 0,

    op.PRINT_EXPR: -1,

    op.LOAD_BUILD_CLASS: 1,

    op.INPLACE_LSHIFT: -1,
    op.INPLACE_RSHIFT: -1,
    op.INPLACE_AND: -1,
    op.INPLACE_XOR: -1,
    op.INPLACE_OR: -1,

    op.BREAK_LOOP: 0,
    op.SETUP_WITH: 7,
    op.WITH_CLEANUP: -1, # XXX Sometimes more
    op.RETURN_VALUE: -1,
    op.IMPORT_STAR: -1,
    op.YIELD_VALUE: 0,
    op.YIELD_FROM: -1,
    op.POP_BLOCK: 0,
    op.POP_EXCEPT: 0, # -3 except if bad bytecode
    op.END_FINALLY: -1, # or -2 or -3 if exception occurred

    op.STORE_NAME: -1,
    op.DELETE_NAME: 0,

    op.FOR_ITER: 1, # or -1, at end of iterator

    op.STORE_ATTR: -2,
    op.DELETE_ATTR: -1,
    op.STORE_GLOBAL: -1,
    op.DELETE_GLOBAL: 0,
    op.LOAD_CONST: 1,
    op.LOAD_NAME: 1,

    op.BUILD_TUPLE: 1,
    op.BUILD_LIST: 1,
    op.BUILD_MAP: 1,
    op.LOAD_ATTR: 0,
    op.COMPARE_OP: -1,
    op.IMPORT_NAME: -1,
    op.IMPORT_FROM: 1,

    op.JUMP_FORWARD: 0,
    op.JUMP_IF_TRUE_OR_POP: 0, # -1 if jump not taken
    op.JUMP_IF_FALSE_OR_POP: 0,  #  ""
    op.JUMP_ABSOLUTE: 0,

    op.POP_JUMP_IF_FALSE: -1,
    op.POP_JUMP_IF_TRUE: -1,

    op.LOAD_GLOBAL: 1,

    op.CONTINUE_LOOP: 0,
    op.SETUP_LOOP: 0,

    op.SETUP_EXCEPT: 6,
    op.SETUP_FINALLY: 6, # can push 3 values for the new exception
                         # + 3 others for the previous exception state

    op.LOAD_FAST: 1,
    op.STORE_FAST: -1,
    op.DELETE_FAST: 0,

    op.LOAD_CLOSURE: 1,
    op.LOAD_DEREF: 1,
    op.LOAD_CLASSDEREF: 1,
    op.STORE_DEREF: -1,
    op.DELETE_DEREF: 0,
}
