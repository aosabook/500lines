"""
Wrapper fixing dis.stack_effect for this compiler.
TODO: I think we can delete this module now.
"""

import dis

or_pop_instructions = (dis.opmap['JUMP_IF_TRUE_OR_POP'],
                       dis.opmap['JUMP_IF_FALSE_OR_POP'])

def stack_effect(opcode, oparg=None):
    "Return the stack effect as seen by the following instruction."
    if opcode in or_pop_instructions:
        # N.B. these are 0 in the dis version; -1 if jump not taken
        return -1
    else:
        return dis.stack_effect(opcode, oparg if isinstance(oparg, (int, type(None))) else 0)
