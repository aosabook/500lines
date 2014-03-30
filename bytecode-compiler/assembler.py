"""
Produce the bytecode bytestring, stack depth, and line-number
mapping from a hierarchical symbolic form with local labels for
addresses.
"""

import dis
from stack_effect import stack_effect

def jump_stack_effect(opcode):
    return jump_stack_effects.get(opcode, stack_effect(opcode))
jump_stack_effects = {dis.opmap['FOR_ITER']: -1,
                      dis.opmap['JUMP_IF_TRUE_OR_POP']: 0,
                      dis.opmap['JUMP_IF_FALSE_OR_POP']: 0}

def take_argument(opcode):
    code0 = encode(opcode, 0)
    if opcode in dis.hasjrel:
        return lambda label: (code0, [(label, lambda addr: addr, jump_stack_effect(opcode))], stack_effect(opcode), None)
    elif opcode in dis.hasjabs:
        return lambda label: (code0, [(label, lambda addr: 0, jump_stack_effect(opcode))], stack_effect(opcode), None)
    else:
        return lambda arg: (encode(opcode, arg), [], stack_effect(opcode, arg), None)

def encode(opcode, arg): return [opcode, arg % 256, arg // 256]

class Opcodes: pass
op = Opcodes()
for name, opcode in dis.opmap.items():
    setattr(op, name,
            ([opcode], [], stack_effect(opcode), None) if opcode < dis.HAVE_ARGUMENT else take_argument(opcode))

def set_lineno(line):
    return ([], [], 0, line)

def assemble(assembly):
    code = []
    max_depth = 0
    firstlineno, lnotab, cur_byte, cur_line = None, [], 0, None

    def flatten(assembly, refs, depth, depth_at_label):
        """Assemble `assembly`: append bytecode to `code`, append references
        to labels into `refs`, take `depth` as the stack depth at entry, and
        assign stack depths at labels to `depth_at_label`. Update
        `firstlineno` and `lnotab`. Return the stack depth at exit."""
        nonlocal max_depth, firstlineno, lnotab, cur_byte, cur_line
        if isinstance(assembly, tuple):
            my_code, my_linking, my_stack_effect, my_lineno = assembly

            if my_lineno is not None:
                if firstlineno is None:
                    firstlineno = cur_line = my_lineno
                elif my_lineno > cur_line: # XXX should be != ideally
                    byte_step = len(code) - cur_byte
                    line_step = my_lineno - cur_line
                    cur_byte, cur_line = len(code), my_lineno
                    while 255 < byte_step:
                        lnotab += [255, 0]
                        byte_step -= 255
                    while 255 < line_step:
                        lnotab += [byte_step, 255]
                        byte_step = 0
                        line_step -= 255
                    if (byte_step, line_step) != (0, 0):
                        lnotab += [byte_step, line_step]
                    
            for label, fixup, stack_effect in my_linking:
                refs.append((len(code) + 1, label, fixup))
                depth_at_label[label] = depth + stack_effect

            depth += my_stack_effect
            max_depth = max(max_depth, depth)

            code.extend(my_code)

        elif isinstance(assembly, list):
            for subassembly in assembly:
                depth = flatten(subassembly, refs, depth, depth_at_label)

        elif isinstance(assembly, dict):
            my_refs = []
            my_addresses = {}
            my_depth_at_label = {}
            for label, subassembly in sorted(assembly.items()):
                my_addresses[label] = len(code)
                depth = my_depth_at_label.get(label, depth)
                depth = flatten(subassembly, my_refs, depth, my_depth_at_label)
            for address, label, fixup in my_refs:
                target = my_addresses[label] - fixup(address+2)
                code[address+0] = target % 256
                code[address+1] = target // 256

        else:
            raise TypeError("Not an assembly", assembly)

        return depth

    refs = []
    depth_at_label = {}
    flatten(assembly, refs, 0, depth_at_label)
    assert not refs and not depth_at_label
#    print(assembly); print(lnotab)
    return bytes(tuple(code)), max_depth, firstlineno, bytes(lnotab)

if __name__ == '__main__':
    example = {0: [op.LOAD_CONST(0), op.POP_JUMP_IF_FALSE(1),
                   op.LOAD_CONST(1), op.JUMP_FORWARD(2)],
               1: [op.LOAD_CONST(2)],
               2: []}
    bytecode, stack_depth, firstlineno, lnotab = assemble(example)
    print(stack_depth, firstlineno, lnotab)
    for pc, byte in enumerate(bytecode):
        print(pc, byte)
