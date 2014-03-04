"""
Produce bytecode bytestrings from a hierarchical symbolic form with
local labels for addresses.
"""

import dis
from stack_effect import stack_effect

def jump_stack_effect(opcode):
    return stack_effect(opcode) # XXX

def take_argument(opcode):
    code0 = encode(opcode, 0)
    if opcode in dis.hasjrel:
        return lambda label: (code0, [(label, lambda addr: addr, jump_stack_effect(opcode))], stack_effect(opcode))
    elif opcode in dis.hasjabs:
        return lambda label: (code0, [(label, lambda addr: 0, jump_stack_effect(opcode))], stack_effect(opcode))
    else:
        return lambda arg: (encode(opcode, arg), [], stack_effect(opcode, arg))

def encode(opcode, arg): return [opcode, arg % 256, arg // 256]

class Opcodes: pass
op = Opcodes()
for name, opcode in dis.opmap.items():
    setattr(op, name,
            ([opcode], [], stack_effect(opcode)) if opcode < dis.HAVE_ARGUMENT else take_argument(opcode))

def assemble(assembly):
    code = []

    def flatten(assembly, refs):
        if isinstance(assembly, tuple):
            my_code, my_linking, my_stack_effect = assembly
            refs.extend((len(code) + 1, label, fixup)
                        for label, fixup, _ in my_linking)
            code.extend(my_code)
        elif isinstance(assembly, list):
            for subassembly in assembly:
                flatten(subassembly, refs)
        elif isinstance(assembly, dict):
            my_refs = []
            my_addresses = {}
            for label, subassembly in sorted(assembly.items()):
                my_addresses[label] = len(code)
                flatten(subassembly, my_refs)
            for address, label, fixup in my_refs:
                target = my_addresses[label] - fixup(address+2)
                code[address+0] = target % 256
                code[address+1] = target // 256
        else:
            assert False, "Yo: {}".format(assembly)

    refs = []
    flatten(assembly, refs)
    assert not refs
    return bytes(tuple(code))

def stack_depth(assembly):
    max_depth = 0
    def stack_depths(assembly, depth, depth_at_label):
        nonlocal max_depth
        if isinstance(assembly, tuple):
            my_code, my_linking, my_stack_effect = assembly
            for label, _, stack_effect in my_linking:
                depth_at_label[label] = depth + stack_effect
            depth += my_stack_effect
            max_depth = max(max_depth, depth)
        elif isinstance(assembly, list):
            for subassembly in assembly:
                depth = stack_depths(subassembly, depth, depth_at_label)
        elif isinstance(assembly, dict):
            my_depth_at_label = {}
            for label, subassembly in sorted(assembly.items()):
                depth = my_depth_at_label.get(label, depth)
                depth = stack_depths(subassembly, depth, my_depth_at_label)
        else:
            assert False
        return depth
    depth_at_label = {}
    stack_depths(assembly, 0, depth_at_label)
    assert not depth_at_label
    return max_depth

if __name__ == '__main__':
    example = {0: [op.LOAD_CONST(0), op.POP_JUMP_IF_FALSE(1),
                   op.LOAD_CONST(1), op.JUMP_FORWARD(2)],
               1: [op.LOAD_CONST(2)],
               2: []}
    for pc, byte in enumerate(assemble(example)):
        print(pc, byte)
