"""
Produce bytecode bytestrings from a hierarchical symbolic form with
local labels for addresses.
"""

import dis

def take_argument(opcode):
    code0 = encode(opcode, 0)
    if opcode in dis.hasjrel:
        return lambda label: (code0, [(label, lambda addr: addr)])
    elif opcode in dis.hasjabs:
        return lambda label: (code0, [(label, lambda addr: 0)])
    else:
        return lambda arg: (encode(opcode, arg), [])

def encode(opcode, arg): return [opcode, arg % 256, arg // 256]

class Opcodes: pass
op = Opcodes()
for name, opcode in dis.opmap.items():
    setattr(op, name,
            ([opcode], []) if opcode < dis.HAVE_ARGUMENT else take_argument(opcode))

def assemble(assembly):
    code = []

    def flatten(assembly, refs):
        if isinstance(assembly, tuple):
            my_code, my_linking = assembly
            refs.extend((len(code) + 1, label, fixup)
                        for label, fixup in my_linking)
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


if __name__ == '__main__':
    example = {0: [op.LOAD_CONST(0), op.POP_JUMP_IF_FALSE(1),
                   op.LOAD_CONST(1), op.JUMP_FORWARD(2)],
               1: [op.LOAD_CONST(2)],
               2: []}
    for pc, byte in enumerate(assemble(example)):
        print(pc, byte)
