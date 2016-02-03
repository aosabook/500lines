"""
Compile a file to bytecode and write the bytecode to a file,
in the standard way. But this lets you substitute a nonstandard
compiler.

>>> compile_file(filename, compile=your_compiler)

Sometime I should learn how to integrate into the import machinery and
let it take care of caching to the filesystem, instead.
"""

# For Py3.3+

import imp, marshal, os
from struct import pack

def compile_file(filename, compile=compile):
    with open(filename) as f:
        source = f.read()
    mtime = os.path.getmtime(filename) # XXX race
    code = compile(source, filename, 'exec', dont_inherit=True)
    return marshal_to_pyc(code, int(mtime), len(source)) # XXX len of bytes instead of string?

def marshal_to_pyc(code, mtime, source_size):
    return (imp.get_magic() + pack('L', mtime) + pack('L', source_size)
            + marshal.dumps(code))

# from importlib._bootstrap import _code_to_bytecode
# marshal_to_pyc = _code_to_bytecode

if __name__ == '__main__':
    import sys
    filename = sys.argv[1]
    pyc = compile_file(filename)
    pyc_filename = imp.cache_from_source(filename)
    # pyc_filename = filename + '.compiled'
    with open(pyc_filename, 'wb') as f:
        f.write(pyc)
