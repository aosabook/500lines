"""A main program for Byterun."""

import logging, sys, imp, os
from pyvm2 import VirtualMachine

def run_python_file(filename):
    """Run a python file as if it were the main program on the command line.
    `filename` is the path to the file to execute, it need not be a .py file.
    """
    old_main_mod = sys.modules['__main__']
    main_mod = imp.new_module('__main__') # Create a module to serve as __main__
    sys.modules['__main__'] = main_mod
    main_mod.__builtins__ = sys.modules['builtins']

    try:
        with open(filename, 'rU') as f:
            source = f.read()

        if not source or source[-1] != '\n':
            source += '\n' # `compile` still needs the last line to be clean,
        code = compile(source, filename, "exec")

        vm = VirtualMachine()
        vm.run_code(code, f_globals=main_mod.__dict__)
    finally:
        sys.modules['__main__'] = old_main_mod # Restore the old __main__

if __name__ == '__main__':
    run_python_file(sys.argv[1])
