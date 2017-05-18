"""A pure-Python Python bytecode interpreter."""
# Adapted from:
# 1. pyvm2 by Paul Swartz (z3p), from http://www.twistedmatrix.com/users/z3p/
# 2. byterun by Ned Batchelder, github.com/nedbat/byterun

import dis, operator, sys, collections, inspect, types

class Frame(object):
    def __init__(self, code_obj, global_names, local_names, prev_frame):
        self.code_obj = code_obj
        self.global_names = global_names
        self.local_names = local_names
        self.prev_frame = prev_frame
        self.stack = []
        if prev_frame:
            self.builtin_names = prev_frame.builtin_names
        else:
            self.builtin_names = local_names['__builtins__']
            if hasattr(self.builtin_names, '__dict__'):
                self.builtin_names = self.builtin_names.__dict__

        self.last_instruction = 0
        self.block_stack = []

    # Data stack manipulation
    def top(self):
        return self.stack[-1]

    def pop(self):
        return self.stack.pop()

    def push(self, *vals):
        self.stack.extend(vals)

    def popn(self, n):
        """Pop a number of values from the value stack.
        A list of `n` values is returned, the deepest value first.
        """
        if n:
            ret = self.stack[-n:]
            self.stack[-n:] = []
            return ret
        else:
            return []

    # Block stack manipulation
    def push_block(self, b_type, handler=None):
        stack_height = len(self.stack)
        self.block_stack.append(Block(b_type, handler, stack_height))

    def pop_block(self):
        return self.block_stack.pop()

    def unwind_block(self, block):
        """Unwind the values on the data stack when a given block is finished."""
        if block.type == 'except-handler':
            offset = 3
        else:
            offset = 0

        while len(self.stack) > block.stack_height + offset:
            self.pop()

        if block.type == 'except-handler':
            traceback, value, exctype = self.popn(3)
            return exctype, value, traceback

Block = collections.namedtuple("Block", "type, handler, stack_height")

class Function(object):
    __slots__ = [
        'func_code', 'func_name', 'func_defaults', 'func_globals',
        'func_locals', 'func_dict', 'func_closure',
        '__name__', '__dict__', '__doc__',
        '_vm', '_func',
    ]


    def __init__(self, name, code, globs, defaults, closure, vm):
        self._vm = vm
        self.func_code = code
        self.func_name = self.__name__ = name or code.co_name
        self.func_defaults = tuple(defaults)
        self.func_globals = globs
        self.func_locals = self._vm.frame.local_names
        self.__dict__ = {}
        self.func_closure = closure
        self.__doc__ = code.co_consts[0] if code.co_consts else None

        # Sometimes, we need a real Python function.  This is for that.
        kw = {
            'argdefs': self.func_defaults,
        }
        if closure:
            kw['closure'] = tuple(make_cell(0) for _ in closure)
        self._func = types.FunctionType(code, globs, **kw)

    def __call__(self, *args, **kwargs):
        callargs = inspect.getcallargs(self._func, *args, **kwargs)
        frame = self._vm.make_frame(
            self.func_code, callargs, self.func_globals, {}
        )
        return self._vm.run_frame(frame)

def make_cell(value):
    # Thanks to Alex Gaynor for help with this bit of twistiness.
    fn = (lambda x: lambda: x)(value)
    return fn.__closure__[0]



class VirtualMachineError(Exception):
    pass

class VirtualMachine(object):
    def __init__(self):
        self.frames = []   # The call stack of frames.
        self.frame = None  # The current frame.
        self.return_value = None
        self.last_exception = None

    # Frame manipulation
    def make_frame(self, code, callargs={}, global_names=None, local_names=None):
        if global_names is not None and local_names is not None:
            local_names = global_names
        elif self.frames:
            global_names = self.frame.global_names
            local_names = {}
        else:
            global_names = local_names = {
                '__builtins__': __builtins__,
                '__name__': '__main__',
                '__doc__': None,
                '__package__': None,
            }
        local_names.update(callargs)
        frame = Frame(code, global_names, local_names, self.frame)
        return frame

    def push_frame(self, frame):
        self.frames.append(frame)
        self.frame = frame

    def pop_frame(self):
        self.frames.pop()
        if self.frames:
            self.frame = self.frames[-1]
        else:
            self.frame = None

    # Jumping through bytecode
    def jump(self, jump):
        """Move the bytecode pointer to `jump`, so it will execute next."""
        self.frame.last_instruction = jump

    def run_code(self, code, global_names=None, local_names=None):
        """ An entry point to execute code using the virtual machine."""
        frame = self.make_frame(code, global_names=global_names, local_names=local_names)

        self.run_frame(frame)
        # Check some invariants
        # if self.frames:
        #     raise VirtualMachineError("Frames left over!")
        # if self.frame and self.frame.stack:
        #     raise VirtualMachineError("Data left on stack! %r" % self.frame.stack)

        # for testing, was val = self.run_frame(frame)
        # return val # for testing

    def parse_byte_and_args(self):
        f = self.frame
        opoffset = f.last_instruction
        byteCode = f.code_obj.co_code[opoffset]
        f.last_instruction += 1
        byte_name = dis.opname[byteCode]
        if byteCode >= dis.HAVE_ARGUMENT:
            arg = f.code_obj.co_code[f.last_instruction:f.last_instruction+2]  # index into the bytecode
            f.last_instruction += 2   # advance the instruction pointer
            arg_val = arg[0] + (arg[1] << 8)
            if byteCode in dis.hasconst:   # Look up a constant
                arg = f.code_obj.co_consts[arg_val]
            elif byteCode in dis.hasname:  # Look up a name
                arg = f.code_obj.co_names[arg_val]
            elif byteCode in dis.haslocal: # Look up a local name
                arg = f.code_obj.co_varnames[arg_val]
            elif byteCode in dis.hasjrel:  # Calculate a relative jump
                arg = f.last_instruction + arg_val
            else:
                arg = arg_val
            argument = [arg]
        else:
            argument = []

        return byte_name, argument

    def dispatch(self, byte_name, argument):
        """ Dispatch by bytename to the corresponding methods.
        Exceptions are caught and set on the virtual machine."""

        # When later unwinding the block stack,
        # we need to keep track of why we are doing it.
        why = None
        try:
            bytecode_fn = getattr(self, 'byte_%s' % byte_name, None)
            if bytecode_fn is None:
                if byte_name.startswith('UNARY_'):
                    self.unaryOperator(byte_name[6:])
                elif byte_name.startswith('BINARY_'):
                    self.binaryOperator(byte_name[7:])
                else:
                    raise VirtualMachineError(
                        "unsupported bytecode type: %s" % byte_name
                    )
            else:
                why = bytecode_fn(*argument)
        except:
            # deal with exceptions encountered while executing the op.
            self.last_exception = sys.exc_info()[:2] + (None,)
            why = 'exception'

        return why

    def manage_block_stack(self, why):
        block = self.frame.block_stack[-1]

        if block.type == 'loop' and why == 'continue':
            self.jump(self.return_value)
            why = None
            return why

        self.frame.pop_block()
        current_exc = self.frame.unwind_block(block)
        if current_exc is not None:
            self.last_exception = current_exc

        if block.type == 'loop' and why == 'break':
            self.jump(block.handler)
            why = None

        elif (block.type in ['setup-except', 'finally'] and why == 'exception'):
            self.frame.push_block('except-handler')
            exctype, value, tb = self.last_exception
            self.frame.push(tb, value, exctype)
            self.frame.push(tb, value, exctype) # yes, twice
            self.jump(block.handler)
            why = None

        elif block.type == 'finally':
            if why in ('return', 'continue'):
                self.frame.push(self.return_value)
            self.frame.push(why)
            self.jump(block.handler)
            why = None

        return why


    def run_frame(self, frame):
        """Run a frame until it returns (somehow).
        Exceptions are raised, the return value is returned.
        """
        self.push_frame(frame)
        while True:
            byte_name, argument = self.parse_byte_and_args()

            why = self.dispatch(byte_name, argument)

            # Deal with any block management we need to do
            while why and frame.block_stack:
                why = self.manage_block_stack(why)

            if why:
                break

        self.pop_frame()

        if why == 'exception':
            exc, val, tb = self.last_exception
            e = exc(val)
            e.__traceback__ = tb
            raise e

        return self.return_value

    ## Stack manipulation

    def byte_LOAD_CONST(self, const):
        self.frame.push(const)

    def byte_POP_TOP(self):
        self.frame.pop()

    def byte_DUP_TOP(self):
        self.frame.push(self.frame.top())

    ## Names
    def byte_LOAD_NAME(self, name):
        frame = self.frame
        if name in frame.local_names:
            val = frame.local_names[name]
        elif name in frame.global_names:
            val = frame.global_names[name]
        elif name in frame.builtin_names:
            val = frame.builtin_names[name]
        else:
            raise NameError("name '%s' is not defined" % name)
        self.frame.push(val)

    def byte_STORE_NAME(self, name):
        self.frame.local_names[name] = self.frame.pop()

    def byte_DELETE_NAME(self, name):
        del self.frame.local_names[name]

    def byte_LOAD_FAST(self, name):
        if name in self.frame.local_names:
            val = self.frame.local_names[name]
        else:
            raise UnboundLocalError(
                "local variable '%s' referenced before assignment" % name
            )
        self.frame.push(val)

    def byte_STORE_FAST(self, name):
        self.frame.local_names[name] = self.frame.pop()

    def byte_LOAD_GLOBAL(self, name):
        f = self.frame
        if name in f.global_names:
            val = f.global_names[name]
        elif name in f.builtin_names:
            val = f.builtin_names[name]
        else:
            raise NameError("global name '%s' is not defined" % name)
        f.push(val)

    ## Operators

    UNARY_OPERATORS = {
        'POSITIVE': operator.pos,
        'NEGATIVE': operator.neg,
        'NOT':      operator.not_,
        'INVERT':   operator.invert,
    }

    def unaryOperator(self, op):
        x = self.frame.pop()
        self.frame.push(self.UNARY_OPERATORS[op](x))

    BINARY_OPERATORS = {
        'POWER':    pow,
        'MULTIPLY': operator.mul,
        'FLOOR_DIVIDE': operator.floordiv,
        'TRUE_DIVIDE':  operator.truediv,
        'MODULO':   operator.mod,
        'ADD':      operator.add,
        'SUBTRACT': operator.sub,
        'SUBSCR':   operator.getitem,
        'LSHIFT':   operator.lshift,
        'RSHIFT':   operator.rshift,
        'AND':      operator.and_,
        'XOR':      operator.xor,
        'OR':       operator.or_,
    }

    def binaryOperator(self, op):
        x, y = self.frame.popn(2)
        self.frame.push(self.BINARY_OPERATORS[op](x, y))

    COMPARE_OPERATORS = [
        operator.lt,
        operator.le,
        operator.eq,
        operator.ne,
        operator.gt,
        operator.ge,
        lambda x, y: x in y,
        lambda x, y: x not in y,
        lambda x, y: x is y,
        lambda x, y: x is not y,
        lambda x, y: issubclass(x, Exception) and issubclass(x, y),
    ]

    def byte_COMPARE_OP(self, opnum):
        x, y = self.frame.popn(2)
        self.frame.push(self.COMPARE_OPERATORS[opnum](x, y))

    ## Attributes and indexing

    def byte_LOAD_ATTR(self, attr):
        obj = self.frame.pop()
        val = getattr(obj, attr)
        self.frame.push(val)

    def byte_STORE_ATTR(self, name):
        val, obj = self.frame.popn(2)
        setattr(obj, name, val)

    def byte_STORE_SUBSCR(self):
        val, obj, subscr = self.frame.popn(3)
        obj[subscr] = val

    ## Building

    def byte_BUILD_TUPLE(self, count):
        elts = self.frame.popn(count)
        self.frame.push(tuple(elts))

    def byte_BUILD_LIST(self, count):
        elts = self.frame.popn(count)
        self.frame.push(elts)

    def byte_BUILD_MAP(self, size):
        self.frame.push({})

    def byte_STORE_MAP(self):
        the_map, val, key = self.frame.popn(3)
        the_map[key] = val
        self.frame.push(the_map)

    def byte_UNPACK_SEQUENCE(self, count):
        seq = self.frame.pop()
        for x in reversed(seq):
            self.frame.push(x)

    def byte_BUILD_SLICE(self, count):
        if count == 2:
            x, y = self.frame.popn(2)
            self.frame.push(slice(x, y))
        elif count == 3:
            x, y, z = self.frame.popn(3)
            self.frame.push(slice(x, y, z))
        else:           # pragma: no cover
            raise VirtualMachineError("Strange BUILD_SLICE count: %r" % count)

    def byte_LIST_APPEND(self, count):
        val = self.frame.pop()
        the_list = self.frame.stack[-count] # peek
        the_list.append(val)


    ## Jumps

    def byte_JUMP_FORWARD(self, jump):
        self.jump(jump)

    def byte_JUMP_ABSOLUTE(self, jump):
        self.jump(jump)

    def byte_POP_JUMP_IF_TRUE(self, jump):
        val = self.frame.pop()
        if val:
            self.jump(jump)

    def byte_POP_JUMP_IF_FALSE(self, jump):
        val = self.frame.pop()
        if not val:
            self.jump(jump)

    def byte_JUMP_IF_TRUE_OR_POP(self, jump):
        val = self.frame.top()
        if val:
            self.jump(jump)
        else:
            self.frame.pop()

    def byte_JUMP_IF_FALSE_OR_POP(self, jump):
        val = self.frame.top()
        if not val:
            self.jump(jump)
        else:
            self.frame.pop()

    ## Blocks

    def byte_SETUP_LOOP(self, dest):
        self.frame.push_block('loop', dest)

    def byte_GET_ITER(self):
        self.frame.push(iter(self.frame.pop()))

    def byte_FOR_ITER(self, jump):
        iterobj = self.frame.top()
        try:
            v = next(iterobj)
            self.frame.push(v)
        except StopIteration:
            self.frame.pop()
            self.jump(jump)

    def byte_BREAK_LOOP(self):
        return 'break'

    def byte_CONTINUE_LOOP(self, dest):
        # This is a trick with the return value.
        # While unrolling blocks, continue and return both have to preserve
        # state as the finally blocks are executed.  For continue, it's
        # where to jump to, for return, it's the value to return.  It gets
        # pushed on the stack for both, so continue puts the jump destination
        # into return_value.
        self.return_value = dest
        return 'continue'

    def byte_SETUP_EXCEPT(self, dest):
        self.frame.push_block('setup-except', dest)

    def byte_SETUP_FINALLY(self, dest):
        self.frame.push_block('finally', dest)

    def byte_POP_BLOCK(self):
        self.frame.pop_block()

    def byte_RAISE_VARARGS(self, argc):
        cause = exc = None
        if argc == 2:
            cause = self.frame.pop()
            exc = self.frame.pop()
        elif argc == 1:
            exc = self.frame.pop()
        return self.do_raise(exc, cause)

    def do_raise(self, exc, cause):
        if exc is None:         # reraise
            exc_type, val, tb = self.last_exception

        elif type(exc) == type:  # As in `raise ValueError`
            exc_type = exc
            val = exc()             # Make an instance.
        elif isinstance(exc, BaseException):
            # As in `raise ValueError('foo')`
            exc_type = type(exc)
            val = exc
        else:
            return 'exception' # failure

        self.last_exception = exc_type, val, val.__traceback__
        return 'exception'

    def byte_POP_EXCEPT(self):
        block = self.frame.pop_block()
        if block.type != 'except-handler':
            raise Exception("popped block is not an except handler")
        current_exc = self.frame.unwind_block(block)
        if current_exc is not None:
            self.last_exception = current_exc

    ## Functions

    def byte_MAKE_FUNCTION(self, argc):
        name = self.frame.pop()
        code = self.frame.pop()
        defaults = self.frame.popn(argc)
        globs = self.frame.global_names
        #TODO: if we're not supporting kwargs, do we need the defaults?
        fn = Function(name, code, globs, defaults, None, self)
        self.frame.push(fn)

    def byte_CALL_FUNCTION(self, arg):
        lenKw, lenPos = divmod(arg, 256) # KWargs not supported in byterun
        posargs = self.frame.popn(lenPos)

        func = self.frame.pop()
        frame = self.frame
        retval = func(*posargs)
        self.frame.push(retval)

    def byte_RETURN_VALUE(self):
        self.return_value = self.frame.pop()
        return "return"

    ## Importing

    def byte_IMPORT_NAME(self, name):
        level, fromlist = self.frame.popn(2)
        frame = self.frame
        self.frame.push(__import__(name, frame.global_names, frame.local_names, fromlist, level))

    def byte_IMPORT_FROM(self, name):
        mod = self.frame.top()
        self.frame.push(getattr(mod, name))

    ## And the rest...
    def byte_LOAD_BUILD_CLASS(self):
        self.frame.push(__build_class__)

    def byte_STORE_LOCALS(self):
        self.frame.local_names = self.frame.pop()


