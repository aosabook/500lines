class VirtualMachine(object):
    def __init__(self):
        self.return_value = None

    # Frame manipulation
    def make_frame(self, code, callargs={}, f_globals=None, f_locals=None):
        if f_globals is not None and f_locals is not None:
            f_locals = f_globals
        elif self.frames:
            f_globals = self.frame.f_globals
            f_locals = {}
        else:
            f_globals = f_locals = {
                '__builtins__': __builtins__,
                '__name__': '__main__',
                '__doc__': None,
                '__package__': None,
            }
        f_locals.update(callargs)
        frame = Frame(code, f_globals, f_locals, self.frame)
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

    # Data stack manipulation
    def top(self):
        return self.frame.stack[-1]

    def pop(self):
        return self.frame.stack.pop()

    def push(self, *vals):
        self.frame.stack.extend(vals)

    def popn(self, n):
        """Pop a number of values from the value stack.
        A list of `n` values is returned, the deepest value first.
        """
        if n:
            ret = self.frame.stack[-n:]
            self.frame.stack[-n:] = []
            return ret
        else:
            return []

    # Block stack manipulation
    def push_block(self, b_type, handler=None):
        level = len(self.frame.stack)
        self.frame.block_stack.append(Block(b_type, handler, level))

    def pop_block(self):
        return self.frame.block_stack.pop()

    def unwind_block(self, block):
        """Unwind the values on the data stack corresponding to a given block."""
        if block.type == 'except-handler':
            offset = 3
        else:
            offset = 0

        while len(self.frame.stack) > block.level + offset:
            self.pop()

        if block.type == 'except-handler':
            traceback, value, exctype = self.popn(3)
            self.last_exception = exctype, value, traceback

    # Jumping through bytecode
    def jump(self, jump):
        """Move the bytecode pointer to `jump`, so it will execute next."""
        self.frame.f_lasti = jump

    def run_code(self, code, f_globals=None, f_locals=None):
        """ An entry point to execute code using the virtual machine."""
        frame = self.make_frame(code, f_globals=f_globals, f_locals=f_locals)
        val = self.run_frame(frame)
        # Check some invariants
        # if self.frames:
        #     raise VirtualMachineError("Frames left over!")
        # if self.frame and self.frame.stack:
        #     raise VirtualMachineError("Data left on stack! %r" % self.frame.stack)

        return val # for testing - will be removed

    def parse_byte_and_args(self):
        f = self.frame
        opoffset = f.f_lasti
        byteCode = f.f_code.co_code[opoffset]
        f.f_lasti += 1
        byteName = dis.opname[byteCode]
        arg = None
        arguments = []
        if byteCode >= dis.HAVE_ARGUMENT:
            arg = f.f_code.co_code[f.f_lasti:f.f_lasti+2]
            f.f_lasti += 2
            intArg = arg[0] + (arg[1] << 8)
            if byteCode in dis.hasconst:   # Look up a constant
                arg = f.f_code.co_consts[intArg]
            elif byteCode in dis.hasname:  # Look up a name
                arg = f.f_code.co_names[intArg]
            elif byteCode in dis.haslocal: # Look up a local name
                arg = f.f_code.co_varnames[intArg]
            elif byteCode in dis.hasjrel:  # Calculate a relative jump
                arg = f.f_lasti + intArg
            else:
                arg = intArg
            arguments = [arg]

        return byteName, arguments

    def dispatch(self, byteName, arguments):
        """ Dispatch by bytename to the corresponding methods.
        Exceptions are caught and set on the virtual machine."""

        # When later unwinding the block stack,
        # we need to keep track of why we are doing it.
        why = None
        try:
            if byteName.startswith('UNARY_'):
                self.unaryOperator(byteName[6:])
            elif byteName.startswith('BINARY_'):
                self.binaryOperator(byteName[7:])
            else:
                # main dispatch
                bytecode_fn = getattr(self, 'byte_%s' % byteName, None)
                if not bytecode_fn:            # pragma: no cover
                    raise VirtualMachineError(
                        "unsupported bytecode type: %s" % byteName
                    )
                why = bytecode_fn(*arguments)
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

        self.pop_block()
        self.unwind_block(block)

        if block.type == 'loop' and why == 'break':
            why = None
            self.jump(block.handler)
            return why

        if (block.type in ['setup-except', 'finally'] and why == 'exception'):
            self.push_block('except-handler')
            exctype, value, tb = self.last_exception
            self.push(tb, value, exctype)
            self.push(tb, value, exctype) # yes, twice
            why = None
            self.jump(block.handler)
            return why

        elif block.type == 'finally':
            if why in ('return', 'continue'):
                self.push(self.return_value)

            self.push(why)

            why = None
            self.jump(block.handler)
            return why
        return why


    def run_frame(self, frame):
        """Run a frame until it returns (somehow).
        Exceptions are raised, the return value is returned.
        """
        self.push_frame(frame)
        while True:
            byteName, arguments = self.parse_byte_and_args()

            why = self.dispatch(byteName, arguments)

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
        self.push(const)

    def byte_POP_TOP(self):
        self.pop()

    def byte_DUP_TOP(self):
        self.push(self.top())

    ## Names
    def byte_LOAD_NAME(self, name):
        frame = self.frame
        if name in frame.f_locals:
            val = frame.f_locals[name]
        elif name in frame.f_globals:
            val = frame.f_globals[name]
        elif name in frame.f_builtins:
            val = frame.f_builtins[name]
        else:
            raise NameError("name '%s' is not defined" % name)
        self.push(val)

    def byte_STORE_NAME(self, name):
        self.frame.f_locals[name] = self.pop()

    def byte_DELETE_NAME(self, name):
        del self.frame.f_locals[name]

    def byte_LOAD_FAST(self, name):
        if name in self.frame.f_locals:
            val = self.frame.f_locals[name]
        else:
            raise UnboundLocalError(
                "local variable '%s' referenced before assignment" % name
            )
        self.push(val)

    def byte_STORE_FAST(self, name):
        self.frame.f_locals[name] = self.pop()

    def byte_LOAD_GLOBAL(self, name):
        f = self.frame
        if name in f.f_globals:
            val = f.f_globals[name]
        elif name in f.f_builtins:
            val = f.f_builtins[name]
        else:
            raise NameError("global name '%s' is not defined" % name)
        self.push(val)

    ## Operators

    UNARY_OPERATORS = {
        'POSITIVE': operator.pos,
        'NEGATIVE': operator.neg,
        'NOT':      operator.not_,
        'INVERT':   operator.invert,
    }

    def unaryOperator(self, op):
        x = self.pop()
        self.push(self.UNARY_OPERATORS[op](x))

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
        x, y = self.popn(2)
        self.push(self.BINARY_OPERATORS[op](x, y))

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
        x, y = self.popn(2)
        self.push(self.COMPARE_OPERATORS[opnum](x, y))

    ## Attributes and indexing

    def byte_LOAD_ATTR(self, attr):
        obj = self.pop()
        val = getattr(obj, attr)
        self.push(val)

    def byte_STORE_ATTR(self, name):
        val, obj = self.popn(2)
        setattr(obj, name, val)

    def byte_STORE_SUBSCR(self):
        val, obj, subscr = self.popn(3)
        obj[subscr] = val

    ## Building

    def byte_BUILD_TUPLE(self, count):
        elts = self.popn(count)
        self.push(tuple(elts))

    def byte_BUILD_LIST(self, count):
        elts = self.popn(count)
        self.push(elts)

    def byte_BUILD_MAP(self, size):
        self.push({})

    def byte_STORE_MAP(self):
        the_map, val, key = self.popn(3)
        the_map[key] = val
        self.push(the_map)

    def byte_UNPACK_SEQUENCE(self, count):
        seq = self.pop()
        for x in reversed(seq):
            self.push(x)
