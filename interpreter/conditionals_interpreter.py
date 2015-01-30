# Abandoned this because calculating jump targets by hand is *so tedious*

class SimpleInterpreter(object):
    def __init__(self):
        self.stack = []
        self.environment = {}
        self.should_stop = False
        # instruction_map = {1: self.LOAD_VALUE,
        #                    2: self.ADD_TWO_VALUES,
        #                    3: self.POP_FROM_STACK }

    def LOAD_VALUE(self, number):
        self.stack.append(number)

    def PRINT_ANSWER(self):
        answer = self.stack.pop()
        print(answer)

    def ADD_TWO_VALUES(self):
        first_num = self.stack.pop()
        second_num = self.stack.pop()
        total = first_num + second_num
        self.stack.append(total)

    def BINARY_LESS_THAN(self):
        a, b = self.stack.pop(), self.stack.pop()
        self.stack.append((a < b))

    def STORE_NAME(self, name):
        val = self.stack.pop()
        print("storing name %s: %s" % (name, val))
        self.environment[name] = val

    def LOAD_NAME(self, name):
        val = self.environment[name]
        self.stack.append(val)

    def JUMP(self, jump_target):
        self.next_i = jump_target

    def JUMP_IF_FALSE(self, jump_target):
        cond = self.stack.pop()
        if not cond:
            self.next_i = jump_target

    def RETURN(self):
        self.should_stop = True

    def parse_argument(self, instruction, argument, what_to_execute):
        argument_meaning = {"values": ["LOAD_VALUE"],
                            "names": ["LOAD_NAME", "STORE_NAME"],
                            "jumps": ["JUMP_IF_FALSE", "JUMP"]}

        if instruction in argument_meaning["values"]:
            argument = what_to_execute["values"][argument]
        elif instruction in argument_meaning["names"]:
            argument = what_to_execute["names"][argument]
        elif instruction in argument_meaning["jumps"]:
            pass

        return argument


    def execute(self, what_to_execute):
        instructions = what_to_execute["instructions"]
        self.next_i = 0
        while True:
            if self.next_i >= len(instructions) or self.should_stop:
                break
            print(self.next_i)
            print(self.environment)
            instruction, argument = instructions[self.next_i]
            print(instructions[self.next_i])
            argument = self.parse_argument(instruction, argument, what_to_execute)

            if instruction == "LOAD_VALUE":
                self.LOAD_VALUE(argument)
            elif instruction == "ADD_TWO_VALUES":
                self.ADD_TWO_VALUES()
            elif instruction == "PRINT_ANSWER":
                self.PRINT_ANSWER()
            elif instruction == "STORE_NAME":
                self.STORE_NAME(argument)
            elif instruction == "LOAD_NAME":
                self.LOAD_NAME(argument)
            elif instruction == "JUMP_IF_FALSE":
                self.JUMP_IF_FALSE(argument)
            elif instruction == "JUMP":
                self.JUMP(argument)
            elif instruction == "RETURN":
                self.RETURN()

            self.next_i += 1

def test_simple_interpreter():
    def test_conditional_true():
        what_to_execute = {
            "instructions": [("LOAD_VALUE", 0),
                             ("STORE_NAME", 0),
                             ("LOAD_NAME", 0),
                             ("JUMP_IF_FALSE", 6),
                             ("LOAD_VALUE", 1),
                             ("PRINT_ANSWER", None),
                             ("RETURN", None),
                             ("LOAD_VALUE", 2),
                             ("PRINT_ANSWER", None),
                             ("RETURN", None)],
            "values": [True, 'yes', 'no'],
            "names": ["a"] }

        interpreter = SimpleInterpreter()
        interpreter.execute(what_to_execute)
        print("== 'yes'")
    test_conditional_true()

    def test_conditional_false():
        what_to_execute = {
            "instructions": [("LOAD_VALUE", 0),
                             ("STORE_NAME", 0),
                             ("LOAD_NAME", 0),
                             ("JUMP_IF_FALSE", 6),
                             ("LOAD_VALUE", 1),
                             ("PRINT_ANSWER", None),
                             ("RETURN", None),
                             ("LOAD_VALUE", 2),
                             ("PRINT_ANSWER", None),
                             ("RETURN", None)],
            "values": [False, 'yes', 'no'],
            "names": ["a"] }
        interpreter = SimpleInterpreter()
        interpreter.execute(what_to_execute)
        print("== 'no'")
    test_conditional_false()

    # def test_loop():
        # >>> def loop():
        #         x = 1
        # ...     while x < 5:
        # ...         print("going")
        # what_to_execute = {
        #     "instructions": [("LOAD_VALUE", 0),
        #                      ("STORE_NAME", 0),
        #                      ("LOAD_NAME", 0),
        #                      ("LOAD_VALUE", 1),
        #                      ("BINARY_LESS_THAN", None),
        #                      ("JUMP_IF_FALSE", 8), # <- decide whether or not to jump FIXME
        #                      ("LOAD_VALUE", 2),
        #                      ("PRINT_ANSWER", None),
        #                      ("LOAD_NAME", 0),
        #                      ("LOAD_VALUE", 1),
        #                      ("ADD_TWO_VALUES", None),
        #                      ("STORE_NAME", 0),
        #                      ("JUMP", 1),        # <- Jump to top of loop
        #                      ("RETURN", None)],  # <- Jump to here when done looping
        #     "values": [1, 5, 'going'],
        #     "names": ["x"] }
        # interpreter = SimpleInterpreter()
        # interpreter.execute(what_to_execute)
        # print("==" + "going\n"*5)
    # test_loop()

if __name__ == '__main__':
    test_simple_interpreter()

