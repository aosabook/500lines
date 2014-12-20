class SimpleInterpreter(object):
    def __init__(self):
        self.stack = []
        self.environment = {}
        # instruction_map = {1: self.LOAD_VALUE,
        #                    2: self.ADD_TWO_VALUES,
        #                    3: self.POP_FROM_STACK }

    def LOAD_VALUE(self, argument):
        what_to_push = self.numbers[argument]
        self.stack.append(what_to_push)

    def PRINT_ANSWER(self):
        answer = self.stack.pop()
        print(answer)

    def ADD_TWO_VALUES(self):
        first_num = self.stack.pop()
        second_num = self.stack.pop()
        total = first_num + second_num
        self.stack.append(total)

    def STORE_NAME(self, name):
        val = self.stack.pop()
        print("storing name %s: %s" % (name, val))
        self.environment[name] = val

    def LOAD_NAME(self, name):
        val = self.environment[name]
        self.stack.append(val)

    def execute(self, what_to_execute):
        instructions = what_to_execute["instructions"]

        for each_step in instructions:
            instruction, argument_type, index = each_step
            if argument_type == "number":
                argument = what_to_execute["numbers"][index]
            elif argument_type == "name":
                argument = what_to_execute["name"][index]

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



def test_simple_interpreter():
    simple = SimpleInterpreter()
    what_to_execute = {
        "instructions": [("LOAD_VALUE", 0),  # the first number
                        ("LOAD_VALUE", 1),  # the second number
                        ("ADD_TWO_VALUES", None),
                        ("PRINT_ANSWER", None)],
        "numbers": [7,5] }
    simple.execute(what_to_execute)

    what_to_execute = {
        "instructions": [("LOAD_VALUE", 0),  # the first number
                        ("LOAD_VALUE", 1),  # the second number
                        ("ADD_TWO_VALUES", None),
                        ("LOAD_VALUE", 2),  # the second number
                        ("ADD_TWO_VALUES", None),
                        ("PRINT_ANSWER", None)],
        "numbers": [7,5, 8] }
    simple.execute(what_to_execute)

    what_to_execute = {
        "instructions": [("LOAD_VALUE", 0),
                         ("STORE_NAME", 0),
                         ("LOAD_VALUE", 1),
                         ("STORE_NAME", 1),
                         ("LOAD_NAME", 0),
                         ("LOAD_NAME", 1),
                         ("ADD_TWO_VALUES", None),
                         ("PRINT_ANSWER", None)],
        "numbers": [1, 2],
        "names":   ["a", "b"] }
    simple.execute(what_to_execute)




if __name__ == '__main__':
    test_simple_interpreter()

