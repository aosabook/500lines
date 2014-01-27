class Member(object):

    def __init__(self, execute_fn):
        self.execute_fn = execute_fn

    def start(self, initial_value=None):
        self.state = initial_value

    def invoke(self, input):
        self.state, output = self.execute_fn(self.state, input)
        return output


class Client(object):

    def __init__(self, member):
        self.member = member

    def invoke(self, input):
        return self.member.invoke(input)


def sequence_generator(state, input):
    return state + input, range(state, state + input)


if __name__ == "__main__":
    member = Member(sequence_generator)
    member.start(initial_value=0)
    print Client(member).invoke(4)
    print Client(member).invoke(1)
