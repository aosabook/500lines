import sys
import logging
from network import Node

# Remove from final copy:
#  - logging


class Member(Node):

    def __init__(self, execute_fn):
        Node.__init__(self)
        self.execute_fn = execute_fn

    def start(self, initial_value=None):
        self.state = initial_value
        self.run()

    def invoke(self, input):
        self.state, output = self.execute_fn(self.state, input)
        return output

    def do_INVOKE(self, input, caller):
        self.send([caller], 'INVOKED', output=self.invoke(input))


def sequence_generator(state, input):
    return state + input, range(state, state + input)


if __name__ == "__main__":
    logging.basicConfig(
        format="%(asctime)s %(name)s %(message)s", level=logging.DEBUG)
    member = Member(sequence_generator)
    print member.address
    member.start(initial_value=0)

# tests

import unittest
import threading


class FakeClient(Node):

    def do_INVOKED(self, output):
        self.output = output
        self.stop()


class MemberTests(unittest.TestCase):

    def test_invoke(self):
        member = Member(sequence_generator)
        client = FakeClient()
        client.member = member
        memberthd = threading.Thread(target=member.start, args=(0,))
        memberthd.daemon = 1
        memberthd.start()
        client.send([member.address], 'INVOKE', input=5, caller=client.address)
        client.run()
        self.assertEqual(client.output, [0, 1, 2, 3, 4])
