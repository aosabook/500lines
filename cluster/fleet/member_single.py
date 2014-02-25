import logging
from network import Node
from statemachine import sequence_generator

# Remove from final copy:
#  - logging


class Member(Node):

    def __init__(self):
        Node.__init__(self)

    def start(self, execute_fn, initial_value=None):
        self.execute_fn = execute_fn
        self.state = initial_value
        self.run()

    def invoke(self, input):
        self.state, output = self.execute_fn(self.state, input)
        return output

    def do_INVOKE(self, input, cid, caller):
        self.send([caller], 'INVOKED', output=self.invoke(input))


if __name__ == "__main__":
    logging.basicConfig(
        format="%(asctime)s %(name)s %(message)s", level=logging.DEBUG)
    member = Member()
    print member.address
    member.start(sequence_generator, initial_value=0)

# tests

import unittest
import threading


class FakeClient(Node):

    def do_INVOKED(self, output):
        self.output = output
        self.stop()


class MemberTests(unittest.TestCase):

    def test_invoke(self):
        member = Member()
        client = FakeClient()
        client.member = member
        memberthd = threading.Thread(target=member.start, args=(sequence_generator, 0,))
        memberthd.daemon = 1
        memberthd.start()
        client.send([member.address], 'INVOKE', input=5, caller=client.address)
        client.run()
        self.assertEqual(client.output, [0, 1, 2, 3, 4])
