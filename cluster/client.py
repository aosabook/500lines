import sys
import logging
from network import Node


class Client(Node):

    def __init__(self, member_address):
        super(Client, self).__init__()
        self.member_address = member_address

    def invoke(self, input):
        self.output = None
        self.send([self.member_address], 'INVOKE',
                  input=input, caller=self.address)
        self.run()
        return self.output

    def do_INVOKED(self, output):
        self.logger.debug("received output %r" % (output,))
        self.output = output
        self.stop()

if __name__ == "__main__":
    logging.basicConfig(
        format="%(asctime)s %(name)s %(message)s", level=logging.DEBUG)
    client = Client(sys.argv[1])
    print client.invoke(4)
    print client.invoke(1)

# tests

import unittest
import threading


class FakeMember(Node):

    def do_INVOKE(self, caller, input):
        self.send([caller], 'INVOKED', output=input * 10)
        self.stop()


class ClientTests(unittest.TestCase):

    def test_invoke(self):
        member = FakeMember()
        client = Client(member.address)
        memberthd = threading.Thread(target=member.run)
        memberthd.start()
        self.assertEqual(client.invoke(5), 50)
        memberthd.join()
