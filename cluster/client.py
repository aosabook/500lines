import time
import sys
import logging
from deterministic_network import Node


class Client(Node):

    def __init__(self, member_address):
        super(Client, self).__init__()
        self.member_address = member_address
        self.cid = 1000000
        self.current_request = None

    def start(self):
        def re_invoke(n):
            if n < 10:
                self.invoke(n, lambda output: re_invoke(n+1))
        self.set_timer(1, lambda: re_invoke(1))

    def invoke(self, input, callback):
        assert self.current_request is None
        self.output = None
        self.current_request = (self.cid, input, callback)
        self.cid += 1
        self.send_invoke()
        return self.output

    def send_invoke(self):
        cid, input, callback = self.current_request
        self.send([self.member_address], 'INVOKE',
                  caller=self.address, cid=cid, input=input)
        self.invoke_timer = self.set_timer(3, self.send_invoke)

    def do_INVOKED(self, cid, output):
        if not self.current_request or cid != self.current_request[0]:
            return
        self.logger.debug("received output %r" % (output,))
        callback = self.current_request[2]
        self.current_request = None
        self.cancel_timer(self.invoke_timer)
        callback(output)

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

    def do_INVOKE(self, caller, cid, input):
        self.send([caller], 'INVOKED', cid=cid, output=input * 10)
        self.stop()


class ClientTests(unittest.TestCase):

    def test_invoke(self):
        member = FakeMember()
        client = Client(member.address)
        memberthd = threading.Thread(target=member.run)
        memberthd.start()
        self.assertEqual(client.invoke(5), 50)
        memberthd.join()
