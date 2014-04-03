from .. import network
from .. import member
from . import utils
import mock
import unittest
import threading


class TestComp(member.Component):
    foo_called = False

    def do_FOO(self, x, y):
        self.foo_called = True
        self.member.node.kill()


class NodeTests(unittest.TestCase):

    def test_comm(self):
        sender = network.Node(0)
        receiver = network.Node(0)
        memb = member.Member(receiver)
        comp = TestComp(memb)
        rxthread = threading.Thread(target=receiver.run)
        rxthread.start()
        sender.send([receiver.address], 'FOO', x=10, y=20)
        rxthread.join()
        self.failUnless(comp.foo_called)

    def test_timeout(self):
        node = network.Node(0)

        cb = mock.Mock(side_effect=node.kill)
        node.set_timer(0.01, cb)
        node.run()
        self.failUnless(cb.called)

    def test_cancel_timeout(self):
        node = network.Node(0)

        def fail():
            raise RuntimeError("nooo")

        nonex = node.set_timer(0.01, fail)

        cb = mock.Mock(side_effect=node.kill)
        node.set_timer(0.02, cb)
        node.cancel_timer(nonex)
        node.run()
        self.failUnless(cb.called)
