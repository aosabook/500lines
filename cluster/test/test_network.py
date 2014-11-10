from cluster import *
import mock
import unittest


class TestComp(Role):
    join_called = False

    def do_Join(self, sender):
        self.join_called = True
        self.kill()


class NetworkTests(unittest.TestCase):

    def setUp(self):
        self.network = Network(1234)

    def kill(self, node):
        del self.network.nodes[node.address]

    def test_comm(self):
        """Node can successfully send a message between instances"""
        sender = self.network.new_node('S')
        receiver = self.network.new_node('R')
        comp = TestComp(receiver)
        comp.kill = lambda: self.kill(receiver)
        sender.send([receiver.address], Join())
        self.network.run()
        self.failUnless(comp.join_called)

    def test_timeout(self):
        """Node's timeouts trigger at the appropriate time"""
        node = self.network.new_node('T')

        cb = mock.Mock(side_effect=lambda: self.kill(node))
        self.network.set_timer(node.address, 0.01, cb)
        self.network.run()
        self.failUnless(cb.called)

    def test_cancel_timeout(self):
        """Node's timeouts do not occur if they are cancelled."""
        node = self.network.new_node('C')

        def fail():
            raise RuntimeError("nooo")

        nonex = self.network.set_timer(node.address, 0.01, fail)

        cb = mock.Mock(side_effect=lambda: self.kill(node))
        self.network.set_timer(node.address, 0.02, cb)
        nonex.cancel()
        self.network.run()
        self.failUnless(cb.called)
