from fleet import *
import mock
import unittest


class TestComp(Component):
    join_called = False

    def do_JOIN(self, sender):
        self.join_called = True
        self.node.kill()


# TODO: lots more tests

class NodeTests(unittest.TestCase):

    def setUp(self):
        self.network = Network(1234)

    def test_comm(self):
        """Node can successfully send a message between instances"""
        sender = self.network.new_node('S')
        receiver = self.network.new_node('R')
        comp = TestComp(receiver)
        sender.send([receiver.address], Join())
        self.network.run(realtime=False)
        self.failUnless(comp.join_called)

    def test_timeout(self):
        """Node's timeouts trigger at the appropriate time"""
        node = self.network.new_node('T')

        cb = mock.Mock(side_effect=node.kill)
        node.set_timer(0.01, cb)
        self.network.run(realtime=False)
        self.failUnless(cb.called)

    def test_cancel_timeout(self):
        """Node's timeouts do not occur if they are cancelled."""
        node = self.network.new_node('C')

        def fail():
            raise RuntimeError("nooo")

        nonex = node.set_timer(0.01, fail)

        cb = mock.Mock(side_effect=node.kill)
        node.set_timer(0.02, cb)
        nonex.cancel()
        self.network.run()
        self.failUnless(cb.called)
