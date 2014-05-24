from .. import network
from .. import member
from .. import Join
import mock
import unittest
import threading


class TestComp(member.Component):
    join_called = False

    def do_JOIN(self, sender):
        self.join_called = True
        self.member.node.kill()


class NodeTests(unittest.TestCase):

    def setUp(self):
        self.patch = mock.patch('fleet.network.tuple_to_addr')
        m = self.patch.start()
        m.side_effect = lambda addr: '%s-%s' % addr

    def tearDown(self):
        self.patch.stop()

    def test_comm(self):
        """Node can successfully send a message between instances"""
        sender = network.Node(0)
        receiver = network.Node(0)
        memb = member.Member(receiver)
        comp = TestComp(memb)
        rxthread = threading.Thread(target=receiver.run)
        rxthread.start()
        sender.send([receiver.address], Join())
        rxthread.join()
        self.failUnless(comp.join_called)

    def test_timeout(self):
        """Node's timeouts trigger at the appropriate time"""
        node = network.Node(0)

        cb = mock.Mock(side_effect=node.kill)
        node.set_timer(0.01, cb)
        node.run()
        self.failUnless(cb.called)

    def test_cancel_timeout(self):
        """Node's timeouts do not occur if they are cancelled."""
        node = network.Node(0)

        def fail():
            raise RuntimeError("nooo")

        nonex = node.set_timer(0.01, fail)

        cb = mock.Mock(side_effect=node.kill)
        node.set_timer(0.02, cb)
        nonex.cancel()
        node.run()
        self.failUnless(cb.called)
