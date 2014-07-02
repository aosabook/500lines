from cluster import *
from . import fake_network
import unittest


class ComponentTestCase(unittest.TestCase):

    def setUp(self):
        self.network = fake_network.FakeNetwork()
        self.node = fake_network.FakeNode(self.network)

    def tearDown(self):
        if self.node.sent:
            self.fail("extra messages from node: %r" % (self.node.sent,))

    def assertMessage(self, destinations, message):
        got = self.node.sent.pop(0)
        self.assertEqual((sorted(got[0]), got[1]),
                         (sorted(destinations), message))

    def assertNoMessages(self):
        self.assertEqual(self.node.sent, [])

    def assertTimers(self, times):
        self.assertEqual(self.node.network.get_times(), times)

    def assertUnregistered(self):
        self.assertEqual(self.node.components, [])

    def verifyPromiseAccepted(self, accepted):
        """Verify that the ``accepted`` field of a promise is formatted
        as a dictionary mapping Ballots to Proposals."""
        self.assertIsInstance(accepted, dict)
        for k, v in accepted.iteritems():
            self.assertIsInstance(k, tuple)
            self.assertEqual(len(k), 2)
            self.assertIsInstance(k[0], Ballot)
            self.assertIsInstance(k[1], int)
            self.assertIsInstance(v, Proposal)
