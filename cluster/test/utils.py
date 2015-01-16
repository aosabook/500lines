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
        self.assertEqual(self.node.roles, [])

    def verifyAcceptedProposals(self, accepted_proposals):
        """Verify that the ``accepted_proposals`` field of a promise is formatted
        as a dictionary mapping slots to (ballot, proposal) tuples."""
        self.assertIsInstance(accepted_proposals, dict)
        for k, v in accepted_proposals.iteritems():
            self.assertIsInstance(k, int)
            self.assertIsInstance(v, tuple)
            self.assertEqual(len(v), 2)
            self.assertIsInstance(v[0], Ballot)
            self.assertIsInstance(v[1], Proposal)
