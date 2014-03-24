from . import fake_network
from .. import member
import unittest


class ComponentTestCase(unittest.TestCase):

    def setUp(self):
        self.node = fake_network.FakeNode()
        self.member = member.Member(self.node)

    def tearDown(self):
        if self.node.sent:
            self.fail("extra messages from node: %r" % (self.node.sent,))

    def assertMessage(self, destinations, action, **kwargs):
        got = self.node.sent.pop(0)
        self.assertEqual((sorted(got[0]), got[1], got[2]), (sorted(destinations), action, kwargs))
