from . import fake_network
from .. import member
import unittest


class ComponentTestCase(unittest.TestCase):

    def setUp(self):
        self.node = fake_network.FakeNode()
        self.member = member.Member(self.node)

        self.events = []
        self.fakeEvent = self.member.event
        self.member.event = lambda msg, **kwargs: self.events.append((msg, kwargs))

    def tearDown(self):
        if self.node.sent:
            self.fail("extra messages from node: %r" % (self.node.sent,))

    def assertMessage(self, destinations, message):
        got = self.node.sent.pop(0)
        self.assertEqual((sorted(got[0]), got[1]),
                         (sorted(destinations), message))

    def assertEvent(self, msg, **kwargs):
        got = self.events.pop(0)
        self.assertEqual(got, (msg, kwargs))

    def assertNoMessages(self):
        self.assertEqual(self.node.sent, [])

    def assertNoEvents(self):
        self.assertEqual(self.events, [])

    def assertTimers(self, times):
        self.assertEqual(self.node.get_times(), times)

    def assertUnregistered(self):
        self.assertEqual(self.node.components, [])
