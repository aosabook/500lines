from . import fake_network
import unittest


class ComponentTestCase(unittest.TestCase):

    def setUp(self):
        self.network = fake_network.FakeNetwork()
        self.node = fake_network.FakeNode(self.network)

        self.fakeEvent = self.node.fakeEvent

    def tearDown(self):
        if self.node.sent:
            self.fail("extra messages from node: %r" % (self.node.sent,))

    def assertMessage(self, destinations, message):
        got = self.node.sent.pop(0)
        self.assertEqual((sorted(got[0]), got[1]),
                         (sorted(destinations), message))

    def assertEvent(self, msg, **kwargs):
        got = self.node.events.pop(0)
        self.assertEqual(got, (msg, kwargs))

    def assertNoMessages(self):
        self.assertEqual(self.node.sent, [])

    def assertNoEvents(self):
        self.assertEqual(self.node.events, [])

    def assertTimers(self, times):
        self.assertEqual(self.node.network.get_times(), times)

    def assertUnregistered(self):
        self.assertEqual(self.node.components, [])
