from cluster import *
import logging
import heapq

class FakeNetwork(Network):

    def __init__(self):
        super(FakeNetwork, self).__init__(1234)
        self.now = 0.0
        self.node = None
        self.ran = False

    def new_node(self, address=None):
        assert not self.node, "FakeNetwork only runs one node"
        self.node = FakeNode(self)
        return self.node

    def run(self):
        self.ran = True

    def tick(self, seconds):
        until = self.now + seconds
        self.timers.sort()
        while self.timers and self.timers[0].expires <= until:
            timer = self.timers.pop(0)
            self.now = timer.expires
            if not timer.cancelled:
                timer.callback()
        self.now = until

    def send(self, sender, destinations, message):
        sender.sent.append((destinations, message))

    def get_times(self):
        return sorted([t.expires - self.now for t in self.timers if not t.cancelled])

class FakeNode(Node):

    def __init__(self, network=None):
        network = network or FakeNetwork()
        super(FakeNode, self).__init__(network, 'F999')
        self.unique_id = 999
        self.sent = []
        self.events = []
        self.logger = logging.getLogger('node.%s' % (self.address,))

    def register(self, component):
        assert component not in self.components
        super(FakeNode, self).register(component)

    def unregister(self, component):
        assert component in self.components
        super(FakeNode, self).unregister(component)

    def fake_message(self, message, sender='F999'):
        for component in self.components:
            fn = getattr(component, 'do_%s' % type(message).__name__.upper())
            fn(sender=sender, **message._asdict())

    def fakeEvent(self, message, **kwargs):
        super(FakeNode, self).event(message, **kwargs)

    def event(self, message, **kwargs):
        self.events.append((message, kwargs))
