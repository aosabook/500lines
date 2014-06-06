from cluster import *
import logging
import heapq

class FakeNetwork(object):

    # TODO: inherit from "real" Network

    def __init__(self):
        self.now = 0.0
        self.node = None
        self.ran = False

    def new_node(self, address=None):
        assert not self.node, "FakeNetwork only runs one node"
        self.node = FakeNode(self)
        return self.node

    def run(self):
        self.ran = True

class FakeNode(Node):

    def __init__(self, network=None):
        network = network or FakeNetwork()
        super(FakeNode, self).__init__(network, 'F999')
        self.unique_id = 999
        self.timers = []
        self.sent = []
        self.events = []
        self.logger = logging.getLogger('node.%s' % (self.address,))

    def register(self, component):
        assert component not in self.components
        super(FakeNode, self).register(component)

    def unregister(self, component):
        assert component in self.components
        super(FakeNode, self).unregister(component)

    def set_timer(self, seconds, callback):
        timer = Timer(self.network.now + seconds, self.address, callback)
        heapq.heappush(self.timers, timer)
        return timer

    def tick(self, seconds):
        until = self.network.now + seconds
        self.timers.sort()
        while self.timers and self.timers[0].expires <= until:
            timer = self.timers.pop(0)
            self.network.now = timer.expires
            if not timer.cancelled:
                timer.callback()
        self.network.now = until

    def get_times(self):
        return sorted([t.expires - self.network.now for t in self.timers if not t.cancelled])

    def send(self, destinations, message):
        self.sent.append((destinations, message))

    def fake_message(self, message, sender='F999'):
        for component in self.components:
            fn = getattr(component, 'do_%s' % type(message).__name__.upper())
            fn(sender=sender, **message._asdict())

    def fakeEvent(self, message, **kwargs):
        super(FakeNode, self).event(message, **kwargs)

    def event(self, message, **kwargs):
        self.events.append((message, kwargs))
