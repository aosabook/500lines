import uuid
import logging
import heapq
import random

class Node(object):

    unique_ids = xrange(1000).__iter__()

    def __init__(self):
        self.unique_id = self.unique_ids.next()

    def set_up_node(self, address, core):
        self.core = core
        self.address = address
        self.core.nodes[self.address] = self
        self.logger = logging.getLogger('node.%s' % (self.address,))

    def stop(self):
        if self.address in self.core.nodes:
            del self.core.nodes[self.address]

    def start(self):
        pass

    def set_timer(self, seconds, callable):
        # TODO: refactor so this won't call a stopped node
        return self.core.set_timer(seconds, self.address, callable)

    def cancel_timer(self, timer):
        self.core.cancel_timer(timer)

    def send(self, destinations, action, **kwargs):
        self.logger.debug("sending %s with args %s to %s" %
                          (action, kwargs, destinations))
        self.core.send(destinations, action, **kwargs)


class Core(object):

    PROP_DELAY = 0.03
    PROP_JITTER = 0.02

    def __init__(self, seed):
        self.nodes = {}
        self.rnd = random.Random(seed)
        self.timers = []
        self.now = 1000.0
        self.logger = logging.getLogger('core')

    def run(self):
        for node in sorted(self.nodes.values()):
            node.start()
        while self.timers:
            next_timer = self.timers[0][0]
            if next_timer > self.now:
                self.now = next_timer
            when, do, address, callable = heapq.heappop(self.timers)
            if do and address in self.nodes:
                callable()

    def stop(self):
        self.timers = []

    def set_timer(self, seconds, address, callable):
        timer = [self.now + seconds, True, address, callable]
        heapq.heappush(self.timers, timer)
        return timer

    def cancel_timer(self, timer):
        timer[1] = False

    def _receive(self, address, action, kwargs):
        try:
            node = self.nodes[address]
        except KeyError:
            return
        try:
            fn = getattr(node, 'do_%s' % action)
        except AttributeError:
            return

        node.logger.debug("received %r with args %r" % (action, kwargs))
        fn(**kwargs)

    def send(self, destinations, action, **kwargs):
        for dest in destinations:
            delay = self.PROP_DELAY + self.rnd.uniform(-self.PROP_JITTER, self.PROP_JITTER)
            self.set_timer(delay, dest, lambda dest=dest: self._receive(dest, action, kwargs))

