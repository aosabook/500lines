import time
import logging
import heapq
import random

class Node(object):

    unique_ids = xrange(1000).__iter__()

    def __init__(self, network):
        self.network = network
        self.unique_id = self.unique_ids.next()
        self.address = 'N%d' % self.unique_id
        self.components = []
        self.logger = logging.getLogger(self.address)
        self.logger.info('starting')

    def kill(self):
        self.logger.error('node dying')
        if self.address in self.network.nodes:
            del self.network.nodes[self.address]

    def set_timer(self, seconds, callable):
        return self.network.set_timer(seconds, self.address, callable)

    def cancel_timer(self, timer):
        self.network.cancel_timer(timer)

    def send(self, destinations, action, **kwargs):
        self.logger.debug("sending %s with args %s to %s" %
                          (action, kwargs, destinations))
        self.network.send(destinations, action, **kwargs)

    def register(self, component):
        self.components.append(component)

    def unregister(self, component):
        self.components.remove(component)

    def receive(self, action, kwargs):
        import sys
        for comp in self.components[:]:
            try:
                fn = getattr(comp, 'do_%s' % action)
            except AttributeError:
                continue
            comp.logger.debug("received %r with args %r" % (action, kwargs))
            fn(**kwargs)


class Network(object):

    PROP_DELAY = 0.03
    PROP_JITTER = 0.02

    def __init__(self, seed, pause=False):
        self.nodes = {}
        self.rnd = random.Random(seed)
        self.pause = pause
        self.timers = []
        self.now = 1000.0
        self.logger = logging.getLogger('network')

    def new_node(self):
        node = Node(self)
        self.nodes[node.address] = node
        return node

    def run(self):
        while self.timers:
            next_timer = self.timers[0][0]
            if next_timer > self.now:
                if self.pause:
                    raw_input()
                else:
                    time.sleep(next_timer - self.now)
                self.now = next_timer
            when, do, address, callable = heapq.heappop(self.timers)
            if do and address in self.nodes:
                callable()

    def stop(self):
        self.timers = []

    def set_timer(self, seconds, address, callable):
        # TODO: return an obj with 'cancel'
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
        node.receive(action, kwargs)

    def send(self, destinations, action, **kwargs):
        for dest in destinations:
            delay = self.PROP_DELAY + self.rnd.uniform(-self.PROP_JITTER, self.PROP_JITTER)
            self.set_timer(delay, dest, lambda dest=dest: self._receive(dest, action, kwargs))

