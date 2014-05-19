import copy
import functools
import time
import logging
import heapq
import random
import itertools


class NetworkLogger(logging.LoggerAdapter):

    def process(self, msg, kwargs):
        return "T=%.3f %s" % (self.extra['network'].now, msg), kwargs

    def getChild(self, name):
        return self.__class__(self.logger.getChild(name), {'network': self.extra['network']})


class Node(object):
    unique_ids = itertools.count()

    def __init__(self, network, address):
        self.network = network
        self.unique_id = next(self.unique_ids)
        self.address = address or 'N%d' % self.unique_id
        self.components = []
        self.logger = NetworkLogger(logging.getLogger(self.address), {'network': self.network})
        self.logger.info('starting')

    def kill(self):
        self.logger.error('node dying')
        if self.address in self.network.nodes:
            del self.network.nodes[self.address]

    def set_timer(self, seconds, callback):
        return self.network.set_timer(seconds, self.address, callback)

    def send(self, destinations, message):
        self.logger.debug("sending %s to %s",
                          message, destinations)
        self.network.send(destinations, message)

    def register(self, component):
        self.components.append(component)

    def unregister(self, component):
        self.components.remove(component)

    def receive(self, message):
        handler_name = 'do_%s' % type(message).__name__.upper()

        for comp in self.components[:]:
            if not hasattr(comp, handler_name):
                continue
            comp.logger.debug("received %s", message)
            fn = getattr(comp, handler_name)
            fn(**message._asdict())


class Timer(object):

    def __init__(self, expires, address, callback):
        self.expires = expires
        self.address = address
        self.callback = callback
        self.cancelled = False

    def __cmp__(self, other):
        return cmp(self.expires, other.expires)

    def cancel(self):
        self.cancelled = True


class Network(object):
    PROP_DELAY = 0.03
    PROP_JITTER = 0.02
    DROP_PROB = 0.05

    def __init__(self, seed):
        self.nodes = {}
        self.rnd = random.Random(seed)
        self.timers = []
        self.now = 1000.0

    def new_node(self, address=None):
        node = Node(self, address=address)
        self.nodes[node.address] = node
        return node

    def run(self, pause=False, realtime=True):
        while self.timers:
            next_timer = self.timers[0]
            if next_timer.expires > self.now:
                if pause:
                    raw_input()
                elif realtime:
                    time.sleep(next_timer.expires - self.now)
                self.now = next_timer.expires
            heapq.heappop(self.timers)
            if next_timer.cancelled:
                continue
            if not next_timer.address or next_timer.address in self.nodes:
                next_timer.callback()

    def stop(self):
        self.timers = []

    def set_timer(self, seconds, address, callback):
        timer = Timer(self.now + seconds, address, callback)
        heapq.heappush(self.timers, timer)
        return timer

    def send(self, destinations, message):
        def _receive(address, message):
            if address in self.nodes:
                self.nodes[address].receive(message)
        for dest in destinations:
            if self.rnd.uniform(0, 1.0) > self.DROP_PROB:
                delay = self.PROP_DELAY + \
                    self.rnd.uniform(-self.PROP_JITTER, self.PROP_JITTER)
                self.set_timer(delay, dest, functools.partial(_receive, dest, message))
