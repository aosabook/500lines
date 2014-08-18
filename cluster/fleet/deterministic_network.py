import copy
import functools
import time
import logging
import heapq
import random


class Node(object):
    # # TODO: consider using itertools.count() instead of iter(xrange(1000)) - 1000 seems ot be a magic number
    unique_ids = iter(xrange(1000))

    def __init__(self, network):
        self.network = network
        self.unique_id = next(self.unique_ids)
        self.address = 'N%d' % self.unique_id
        self.components = []
        self.logger = logging.getLogger(self.address)
        self.logger.info('starting')
        self.now = self.network.now

    def kill(self):
        self.logger.error('node dying')
        if self.address in self.network.nodes:
            del self.network.nodes[self.address]

    def set_timer(self, seconds, callback):
        return self.network.set_timer(seconds, self.address, callback)

    def cancel_timer(self, timer):
        self.network.cancel_timer(timer)

    def send(self, destinations, action, **kwargs):
        self.logger.debug("sending %s with args %s to %s", action, kwargs, destinations)
        self.network.send(destinations, action, **kwargs)

    def register(self, component):
        self.components.append(component)

    def unregister(self, component):
        self.components.remove(component)

    def receive(self, action, kwargs):
        action_handler_name = 'do_%s' % action

        for comp in self.components[:]:
            if not hasattr(comp, action_handler_name):
                continue
            comp.logger.debug("received %r with args %r", action, kwargs)
            fn = getattr(comp, action_handler_name)
            fn(**kwargs)


class Network(object):
    PROP_DELAY = 0.03
    PROP_JITTER = 0.02
    DROP_PROB = 0.05

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
            when, do, address, callback = heapq.heappop(self.timers)
            if do and address in self.nodes:
                callback()

    def stop(self):
        self.timers = []

    def set_timer(self, seconds, address, callback):
        # TODO: return an obj with 'cancel'
        timer = [self.now + seconds, True, address, callback]
        heapq.heappush(self.timers, timer)
        return timer

    def cancel_timer(self, timer):
        timer[1] = False

    def _receive(self, address, action, kwargs):
        if address in self.nodes:
            self.nodes[address].receive(action, kwargs)

    def send(self, destinations, action, **kwargs):
        for dest in destinations:
            if self.rnd.uniform(0, 1.0) > self.DROP_PROB:
                delay = self.PROP_DELAY + self.rnd.uniform(-self.PROP_JITTER, self.PROP_JITTER)
                # copy the kwargs now, before the sender modifies them
                self.set_timer(delay, dest, functools.partial(self._receive, dest, action, copy.deepcopy(kwargs)))
