from .. import deterministic_network
from .. import member_replicated
from .. import request
from nose.tools import eq_
import unittest
import itertools
import logging
import sys


class Tests(unittest.TestCase):

    def setUp(self):
        self.network = deterministic_network.Network(1234)
        self.addresses = ('node-%d' % d for d in itertools.count())
        self.nodes = []
        self.events = []

    def tearDown(self):
        if self.events:
            self.fail("unhandled events: %r" % (self.events,))

    def event(self, name):
        self.events.append((self.network.now, name))

    def addNode(self, address):
        node = self.network.new_node(address=address)
        self.nodes.append(node)
        return node

    def assertEvent(self, time, name, fuzz=0):
        for i, e in enumerate(self.events):
            if e[1] == name and time - fuzz <= e[0] <= time + fuzz:
                self.events.pop(i)
                return
        self.fail("event %r not found at or around time %f; events: %r" % (name, time, self.events))

    def setupNetwork(self, count):
        def add(state, input):
            state += input
            return state, state
        peers = ['N%d' % n for n in range(count+1)]
        nodes = [self.addNode(p) for p in peers]
        # TODO: replace seed with a node when it quits
        seed = member_replicated.ClusterSeed(nodes.pop(0),
                                             initial_state=0,
                                             peers=peers)
        seed.start()

        members = []
        for node in nodes:
            member = member_replicated.ClusterMember(node,
                                                     execute_fn=add,
                                                     peers=peers)
            member.start()
            members.append(member)
        return members

    def test_two_requests(self):
        """A full run of the protocol with four nodes + a seed, and two
        requests, succeeds in agreeing on those requests."""
        members = self.setupNetwork(4)
        # set up some timers for various events
        def request_done(output):
            self.event("request done: %s" % output)
        def make_request(n, m):
            self.event("request: %s" % n)
            req = request.Request(m, n, request_done)
            req.start()
        for time, callback in [
                (1.0, lambda: make_request(5, members[1])),
                (5.0, lambda: make_request(6, members[2])),
                (10.0, self.network.stop),
            ]:
            self.network.set_timer(time, None, callback)

        self.network.run(realtime=False)
        self.assertEvent(1001.0, 'request: 5')
        self.assertEvent(1002.0, 'request done: 5', fuzz=1)
        self.assertEvent(1005.0, 'request: 6')
        self.assertEvent(1005.0, 'request done: 11', fuzz=1)

    def test_parallel_requests(self):
        """A full run of the protocol with ten parallel requests to different
        nodes executes each exactly once (regardless of what order they occur
        or are reported in), within 10 simulated seconds."""
        N = 10
        members = self.setupNetwork(4)
        results = []
        def request_done(output):
            results.append(output)
        for n in range(1, N+1):
            req = request.Request(members[n % 4], n, results.append)
            self.network.set_timer(1.0, None, req.start)

        self.network.set_timer(10.0, None, self.network.stop)
        self.network.run(realtime=False)
        self.assertEqual((len(results), results and max(results)), (N, N*(N+1)/2),
                         "got %r" % (results,))

    def test_dead_nodes(self):
        """A full run of the protocol with a request every second succeeds even
        when a minority of the nodes fail midway through"""
        N = 10
        members = self.setupNetwork(6)  # TODO: really a 7-node cluster, w/ node0=seed
        results = []
        def request_done(output):
            results.append(output)
        for n in range(1, N+1):
            req = request.Request(members[n % 3], n, results.append)
            self.network.set_timer(n+1, None, req.start)

        # kill nodes 3 and 4 at N/2 seconds
        self.network.set_timer(N/2-1, None, members[3].node.kill)
        self.network.set_timer(N/2, None, members[4].node.kill)

        self.network.set_timer(N * 3.0, None, self.network.stop)
        self.network.run(realtime=False)
        self.assertEqual((len(results), results and max(results)), (N, N*(N+1)/2),
                         "got %r" % (results,))
