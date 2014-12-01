from cluster import *
import unittest
import itertools


class Tests(unittest.TestCase):

    def setUp(self):
        self.network = Network(1234)
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

    def setupNetwork(self, count, execute_fn=None):
        def add(state, input):
            state += input
            return state, state
        execute_fn = execute_fn or add
        peers = ['N%d' % n for n in range(count)]
        nodes = [self.addNode(p) for p in peers]
        Seed(nodes[0], initial_state=0, peers=peers, execute_fn=execute_fn)

        for node in nodes[1:]:
            bs = Bootstrap(node, execute_fn=execute_fn, peers=peers)
            bs.start()
        return nodes

    def kill(self, node):
        node.logger.warning("KILLED BY TESTS")
        del self.network.nodes[node.address]

    def test_two_requests(self):
        """Full run with non-overlapping requests succeeds."""
        nodes = self.setupNetwork(5)
        # set up some timers for various events
        def request_done(output):
            self.event("request done: %s" % output)
        def make_request(n, node):
            self.event("request: %s" % n)
            req = Requester(node, n, request_done)
            req.start()
        for time, callback in [
                (1.0, lambda: make_request(5, nodes[1])),
                (5.0, lambda: make_request(6, nodes[2])),
                (10.0, self.network.stop),
            ]:
            self.network.set_timer(None, time, callback)

        self.network.run()
        self.assertEvent(1001.0, 'request: 5')
        self.assertEvent(1002.0, 'request done: 5', fuzz=1)
        self.assertEvent(1005.0, 'request: 6')
        self.assertEvent(1005.0, 'request done: 11', fuzz=1)

    def test_parallel_requests(self):
        """Full run with parallel request succeeds."""
        N = 10
        nodes = self.setupNetwork(5)
        results = []
        for n in range(1, N+1):
            req = Requester(nodes[n % 4], n, results.append)
            self.network.set_timer(None, 1.0, req.start)

        self.network.set_timer(None, 10.0, self.network.stop)
        self.network.run()
        self.assertEqual((len(results), results and max(results)), (N, N*(N+1)/2),
                         "got %r" % (results,))

    def test_failed_nodes(self):
        """Full run with requests and some nodes dying midway through succeeds"""
        N = 10
        nodes = self.setupNetwork(7)
        results = []
        for n in range(1, N+1):
            req = Requester(nodes[n % 3], n, results.append)
            self.network.set_timer(None, n+1, req.start)

        # kill nodes 3 and 4 at N/2 seconds
        self.network.set_timer(None, N/2-1, lambda: self.kill(nodes[3]))
        self.network.set_timer(None, N/2, lambda: self.kill(nodes[4]))

        self.network.set_timer(None, N * 3.0, self.network.stop)
        self.network.run()
        self.assertEqual((len(results), results and max(results)), (N, N*(N+1)/2),
                         "got %r" % (results,))

    def test_failed_leader(self):
        """Full run with requests and a dying leader succeeds."""
        N = 10
        # use a bit-setting function so that we can easily ignore requests made
        # by the failed node
        def identity(state, input):
            return state, input
        nodes = self.setupNetwork(7, execute_fn=identity)
        results = []
        for n in range(1, N+1):
            req = Requester(nodes[n % 6], n, results.append)
            self.network.set_timer(None, n+1, req.start)

        # kill the leader node at N/2 seconds (it should be stable by then).  Some of the
        # Requester roles were attached to this node, so we fake success of those requests
        # since we don't know what state they're in right now.
        def is_leader(n):
            try:
                leader_role = [c for c in n.roles if isinstance(c, Leader)][0]
                return leader_role.active
            except IndexError:
                return False
        def kill_leader():
            active_leader_nodes = [n for n in nodes if is_leader(n)]
            if active_leader_nodes:
                active_leader = active_leader_nodes[0]
                active_idx = nodes.index(active_leader)
                # append the N's that this node was requesting
                for n in range(1, N+1):
                    if n % 6 == active_idx:
                        results.append(n)
                self.kill(active_leader)
        self.network.set_timer(None, N/2, kill_leader)

        self.network.set_timer(None, 15, self.network.stop)
        self.network.run()
        self.assertEqual(set(results), set(xrange(1, N+1)))
