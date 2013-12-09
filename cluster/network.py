import cPickle as pickle
import logging
import time
import heapq
import socket


def addr_to_tuple(addr):
    parts = addr.split('-')
    return parts[0], int(parts[1])


def tuple_to_addr(addr):
    if addr[0] == '0.0.0.0':
        addr = socket.gethostbyname(socket.gethostname()), addr[1]
    return '%s-%s' % addr


class Node(object):

    def __init__(self):
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        self.sock.bind(('', 0))
        self.address = tuple_to_addr(self.sock.getsockname())
        self.timers = []
        self.logger = logging.getLogger('node.%s' % (self.address,))

    def run(self):
        self.running = True
        while self.running:
            if self.timers:
                next_timer = self.timers[0][0]
                if next_timer < time.time():
                    when, do, method = heapq.heappop(self.timers)
                    if do:
                        self.logger.debug("timeout %r" % method)
                        getattr(self, 'timeout_%s' % method)()
                    continue
            else:
                next_timer = time.time() + 1
            self.sock.settimeout(next_timer - time.time())
            try:
                msg, address = self.sock.recvfrom(102400)
            except socket.timeout:
                continue
            action, kwargs = pickle.loads(msg)
            self.logger.debug("received %r with args %r" % (action, kwargs))
            getattr(self, 'do_%s' % action)(**kwargs)

    def stop(self):
        self.running = False

    def set_timer(self, seconds, method):
        timer = [time.time() + seconds, True, method]
        heapq.heappush(self.timers, timer)
        return timer

    def cancel_timer(self, timer):
        timer[1] = False

    def send(self, destinations, action, **kwargs):
        self.logger.debug("sending %s with args %s to %s" %
                          (action, kwargs, destinations))
        pkl = pickle.dumps((action, kwargs))
        for dest in destinations:
            self.sock.sendto(pkl, addr_to_tuple(dest))

# tests

import unittest
import threading


class TestNode(Node):
    foo_called = False
    bar_called = False

    def do_FOO(self, x, y):
        self.foo_called = True
        self.stop()

    def timeout_BAR(self):
        self.bar_called = True
        self.stop()


class NodeTests(unittest.TestCase):

    def test_comm(self):
        sender = Node()
        receiver = TestNode()
        rxthread = threading.Thread(target=receiver.run)
        rxthread.start()
        sender.send([receiver.address], 'FOO', x=10, y=20)
        rxthread.join()
        self.failUnless(receiver.foo_called)

    def test_timeout(self):
        node = TestNode()
        node.set_timer(0.01, 'BAR')
        node.run()
        self.failUnless(node.bar_called)

    def test_cancel_timeout(self):
        node = TestNode()
        nonex = node.set_timer(0.01, 'NONEXISTENT')
        node.set_timer(0.02, 'BAR')
        node.cancel_timer(nonex)
        node.run()
        # this just needs to not crash
