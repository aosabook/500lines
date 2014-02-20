import cPickle as pickle
import uuid
import logging
import time
import heapq
import socket

NAMESPACE = uuid.UUID('7e0d7720-fa98-4270-94ff-650a2c25f3f0')

def addr_to_tuple(addr):
    parts = addr.split('-')
    return parts[0], int(parts[1])


def tuple_to_addr(addr):
    if addr[0] == '0.0.0.0':
        addr = socket.gethostbyname(socket.gethostname()), addr[1]
    return '%s-%s' % addr


class Node(object):

    port = 0  # XXX temporary
     
    def __init__(self):
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        self.sock.bind(('', self.port))  # XXX temporary
        self.address = tuple_to_addr(self.sock.getsockname())
        self.timers = []
        self.logger = logging.getLogger('node.%s' % (self.address,))
        self.unique_id = uuid.uuid3(NAMESPACE, self.address).int
        # XXX temporary
        self.unique_id = self.sock.getsockname()[1]

    def start(self):
        # subclasses can override this
        pass

    def run(self):
        self.start()
        self.running = True
        while self.running:
            if self.timers:
                next_timer = self.timers[0][0]
                if next_timer < time.time():
                    when, do, callable = heapq.heappop(self.timers)
                    if do:
                        callable()
                    continue
            else:
                next_timer = 0
            timeout = max(0.1, next_timer - time.time())
            self.sock.settimeout(timeout)
            try:
                msg, address = self.sock.recvfrom(102400)
            except socket.timeout:
                continue
            action, kwargs = pickle.loads(msg)
            self.logger.debug("received %r with args %r" % (action, kwargs))
            getattr(self, 'do_%s' % action)(**kwargs)

    def stop(self):
        self.running = False

    def set_timer(self, seconds, callable):
        timer = [time.time() + seconds, True, callable]
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
        def cb():
            node.bar_called = True
            node.stop()
        node.set_timer(0.01, cb)
        node.run()
        self.failUnless(node.bar_called)

    def test_cancel_timeout(self):
        node = TestNode()
        def fail():
            raise RuntimeError("nooo")
        nonex = node.set_timer(0.01, fail)
        def cb():
            node.bar_called = True
            node.stop()
        node.set_timer(0.02, cb)
        node.cancel_timer(nonex)
        node.run()
        # this just needs to not crash
