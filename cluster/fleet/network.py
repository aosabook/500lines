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

    def __init__(self, port):
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        self.sock.bind(('', port))
        self.address = tuple_to_addr(self.sock.getsockname())
        self.timers = []
        self.logger = logging.getLogger('node.%s' % (self.address,))
        self.unique_id = uuid.uuid3(NAMESPACE, self.address).int
        self.components = []

    def run(self):
        self.logger.debug("node starting")
        self.running = True
        while self.running:
            if self.timers:
                next_timer = self.timers[0][0]
                if next_timer < time.time():
                    when, do, callback = heapq.heappop(self.timers)
                    if do:
                        callback()
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
            for comp in self.components[:]:
                try:
                    fn = getattr(comp, 'do_%s' % action)
                except AttributeError:
                    continue
                fn(**kwargs)

    def kill(self):
        self.running = False

    def set_timer(self, seconds, callback):
        timer = [time.time() + seconds, True, callback]
        heapq.heappush(self.timers, timer)
        return timer

    def cancel_timer(self, timer):
        timer[1] = False

    def now(self):
        return time.time()

    def send(self, destinations, action, **kwargs):
        self.logger.debug("sending %s with args %s to %s" %
                          (action, kwargs, destinations))
        pkl = pickle.dumps((action, kwargs))
        for dest in destinations:
            self.sock.sendto(pkl, addr_to_tuple(dest))

    def register(self, component):
        self.components.append(component)

    def unregister(self, component):
        self.components.remove(component)
