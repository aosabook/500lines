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
                next_timer = self.timers[0]
                if next_timer.expires < time.time():
                    heapq.heappop(self.timers)
                    if not next_timer.cancelled:
                        next_timer.callback()
                    continue
            else:
                next_timer = None
            if next_timer:
                timeout = max(0.1, next_timer.expires - time.time())
            else:
                timeout = 0.1
            self.sock.settimeout(timeout)
            try:
                msg, address = self.sock.recvfrom(102400)
            except socket.timeout:
                continue
            message = pickle.loads(msg)
            self.logger.debug("received %s" % message)
            for comp in self.components[:]:
                try:
                    fn = getattr(comp, 'do_%s' % type(message).__name__.upper())
                except AttributeError:
                    continue
                fn(**message._asdict())

    def kill(self):
        self.running = False

    def set_timer(self, seconds, callback):
        timer = Timer(self.now() + seconds, self.address, callback)
        heapq.heappush(self.timers, timer)
        return timer

    def now(self):
        return time.time()

    def send(self, destinations, message):
        self.logger.debug("sending %s %s" % (message, destinations))
        pkl = pickle.dumps(message)
        for dest in destinations:
            self.sock.sendto(pkl, addr_to_tuple(dest))

    def register(self, component):
        self.components.append(component)

    def unregister(self, component):
        self.components.remove(component)
