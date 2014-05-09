from itertools import cycle
from . import JOIN_RETRANSMIT
from .member import Component


class Bootstrap(Component):

    def __init__(self, member, peers, bootstrapped_cb):
        super(Bootstrap, self).__init__(member)
        self.peers_cycle = cycle(peers)
        self.timer = None
        self.bootstrapped_cb = bootstrapped_cb

    def start(self):
        self.join()

    def join(self):
        """Try to join the cluster"""
        self.send([next(self.peers_cycle)], 'JOIN', requester=self.address)
        self.timer = self.set_timer(JOIN_RETRANSMIT, self.join)

    def do_WELCOME(self, state, slot_num, decisions):
        self.bootstrapped_cb(state, slot_num, decisions)

        if self.timer:
            self.timer.cancel()

        self.stop()
