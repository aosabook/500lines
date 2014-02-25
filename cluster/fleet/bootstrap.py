from . import JOIN_RETRANSMIT
from .member import Component


class Bootstrap(Component):

    def __init__(self, member, peers, bootstrapped_cb):
        super(Bootstrap, self).__init__(member)
        self.peers = peers
        self.timer = None
        self.bootstrapped_cb = bootstrapped_cb

    def start(self):
        self.join()

    def join(self):
        "Try to join the cluster"
        self.peers = self.peers[1:] + self.peers[:1] # rotate through peers
        self.send([self.peers[0]], 'JOIN', requester=self.address)
        self.timer = self.set_timer(JOIN_RETRANSMIT, self.join)

    def do_WELCOME(self, state, slot_num, decisions, viewid, peers, peer_history):
        self.bootstrapped_cb(state, slot_num, decisions, viewid, peers, peer_history)
        self.event('view_change', viewid=viewid, peers=peers, slot=slot_num)
        self.event('peer_history_update', peer_history=peer_history)
        if self.timer:
            self.cancel_timer(self.timer)
        self.stop()
