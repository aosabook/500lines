import protocol
from protocol import ViewChange
from member import Component


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
        self.timer = self.set_timer(protocol.JOIN_RETRANSMIT, self.join)

    def do_WELCOME(self, state, slot_num, decisions, viewid, peers):
        self.bootstrapped_cb(state, slot_num, decisions, viewid, peers)
        self.event('view_change', viewchange=ViewChange(viewid, peers))  # TODO: pass viewid, peers separately
        if self.timer:
            self.cancel_timer(self.timer)
        self.stop()
