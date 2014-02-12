import protocol
from protocol import ViewChange
from member import Component


class Bootstrap(Component):

    def __init__(self, member, peers, bootstrapped_cb):
        super(Bootstrap, self).__init__(member)
        self.peers = peers
        self.bootstrapped_cb = bootstrapped_cb

    def start(self):
        self.join()

    def join(self):
        "Try to join the cluster"
        # TODO: do something more deterministic
        self.send([self.member.node.network.rnd.choice(self.peers)], 'JOIN',
                            requester=self.address)
        self.set_timer(protocol.JOIN_RETRANSMIT, self.join)

    def do_WELCOME(self, state, slot_num, decisions, viewid, peers):
        self.bootstrapped_cb(state, slot_num, decisions, viewid, peers)
        self.event('view_change', viewchange=ViewChange(viewid, peers))  # TODO: WELCOME should include a ViewChange
        self.stop()
