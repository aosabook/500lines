import protocol
from member import Component


class Bootstrap(Component):

    def __init__(self, member, peers):
        super(Bootstrap, self).__init__(member)
        self.peers = peers

    def start(self):
        self.join()

    def join(self):
        "Try to join the cluster"
        # TODO: do something more deterministic
        self.send([self.member.node.network.rnd.choice(self.peers)], 'JOIN',
                            requester=self.address)
        self.set_timer(protocol.JOIN_RETRANSMIT, self.join)

    def do_WELCOME(self, state, slot_num, decisions, viewid, peers):
        self.event('joined', state=state, slot_num=slot_num, decisions=decisions, viewid=viewid, peers=peers)
        # TOOD: finish setup of replica, leader, acceptor here (callback to member?)
        self.stop()
