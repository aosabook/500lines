import protocol
from protocol import CommanderId
from member import Component


class Commander(Component):

    def __init__(self, member, leader, ballot_num, slot, proposal, peers):
        super(Commander, self).__init__(member)
        self.leader = leader
        self.ballot_num = ballot_num
        self.slot = slot
        self.proposal = proposal
        self.commander_id = CommanderId(self.address, slot, proposal)
        self.accepted = set([])
        self.peers = peers
        self.quorum = len(peers) / 2 + 1
        self.timer = None

    def start(self):
        self.send(set(self.peers) - self.accepted, 'ACCEPT',  # p2a
                         commander_id=self.commander_id,
                         ballot_num=self.ballot_num,
                         slot=self.slot,
                         proposal=self.proposal)
        self.timer = self.set_timer(protocol.ACCEPT_RETRANSMIT, self.start)

    def finished(self, ballot_num, preempted):
        self.leader.commander_finished(self.commander_id, ballot_num, preempted)
        if self.timer:
            self.cancel_timer(self.timer)
        self.stop()

    def do_ACCEPTED(self, commander_id, acceptor, ballot_num):  # p2b
        if commander_id != self.commander_id:
            return
        if ballot_num == self.ballot_num:
            self.accepted.add(acceptor)
            if len(self.accepted) < self.quorum:
                return
            # make sure that this node hears about the decision, otherwise the
            # slot can get "stuck" if all of the DECISION messages get lost, or
            # if this node is not in self.peers
            self.event('decision', slot=self.slot, proposal=self.proposal)
            self.send(self.peers, 'DECISION',
                      slot=self.slot,
                      proposal=self.proposal)
            self.finished(ballot_num, False)
        else:
            self.finished(ballot_num, True)
