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

    def start(self):
        self.send(self.peers, 'ACCEPT',  # p2a
                         commander_id=self.commander_id,
                         ballot_num=self.ballot_num,
                         slot=self.slot,
                         proposal=self.proposal)

    def finished(self, ballot_num, preempted):
        self.leader.commander_finished(self.commander_id, ballot_num, preempted)
        self.stop()

    def do_ACCEPTED(self, commander_id, acceptor, ballot_num):  # p2b
        if commander_id != self.commander_id:
            return
        if ballot_num == self.ballot_num:
            self.accepted.add(acceptor)
            if len(self.accepted) < self.quorum:
                return
            # include this node in the decision, in case it wasn't part of
            # the quorum at the time; otherwise we will never learn about this
            # slot
            peers = set(self.peers) | set([self.address])
            self.send(peers, 'DECISION',
                      slot=self.slot,
                      proposal=self.proposal)
            self.finished(ballot_num, False)
        else:
            self.finished(ballot_num, True)
