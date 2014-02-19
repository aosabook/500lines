from collections import defaultdict
from protocol import Ballot
from member import Component


class Acceptor(Component):

    def __init__(self, member):
        super(Acceptor, self).__init__(member)
        self.ballot_num = Ballot(-1, -1)
        self.accepted = defaultdict()  # { (b,s) : p }

    def do_PREPARE(self, scout_id, ballot_num):  # p1a
        if ballot_num > self.ballot_num:
            self.ballot_num = ballot_num
        self.send([scout_id.address], 'PROMISE',  # p1b
                         scout_id=scout_id,
                         acceptor=self.address,
                         ballot_num=self.ballot_num,
                         accepted=self.accepted)

    def do_ACCEPT(self, commander_id, ballot_num, slot, proposal):  # p2a
        if ballot_num >= self.ballot_num:
            self.ballot_num = ballot_num
            self.accepted[(ballot_num, slot)] = proposal
        self.send([commander_id.address], 'ACCEPTED',  # p2b
                         commander_id=commander_id,
                         acceptor=self.address,
                         ballot_num=self.ballot_num)
