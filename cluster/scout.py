from collections import defaultdict
from protocol import ScoutId
from member import Component

class Scout(Component):

    PREPARE_RETRANSMIT = 1

    def __init__(self, member, leader, ballot_num, peers):
        super(Scout, self).__init__(member)
        self.leader = leader
        self.scout_id = ScoutId(self.address, ballot_num)
        self.ballot_num = ballot_num
        self.pvals = defaultdict()
        self.accepted = set([])
        self.peers = peers
        self.quorum = len(peers) / 2 + 1
        self.retransmit_timer = None

    def start(self):
        self.logger.info("scout starting")
        self.send_prepare()

    def send_prepare(self):
        self.send(self.peers, 'PREPARE',  # p1a
                       scout_id=self.scout_id,
                       ballot_num=self.ballot_num)
        self.retransmit_timer = self.set_timer(
            self.PREPARE_RETRANSMIT, self.send_prepare)

    def finished(self, adopted, ballot_num):
        self.cancel_timer(self.retransmit_timer)
        self.logger.info(
            "finished - adopted" if adopted else "finished - preempted")
        self.leader.scout_finished(adopted, ballot_num, self.pvals)
        self.stop()

    def do_PROMISE(self, scout_id, acceptor, ballot_num, accepted):  # p1b
        if scout_id != self.scout_id:
            return
        if ballot_num == self.ballot_num:
            self.logger.info("got matching promise; need %d" %
                                  self.quorum)
            self.pvals.update(accepted)
            self.accepted.add(acceptor)
            if len(self.accepted) >= self.quorum:
                # We're adopted; note that this does *not* mean that no other leader is active.
                # Any such conflicts will be handled by the commanders.
                self.finished(True, ballot_num)
        else:
            # ballot_num > self.ballot_num; responses to other scouts don't
            # result in a call to this method
            self.finished(False, ballot_num)

