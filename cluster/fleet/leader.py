from . import Ballot, CommanderId, LEADER_TIMEOUT, Active
from .commander import Commander
from .member import Component
from .scout import Scout


class Leader(Component):

    def __init__(self, member, unique_id, peers, commander_cls=Commander, scout_cls=Scout):
        super(Leader, self).__init__(member)
        self.ballot_num = Ballot(0, unique_id)
        self.active = False
        self.proposals = {}
        self.commander_cls = commander_cls
        self.commanders = {}
        self.scout_cls = scout_cls
        self.scout = None
        self.peers = peers

    def start(self):
        # reminder others we're active before LEADER_TIMEOUT expires
        def active():
            if self.active:
                self.send(self.peers, Active())
            self.set_timer(LEADER_TIMEOUT/2.0, active)
        active()

    def spawn_scout(self):
        assert not self.scout
        sct = self.scout = self.scout_cls(
            self.member, self, self.ballot_num, self.peers)
        sct.start()

    # TODO: rename pvals to something with semantic meaning
    def merge_pvals(self, pvals):
        # pvals is a defaultdict of proposal by (ballot num, slot); we need
        # the proposal with highest ballot number for each slot.  In this
        # comprehension, proposals with lower ballot numbers will be
        # overwritten by proposals with higher ballot numbers. It is
        # guaranteed since we sorting pvals items in ascending order.
        last_by_slot = {s: p for (b, s), p in sorted(pvals.items())}
        for slot_id, proposal in last_by_slot.iteritems():
            self.proposals[slot_id] = proposal

    def scout_finished(self, adopted, ballot_num, pvals):
        self.scout = None
        if adopted:
            self.merge_pvals(pvals)
            # note that we don't re-spawn commanders here; if there are undecided
            # proposals, the replicas will re-propose
            self.logger.info("leader becoming active")
            self.event('leader_changed', new_leader=self.address)
            self.active = True
        else:
            self.preempted(ballot_num)

    def spawn_commander(self, ballot_num, slot):
        proposal = self.proposals[slot]
        commander_id = CommanderId(self.address, slot, proposal)
        if commander_id in self.commanders:
            return
        cmd = self.commander_cls(
            self.member, self, ballot_num, slot, proposal, commander_id, self.peers)
        self.commanders[commander_id] = cmd
        cmd.start()

    def commander_finished(self, commander_id, ballot_num, preempted):
        del self.commanders[commander_id]
        if preempted:
            self.preempted(ballot_num)

    def preempted(self, ballot_num):
        self.logger.info("leader preempted by %s, but I'm %d" % (ballot_num.leader, self.ballot_num.leader))
        self.active = False
        self.ballot_num = Ballot(
            (ballot_num or self.ballot_num).n + 1, self.ballot_num.leader)

    def do_PROPOSE(self, sender, slot, proposal):
        if slot not in self.proposals:
            if self.active:
                self.proposals[slot] = proposal
                self.logger.info("spawning commander for slot %d" % (slot,))
                self.spawn_commander(self.ballot_num, slot)
            else:
                if not self.scout:
                    self.logger.info("got PROPOSE when not active - scouting")
                    self.spawn_scout()
                else:
                    self.logger.info("got PROPOSE while scouting; ignored")
        else:
            self.logger.info("got PROPOSE for a slot already being proposed")
