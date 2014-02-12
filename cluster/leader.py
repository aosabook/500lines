from util import defaultlist, view_primary
from protocol import Ballot
from member import Component
from scout import Scout
from commander import Commander

class Leader(Component):

    HEARTBEAT_INTERVAL = 1

    def __init__(self, member, unique_id, commander_cls=Commander, scout_cls=Scout):
        super(Leader, self).__init__(member)
        self.ballot_num = Ballot(0, unique_id)
        self.active = False
        self.proposals = defaultlist()
        self.commander_cls = commander_cls
        self.commanders = {}
        self.scout_cls = scout_cls
        self.scout = None
        self.peers = None

    def on_view_change_event(self, viewchange):
        self.peers = viewchange.peers
        is_primary = view_primary(viewchange.viewid, viewchange.peers) == self.address
        self.is_primary = is_primary
        if is_primary:
            if not self.scout and not self.active:
                self.spawn_scout()
        else:
            if self.scout:
                self.scout.finished(False, None)
                # .. which eventually calls self.preempted
            elif self.active:
                self.preempted(None)

    def spawn_scout(self):
        assert not self.scout
        sct = self.scout = self.scout_cls(self.member, self, self.ballot_num, self.peers)
        sct.start()

    def scout_finished(self, adopted, ballot_num, pvals):
        self.scout = None
        if adopted:
            # pvals is a defaultlist of (slot, proposal) by ballot num; we need the
            # highest ballot number for each slot.  TODO: this is super
            # inefficient!
            last_by_slot = defaultlist()
            for b, s in reversed(sorted(pvals.keys())):
                p = pvals[b, s]
                if last_by_slot[s] is None:
                    last_by_slot[s] = p
            for s, p in enumerate(last_by_slot):
                if p is not None:
                    self.proposals[s] = p
            for s, p in enumerate(self.proposals):
                if p is not None:
                    self.spawn_commander(ballot_num, s, p)
            self.logger.info("leader becoming active")
            self.active = True
        else:
            self.preempted(ballot_num)

    def preempted(self, ballot_num):
        # ballot_num is None when we are preempted by a view change
        if ballot_num:
            self.logger.info("leader preempted by %s" % (ballot_num.leader,))
        else:
            self.logger.info("leader preempted by view change")
        self.active = False
        self.ballot_num = Ballot(
            (ballot_num if ballot_num else self.ballot_num).n + 1, self.unique_id)
        # if we're the primary for this view, re-scout immediately
        if not self.scout and self.is_primary:
            self.logger.info("re-scouting as the primary for this view")
            self.spawn_scout()

    def spawn_commander(self, ballot_num, slot, proposal):
        cmd = self.commander_cls(self.member, self, ballot_num, slot, proposal, self.peers)
        if cmd.commander_id in self.commanders:
            return
        print "set", cmd.commander_id
        self.commanders[cmd.commander_id] = cmd
        cmd.start()

    def commander_finished(self, commander_id, ballot_num, preempted):
        print "del", commander_id
        del self.commanders[commander_id]
        if preempted:
            self.preempted(ballot_num)

    def do_PROPOSE(self, slot, proposal):
        if self.proposals[slot] is None:
            if self.active:
                self.proposals[slot] = proposal
                self.spawn_commander(self.ballot_num, slot, proposal)
            else:
                if not self.scout:
                    self.logger.warning(
                        "got PROPOSE when not active - scouting")
                    self.spawn_scout()

