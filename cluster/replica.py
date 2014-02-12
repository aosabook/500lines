from util import defaultlist, view_primary
from protocol import Proposal, ViewChange
from member import Component


class Replica(Component):

    # TODO: move constants to protocol.py
    REPROPOSE_INTERVAL = 0.7

    def __init__(self, member, execute_fn):
        super(Replica, self).__init__(member)
        self.execute_fn = execute_fn
        self.proposals = defaultlist()
        self.viewchange_proposal = None

    def start(self, state, slot_num, decisions, viewid, peers):
        self.state = state
        self.slot_num = slot_num
        # next slot num for a proposal (may lead slot_num)
        self.next_slot = slot_num
        self.decisions = defaultlist(decisions)
        self.viewid = viewid
        self.peers = peers
        self.peers_down = set()

        self.repropose()

    def invoke(self, proposal):
        "actually invoke a proposal that is decided and in sequence"
        if proposal.caller:
            # perform a client operation
            self.state, output = self.execute_fn(
                self.state, proposal.input)
            return output

        if isinstance(proposal.input, ViewChange):
            viewchange = proposal.input
            if viewchange.viewid == self.viewid + 1:
                self.logger.info("entering view %d with peers %s" %
                                 (viewchange.viewid, viewchange.peers))
                self.viewid = viewchange.viewid
                self.send(
                    list(set(viewchange.peers) -
                         set(self.peers)), 'WELCOME',
                    state=self.state,
                    slot_num=self.slot_num,
                    decisions=self.decisions,
                    viewid=viewchange.viewid,
                    peers=viewchange.peers)
                if self.address not in viewchange.peers:
                    self.stop()
                    return
                self.event('view_change', viewchange=viewchange)
            else:
                self.logger.info(
                    "ignored out-of-sequence view change operation")

    def propose(self, proposal, slot=None):
        if not slot:
            slot = self.next_slot
            self.next_slot += 1
        self.proposals[slot] = proposal
        # find a leader we think is working, deterministically
        leaders = [view_primary(self.viewid, self.peers)] + \
            list(self.peers)
        leader = (l for l in leaders if l not in self.peers_down).next()
        self.logger.info("proposing %s at slot %d to leader %s" %
                         (proposal, slot, leader))
        self.send([leader], 'PROPOSE', slot=slot, proposal=proposal)

    def repropose(self):
        "re-transmit any un-decided proposals"
        for slot in xrange(self.slot_num, self.next_slot):
            if self.proposals[slot]:
                self.propose(self.proposals[slot], slot)
            else:
                # make an empty proposal to learn what was decided
                self.propose(Proposal(None, None, None), slot)
        self.set_timer(self.REPROPOSE_INTERVAL, self.repropose)

    def on_lost_peers_event(self, down):
        self.peers_down = down
        if self.viewchange_proposal and self.viewchange_proposal not in self.decisions:
            return  # we're still working on a viewchange that hasn't been decided
        self.logger.info("lost peer(s) %s; proposing new view" % (down,))
        self.viewchange_proposal = Proposal(
                None, None,
                ViewChange(self.viewid + 1, tuple(sorted(set(self.peers) - set(down)))))
        self.propose(self.viewchange_proposal)

    def do_INVOKE(self, caller, cid, input):
        proposal = Proposal(caller, cid, input)
        if proposal not in self.proposals:
            self.propose(proposal)
        else:
            slot = self.proposals.index(proposal)
            self.logger.info("proposal %s already proposed in slot %d" %
                             (proposal, slot))

    def do_DECISION(self, slot, proposal):
        if self.decisions[slot] is not None:
            assert self.decisions[slot] == proposal, \
                "slot %d already decided: %r!" % (
                    slot, self.decisions[slot])
            return
        self.decisions[slot] = proposal
        self.next_slot = max(self.next_slot, slot + 1)

        # execute any pending, decided proposals, eliminating duplicates
        while True:
            decided_proposal = self.decisions[self.slot_num]
            if not decided_proposal:
                break  # not decided yet

            # re-propose any of our proposals which have lost in their slot
            our_proposal = self.proposals[self.slot_num]
            if our_proposal is not None and our_proposal != decided_proposal:
                self.propose(our_proposal)

            self.slot_num += 1

            if decided_proposal in self.decisions[:self.slot_num - 1]:
                continue  # duplicate
            self.logger.info("invoking %r" % (decided_proposal,))
            output = self.invoke(decided_proposal)
            if decided_proposal.caller:
                self.send([decided_proposal.caller], 'INVOKED',
                                 cid=decided_proposal.cid, output=output)

    def do_JOIN(self, requester):
        if requester not in self.peers:
            viewchange = ViewChange(self.viewid + 1,
                                    tuple(sorted(set(self.peers) | set([requester]))))
            self.propose(Proposal(None, None, viewchange))

    def on_view_change_event(self, viewchange):
        self.peers = viewchange.peers
        self.peers_down = set()
