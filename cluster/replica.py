import protocol
from util import defaultlist, view_primary
from protocol import Proposal, ViewChange
from member import Component


class Replica(Component):

    def __init__(self, member, execute_fn):
        super(Replica, self).__init__(member)
        self.execute_fn = execute_fn
        self.proposals = defaultlist()
        self.viewchange_proposal = None

    def start(self, state, slot_num, decisions, viewid, peers, peer_history):
        self.state = state
        self.slot_num = slot_num
        # next slot num for a proposal (may lead slot_num)
        self.next_slot = slot_num
        self.decisions = defaultlist(decisions)
        self.viewid = viewid
        self.peers = peers
        self.peers_down = set()
        self.peer_history = peer_history.copy()

        self.repropose()

    # creating proposals

    def do_INVOKE(self, caller, cid, input):
        proposal = Proposal(caller, cid, input)
        if proposal not in self.proposals:
            self.propose(proposal)
        else:
            slot = self.proposals.index(proposal)
            self.logger.info("proposal %s already proposed in slot %d" %
                             (proposal, slot))

    def do_JOIN(self, requester):
        if requester not in self.peers:
            viewchange = ViewChange(self.viewid + 1,
                                    tuple(sorted(set(self.peers) | set([requester]))))
            self.propose(Proposal(None, None, viewchange))

    # injecting proposals

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
        if self.slot_num != self.next_slot:
            self.logger.warning("re-proposing %d .. %d" % (self.slot_num, self.next_slot-1))
        for slot in xrange(self.slot_num, self.next_slot):
            if self.proposals[slot]:
                if not self.decisions[slot]:
                    self.propose(self.proposals[slot], slot)
            else:
                # make an empty proposal to learn what was decided
                self.propose(Proposal(None, None, None), slot)
        self.set_timer(protocol.REPROPOSE_INTERVAL, self.repropose)

    # view changes

    def on_view_change_event(self, slot, viewid, peers):
        self.peers = peers
        self.peers_down = set()

    def on_peers_down_event(self, down):
        self.peers_down = down
        if self.viewchange_proposal and self.viewchange_proposal not in self.decisions:
            return  # we're still working on a viewchange that hasn't been decided
        new_peers = tuple(sorted(set(self.peers) - set(down)))
        if len(new_peers) < 3:
            self.logger.info("lost peer(s) %s; need at least three peers" % (down,))
            return
        self.logger.info("lost peer(s) %s; proposing new view" % (down,))
        self.viewchange_proposal = Proposal(
                None, None,
                ViewChange(self.viewid + 1, tuple(sorted(set(self.peers) - set(down)))))
        self.propose(self.viewchange_proposal)

    # handling decided proposals

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
            decided_slot, self.slot_num = self.slot_num, self.slot_num + 1

            self.commit(decided_slot, decided_proposal)

            # update the view history
            self.peer_history[decided_slot] = self.peers
            if decided_slot - protocol.ALPHA in self.peer_history:
                del self.peer_history[decided_slot - protocol.ALPHA]
            exp_peer_history = list(range(decided_slot - protocol.ALPHA + 1, decided_slot + 1))
            assert list(sorted(self.peer_history)) == exp_peer_history, \
                    "bad peer history %s, exp %s" % (self.peer_history, exp_peer_history)
            self.event('update_peer_history', peer_history=self.peer_history)

            # re-propose any of our proposals which have lost in their slot
            our_proposal = self.proposals[decided_slot]
            if our_proposal is not None and our_proposal != decided_proposal:
                self.propose(our_proposal)

    def commit(self, slot, proposal):
        "actually commit a proposal that is decided and in sequence"
        if proposal in self.decisions[:slot]:
            return  # duplicate

        self.logger.info("committing %r at slot %d" % (proposal, slot))
        self.event('commit', slot=slot, proposal=proposal)

        if isinstance(proposal.input, ViewChange):
            self.commit_viewchange(slot, proposal.input)
        elif proposal.caller is not None:
            # perform a client operation
            self.state, output = self.execute_fn(
                self.state, proposal.input)
            self.send([proposal.caller], 'INVOKED',
                      cid=proposal.cid, output=output)

    def commit_viewchange(self, slot, viewchange):
        if viewchange.viewid == self.viewid + 1:
            self.logger.info("entering view %d with peers %s" %
                                (viewchange.viewid, viewchange.peers))
            self.viewid = viewchange.viewid
            self.send(list(set(viewchange.peers) - set(self.peers)), 'WELCOME',
                      state=self.state,
                      slot_num=self.slot_num,
                      decisions=self.decisions,
                      viewid=viewchange.viewid,
                      peers=viewchange.peers,
                      peer_history=self.peer_history)

            # now make sure that next_slot is at least slot + ALPHA, so that we don't
            # try to make any new proposals depending on the old view.  The repropose()
            # method will take care of proposing these later.
            self.next_slot = max(slot + protocol.ALPHA, self.next_slot)

            if self.address not in viewchange.peers:
                self.stop()
                return
            self.event('view_change', slot=slot, viewid=viewchange.viewid, peers=viewchange.peers)
        else:
            self.logger.info(
                "ignored out-of-sequence view change operation")

    def do_PROPOSE(self, slot, proposal):
        # if we have a decision for this proposal, spread the knowledge, since leaders will
        # just ignore this request
        if self.decisions[slot]:
            self.send(self.peers, 'DECISION',
                      slot=slot, proposal=self.decisions[slot])
