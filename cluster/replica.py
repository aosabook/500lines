from util import defaultlist, view_primary
from protocol import Proposal, ViewChange
from member import Component


class Replica(Component):

    # TODO: move constants to protocol.py
    JOIN_RETRANSMIT = 0.2
    HEARTBEAT_INTERVAL = 0.5
    REPROPOSE_INTERVAL = 0.7
    assert JOIN_RETRANSMIT <= HEARTBEAT_INTERVAL

    def __init__(self, member, execute_fn, peers):
        super(Replica, self).__init__(member)
        self.ready = False
        self.execute_fn = execute_fn
        self.state = None
        self.slot_num = 1
        # next slot num for a proposal (may lead slot_num)
        self.next_slot = 1
        self.proposals = defaultlist()
        self.decisions = defaultlist()
        self.viewid = 0
        self.last_heard_from = {}
        self.lost_peer_proposal = None

    def start(self):
        "Try to join the cluster"
        if not self.ready:
            # TODO: do something more deterministic
            self.send([self.member.node.network.rnd.choice(self.member.peers)], 'JOIN',
                             requester=self.address)
            self.set_timer(self.JOIN_RETRANSMIT, self.start)

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
                         set(self.member.peers)), 'WELCOME',
                    state=self.state,
                    slot_num=self.slot_num,
                    decisions=self.decisions,
                    viewid=viewchange.viewid,
                    peers=viewchange.peers)
                if self.address not in viewchange.peers:
                    self.stop()
                    return
                self.member.view_change(viewchange)
            else:
                self.logger.info(
                    "ignored out-of-sequence view change operation")

    def propose(self, proposal, slot=None):
        if not slot:
            slot = self.next_slot
            self.next_slot += 1
        self.proposals[slot] = proposal
        # find a leader we think is working, deterministically
        leaders = [view_primary(self.viewid, self.member.peers)] + \
            list(self.member.peers)
        for leader in leaders:
            # TODO: better way to get current time
            if self.last_heard_from.get(leader, 0) > self.member.node.network.now - 2 * self.HEARTBEAT_INTERVAL:
                break
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

    def heartbeat(self):
        "send and monitor heartbeats"
        self.send(self.member.peers, 'HEARTBEAT', sender=self.address)
        for peer in self.member.peers:
            if peer == self.address or peer not in self.last_heard_from:
                continue
            if self.last_heard_from[peer] < self.member.node.network.now - 2 * self.HEARTBEAT_INTERVAL:
                self.lost_peer(peer)
                break
        self.set_timer(self.HEARTBEAT_INTERVAL, self.heartbeat)

    def lost_peer(self, peer):
        if self.lost_peer_proposal and self.lost_peer_proposal not in self.decisions:
            # we're still working on a lost peer; we'll be called again..
            return
        self.logger.info("lost peer %s; proposing new view" % peer)
        self.lost_peer_proposal = Proposal(None, None,
                                           ViewChange(
                                               self.viewid + 1,
                                               tuple(sorted(set(self.member.peers) - set([peer])))))
        self.propose(self.lost_peer_proposal)

    def do_HEARTBEAT(self, sender):
        self.last_heard_from[sender] = self.member.node.network.now

    def do_INVOKE(self, caller, cid, input):
        if not self.ready:
            self.logger.info("can't INVOKE until joined to the cluster")
            return
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
        if self.ready and requester not in self.member.peers:
            self.last_heard_from[requester] = self.member.node.network.now
            viewchange = ViewChange(self.viewid + 1,
                                    tuple(sorted(set(self.member.peers) | set([requester]))))
            self.propose(Proposal(None, None, viewchange))

    def do_WELCOME(self, state, slot_num, decisions, viewid, peers):
        if not self.ready:
            self.ready = True
            self.state = state
            self.slot_num = slot_num
            self.next_slot = slot_num
            self.decisions = defaultlist(decisions)
            self.viewid = viewid
            self.last_heard_from = {}
            self.member.view_change(ViewChange(viewid, peers))  # TODO: WELCOME should include a ViewChange
            self.heartbeat()
            self.repropose()
