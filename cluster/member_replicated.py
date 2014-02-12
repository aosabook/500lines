import deterministic_network
from collections import namedtuple, defaultdict


Proposal = namedtuple('Proposal', ['caller', 'cid', 'input'])
Ballot = namedtuple('Ballot', ['n', 'leader'])
ScoutId = namedtuple('ScoutId', ['address', 'ballot_num'])
CommanderId = namedtuple('CommanderId', ['address', 'slot', 'proposal'])
ViewChange = namedtuple('ViewChange', ['viewid', 'peers'])


def view_primary(viewid, peers):
    return peers[viewid % len(peers)]


class defaultlist(list):

    def set_len(self, l):
        if l > len(self):
            self.extend([None] * (l - len(self)))

    def __getitem__(self, i):
        self.set_len(i + 1)
        return list.__getitem__(self, i)

    def __setitem__(self, i, v):
        self.set_len(i + 1)
        list.__setitem__(self, i, v)


class Replica(deterministic_network.Node):

    JOIN_RETRANSMIT = 0.2
    HEARTBEAT_INTERVAL = 0.5
    REPROPOSE_INTERVAL = 0.7
    assert JOIN_RETRANSMIT <= HEARTBEAT_INTERVAL

    def __init__(self):
        super(Replica, self).__init__()
        self.replica_ready = False
        self.replica_execute_fn = None
        self.replica_state = None
        self.replica_slot_num = 1
        # next slot num for a proposal (may lead slot_num)
        self.replica_next_slot = 1
        self.replica_proposals = defaultlist()
        self.replica_decisions = defaultlist()
        self.replica_viewid = 0
        self.replica_peers = None
        self.replica_last_heard_from = {}
        self.replica_lost_peer_proposal = None

    def replica_start(self, peers=None):
        self.replica_peers = peers
        self.join()

    def join(self):
        "try to join the cluster"
        if not self.replica_ready:
            self.send([self.core.rnd.choice(self.replica_peers)], 'JOIN',
                      requester=self.address)
        self.set_timer(self.JOIN_RETRANSMIT, self.join)

    def invoke(self, proposal):
        "actually invoke a proposal that is decided and in sequence"
        if proposal.caller:
            # perform a client operation
            self.replica_state, output = self.replica_execute_fn(
                self.replica_state, proposal.input)
            return output

        if isinstance(proposal.input, ViewChange):
            viewchange = proposal.input
            if viewchange.viewid == self.replica_viewid + 1:
                self.logger.info("entering view %d with peers %s" %
                                 (viewchange.viewid, viewchange.peers))
                self.replica_viewid = viewchange.viewid
                self.send(
                    list(set(viewchange.peers) -
                         set(self.replica_peers)), 'WELCOME',
                    state=self.replica_state,
                    slot_num=self.replica_slot_num,
                    decisions=self.replica_decisions,
                    viewid=viewchange.viewid,
                    peers=viewchange.peers)
                if self.address not in viewchange.peers:
                    self.stop()
                    return
                self.replica_peers = tuple(sorted(viewchange.peers))
                self.leader_view_change(
                    view_primary(viewchange.viewid, viewchange.peers) == self.address)
            else:
                self.logger.info(
                    "ignored out-of-sequence view change operation")

    def propose(self, proposal, slot=None):
        if not slot:
            slot = self.replica_next_slot
            self.replica_next_slot += 1
        self.replica_proposals[slot] = proposal
        # find a leader we think is working, deterministically
        leaders = [view_primary(self.replica_viewid, self.replica_peers)] + \
            list(self.replica_peers)
        for leader in leaders:
            if self.replica_last_heard_from.get(leader, 0) > self.core.now - 2 * self.HEARTBEAT_INTERVAL:
                break
        self.logger.info("proposing %s at slot %d to leader %s" %
                         (proposal, slot, leader))
        self.send([leader], 'PROPOSE', slot=slot, proposal=proposal)

    def repropose(self):
        "re-transmit any un-decided proposals"
        for slot in xrange(self.replica_slot_num, self.replica_next_slot):
            if self.replica_proposals[slot]:
                self.propose(self.replica_proposals[slot], slot)
            else:
                # make an empty proposal to learn what was decided
                self.propose(Proposal(None, None, None), slot)
        self.set_timer(self.REPROPOSE_INTERVAL, self.repropose)

    def heartbeat(self):
        "send and monitor heartbeats"
        self.send(self.replica_peers, 'HEARTBEAT', sender=self.address)
        for peer in self.replica_peers:
            if peer == self.address or peer not in self.replica_last_heard_from:
                continue
            if self.replica_last_heard_from[peer] < self.core.now - 2 * self.HEARTBEAT_INTERVAL:
                self.lost_peer(peer)
                break
        self.set_timer(self.HEARTBEAT_INTERVAL, self.heartbeat)

    def lost_peer(self, peer):
        if self.replica_lost_peer_proposal and self.replica_lost_peer_proposal not in self.replica_decisions:
            # we're still working on a lost peer; we'll be called again..
            return
        self.logger.info("lost peer %s; proposing new view" % peer)
        self.replica_lost_peer_proposal = Proposal(None, None,
                                                   ViewChange(
                                                       self.replica_viewid + 1,
                                                       tuple(sorted(set(self.replica_peers) - set([peer])))))
        self.propose(self.replica_lost_peer_proposal)

    def do_HEARTBEAT(self, sender):
        self.replica_last_heard_from[sender] = self.core.now

    def do_INVOKE(self, caller, cid, input):
        if not self.replica_ready:
            self.logger.info("can't INVOKE until joined to the cluster")
            return
        proposal = Proposal(caller, cid, input)
        if proposal not in self.replica_proposals:
            self.propose(proposal)
        else:
            slot = self.replica_proposals.index(proposal)
            self.logger.info("proposal %s already proposed in slot %d" %
                             (proposal, slot))

    def do_DECISION(self, slot, proposal):
        if self.replica_decisions[slot] is not None:
            assert self.replica_decisions[slot] == proposal, \
                "slot %d already decided: %r!" % (
                    slot, self.replica_decisions[slot])
            return
        self.replica_decisions[slot] = proposal
        self.replica_next_slot = max(self.replica_next_slot, slot + 1)

        # execute any pending, decided proposals, eliminating duplicates
        while True:
            decided_proposal = self.replica_decisions[self.replica_slot_num]
            if not decided_proposal:
                break # not decided yet

            # re-propose any of our proposals which have lost in their slot
            our_proposal = self.replica_proposals[self.replica_slot_num]
            if our_proposal is not None and our_proposal != decided_proposal:
                self.propose(our_proposal)

            self.replica_slot_num += 1

            if decided_proposal in self.replica_decisions[:self.replica_slot_num - 1]:
                continue  # duplicate
            self.logger.info("invoking %r" % (decided_proposal,))
            output = self.invoke(decided_proposal)
            if decided_proposal.caller:
                self.send([decided_proposal.caller], 'INVOKED',
                          cid=decided_proposal.cid, output=output)

    def do_JOIN(self, requester):
        if self.replica_ready and requester not in self.replica_peers:
            self.replica_last_heard_from[requester] = self.core.now
            viewchange = ViewChange(self.replica_viewid + 1,
                                    tuple(sorted(set(self.replica_peers) | set([requester]))))
            self.propose(Proposal(None, None, viewchange))

    def do_WELCOME(self, state, slot_num, decisions, viewid, peers):
        if not self.replica_ready:
            self.replica_ready = True
            self.replica_state = state
            self.replica_slot_num = slot_num
            self.replica_next_slot = slot_num
            self.replica_decisions = defaultlist(decisions)
            self.replica_viewid = viewid
            self.replica_peers = tuple(sorted(peers))
            self.replica_last_heard_from = {}
            self.leader_view_change(
                view_primary(self.replica_viewid, self.replica_peers) == self.address)
            self.heartbeat()
            self.repropose()


class Acceptor(deterministic_network.Node):

    def __init__(self):
        super(Acceptor, self).__init__()
        self.acceptor_ballot_num = Ballot(-1, -1)
        self.acceptor_accepted = defaultdict()  # { (b,s) : p }

    def do_PREPARE(self, scout_id, ballot_num):  # p1a
        if ballot_num > self.acceptor_ballot_num:
            self.acceptor_ballot_num = ballot_num
        self.send([scout_id.address], 'PROMISE',  # p1b
                  scout_id=scout_id,
                  acceptor=self.address,
                  ballot_num=self.acceptor_ballot_num,
                  accepted=self.acceptor_accepted)

    def do_ACCEPT(self, commander_id, ballot_num, slot, proposal):  # p2a
        if ballot_num >= self.acceptor_ballot_num:
            self.acceptor_ballot_num = ballot_num
            self.acceptor_accepted[(ballot_num, slot)] = proposal
        self.send([commander_id.address], 'ACCEPTED',  # p2b
                  commander_id=commander_id,
                  acceptor=self.address,
                  ballot_num=self.acceptor_ballot_num)


class Scout(object):

    PREPARE_RETRANSMIT = 1

    def __init__(self, node, ballot_num):
        self.node = node
        self.scout_id = ScoutId(self.node.address, ballot_num)
        self.scout_ballot_num = ballot_num
        self.scout_pvals = defaultdict()
        self.scout_accepted = set([])
        self.scout_quorum = len(node.replica_peers) / 2 + 1
        self.retransmit_timer = None

    def start(self):
        self.node.logger.info("scout starting")
        self.send_prepare()

    def send_prepare(self):
        self.node.send(self.node.replica_peers, 'PREPARE',  # p1a
                       scout_id=self.scout_id,
                       ballot_num=self.scout_ballot_num)
        self.retransmit_timer = self.node.set_timer(
            self.PREPARE_RETRANSMIT, self.send_prepare)

    def finished(self, adopted, ballot_num):
        self.node.cancel_timer(self.retransmit_timer)
        self.node.logger.info(
            "finished - adopted" if adopted else "finished - preempted")
        self.node.scout_finished(adopted, ballot_num, self.scout_pvals)

    def do_PROMISE(self, acceptor, ballot_num, accepted):  # p1b
        if ballot_num == self.scout_ballot_num:
            self.node.logger.info("got matching promise; need %d" %
                                  self.scout_quorum)
            self.scout_pvals.update(accepted)
            self.scout_accepted.add(acceptor)
            if len(self.scout_accepted) >= self.scout_quorum:
                # We're adopted; note that this does *not* mean that no other leader is active.
                # Any such conflicts will be handled by the commanders.
                self.finished(True, ballot_num)
        else:
            # ballot_num > self.scout_ballot_num; responses to other scouts don't
            # result in a call to this method
            self.finished(False, ballot_num)


class Commander(object):

    def __init__(self, node, ballot_num, slot, proposal):
        self.node = node
        self.commander_ballot_num = ballot_num
        self.commander_slot = slot
        self.commander_proposal = proposal
        self.commander_id = CommanderId(node.address, slot, proposal)
        self.commander_accepted = set([])
        self.commander_peers = node.replica_peers = node.replica_peers
        self.commander_quorum = len(node.replica_peers) / 2 + 1

    def start(self):
        self.node.send(self.commander_peers, 'ACCEPT',  # p2a
                       commander_id=self.commander_id,
                       ballot_num=self.commander_ballot_num,
                       slot=self.commander_slot,
                       proposal=self.commander_proposal)

    def do_ACCEPTED(self, acceptor, ballot_num):  # p2b
        if ballot_num == self.commander_ballot_num:
            self.commander_accepted.add(acceptor)
            if len(self.commander_accepted) >= self.commander_quorum:
                self.node.send(self.commander_peers, 'DECISION',
                               slot=self.commander_slot,
                               proposal=self.commander_proposal)
                self.node.commander_finished(
                    self.commander_id, ballot_num, False)
        else:
            self.node.commander_finished(self.commander_id, ballot_num, True)


class Leader(deterministic_network.Node):

    HEARTBEAT_INTERVAL = 1

    def __init__(self):
        super(Leader, self).__init__()
        self.leader_ballot_num = Ballot(0, self.unique_id)
        self.leader_active = False
        self.leader_proposals = defaultlist()
        self.leader_commanders = {}
        self.leader_scout = None

    def leader_view_change(self, is_primary):
        self.leader_is_primary = is_primary
        if is_primary:
            if not self.leader_scout and not self.leader_active:
                self.spawn_scout()
        else:
            if self.leader_scout:
                self.leader_scout.finished(False, None)
                # .. which eventually calls self.preempted
            elif self.leader_active:
                self.preempted(None)

    def spawn_scout(self):
        assert not self.leader_scout
        sct = self.leader_scout = Scout(self, self.leader_ballot_num)
        sct.start()

    def scout_finished(self, adopted, ballot_num, pvals):
        self.leader_scout = None
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
                    self.leader_proposals[s] = p
            for s, p in enumerate(self.leader_proposals):
                if p is not None:
                    self.spawn_commander(ballot_num, s, p)
            self.logger.info("leader becoming active")
            self.leader_active = True
        else:
            self.preempted(ballot_num)

    def commander_finished(self, commander_id, ballot_num, preempted):
        del self.leader_commanders[commander_id]
        if preempted:
            self.preempted(ballot_num)

    def preempted(self, ballot_num):
        # ballot_num is None when we are preempted by a view change
        if ballot_num:
            self.logger.info("leader preempted by %s" % (ballot_num.leader,))
        else:
            self.logger.info("leader preempted by view change")
        self.leader_active = False
        self.leader_ballot_num = Ballot(
            (ballot_num if ballot_num else self.leader_ballot_num).n + 1, self.unique_id)
        # if we're the primary for this view, re-scout immediately
        if not self.leader_scout and self.leader_is_primary:
            self.logger.info("re-scouting as the primary for this view")
            self.spawn_scout()

    def spawn_commander(self, ballot_num, slot, proposal):
        cmd = Commander(self, ballot_num, slot, proposal)
        if cmd.commander_id in self.leader_commanders:
            return
        self.leader_commanders[cmd.commander_id] = cmd
        cmd.start()

    def do_PROPOSE(self, slot, proposal):
        if self.leader_proposals[slot] is None:
            if self.leader_active:
                self.leader_proposals[slot] = proposal
                self.spawn_commander(self.leader_ballot_num, slot, proposal)
            else:
                if not self.leader_scout:
                    self.logger.warning(
                        "got PROPOSE when not active - scouting")
                    self.spawn_scout()

    def do_PROMISE(self, scout_id, acceptor, ballot_num, accepted):
        sct = self.leader_scout
        if sct and scout_id == sct.scout_id:
            sct.do_PROMISE(acceptor, ballot_num, accepted)

    def do_ACCEPTED(self, commander_id, acceptor, ballot_num):
        cmd = self.leader_commanders.get(commander_id)
        if cmd:
            cmd.do_ACCEPTED(acceptor, ballot_num)


class ClusterMember(Replica, Acceptor, Leader):

    def __init__(self, execute_fn, peers):
        super(ClusterMember, self).__init__()
        self.replica_execute_fn = execute_fn
        self.member_potential_peers = peers

    def start(self):
        self.replica_start(self.member_potential_peers)


class ClusterSeed(deterministic_network.Node):

    """A node which simply provides an initial state and view.  It waits until
    it has heard JOIN requests from enough nodes to form a cluster, then WELCOMEs
    them all to the same view with the given initial state."""

    def __init__(self, initial_state):
        super(ClusterSeed, self).__init__()
        self.initial_state = initial_state
        self.peers = set([])
        self.exit_timer = None

    def do_JOIN(self, requester):
        # three is the minimum membership for a working cluster, so don't
        # respond until then, but don't expand the peers list beyond 3
        if len(self.peers) < 3:
            self.peers.add(requester)
            if len(self.peers) < 3:
                return

        # otherwise, we have a cluster, but don't welcome any nodes not
        # part of that cluster (the cluster can do that itself)
        if requester not in self.peers:
            return

        self.send(self.peers, 'WELCOME',
                  state=self.initial_state,
                  slot_num=1,
                  decisions=defaultlist(),
                  viewid=0,
                  peers=list(self.peers))

        # stick around for long enough that we don't hear any new JOINs from
        # the newly formed cluster
        if self.exit_timer:
            self.cancel_timer(self.exit_timer)
        self.exit_timer = self.set_timer(
            Replica.JOIN_RETRANSMIT * 2, self.stop)
