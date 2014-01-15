import deterministic_network
from collections import namedtuple, defaultdict


Proposal = namedtuple('Proposal', ['caller', 'cid', 'input'])
Ballot = namedtuple('Ballot', ['n', 'leader'])
ScoutId = namedtuple('ScoutId', ['address', 'ballot_num'])
CommanderId = namedtuple('CommanderId', ['address', 'slot', 'proposal'])


class defaultlist(list):

    def __getitem__(self, i):
        if i >= len(self):
            return None
        return list.__getitem__(self, i)

    def __setitem__(self, i, v):
        if i >= len(self):
            self.extend([None] * (i - len(self) + 1))
        list.__setitem__(self, i, v)


class Replica(deterministic_network.Node):

    def __init__(self):
        super(Replica, self).__init__()
        self.replica_ready = False
        self.replica_execute_fn = None
        self.replica_state = None
        self.replica_slot_num = 0
        self.replica_proposals = defaultlist()
        self.replica_decisions = defaultlist()

    def replica_start(self, initial_state=None, peers=None):
        def send():
            if not self.replica_ready:
                self.send(self.member_peers, 'GET_STATE', requester=self.address)
            self.set_timer(1, send)
        send()

    def invoke(self, proposal):
        # call the execute_fn if this is a client operation
        if proposal.caller:
            self.replica_state, output = self.replica_execute_fn(self.replica_state, proposal.input)
            return output

        assert 0

    def propose(self, proposal):
        slot = max(len(self.replica_proposals),
                    len(self.replica_decisions))
        self.logger.info("proposing %s at slot %d" % (proposal, slot))
        self.replica_proposals[slot] = proposal
        self.send(self.member_peers, 'PROPOSE', slot=slot, proposal=proposal)

    def do_INVOKE(self, caller, cid, input):
        if not self.replica_ready:
            self.logger.info("can't INVOKE until joined to the cluster")
            return
        proposal = Proposal(caller, cid, input)
        if proposal not in self.replica_proposals:
            self.propose(proposal)
        else:
            slot = self.replica_proposals.index(proposal)
            self.logger.info("proposal %s already proposed in slot %d" % (proposal, slot))

    def do_DECISION(self, slot, proposal):
        if self.replica_decisions[slot] is not None:
            assert self.replica_decisions[slot] == proposal
            return
        self.replica_decisions[slot] = proposal

        # execute any pending, decided proposals, eliminating duplicates
        while True:
            decided_proposal = self.replica_decisions[self.replica_slot_num]
            if not decided_proposal:
                break  # not decided yet

            # re-propose any of our proposals which have lost in their slot
            our_proposal = self.replica_proposals[self.replica_slot_num]
            if our_proposal is not None and our_proposal != decided_proposal:
                self.propose(our_proposal)

            if decided_proposal in self.replica_decisions[:self.replica_slot_num]:
                continue  # duplicate
            self.logger.info("invoking %r" % (decided_proposal,))
            output = self.invoke(decided_proposal)
            if decided_proposal.caller:
                self.send([decided_proposal.caller], 'INVOKED',
                        cid=decided_proposal.cid, output=output)
            self.replica_slot_num += 1

    def do_GET_STATE(self, requester):
        if self.replica_ready:
            self.send([requester], 'SET_STATE',
                      state=self.replica_state,
                      slot_num=self.replica_slot_num)

    def do_SET_STATE(self, state, slot_num):
        if not self.replica_ready:
            self.replica_ready = True
            self.replica_state = state
            self.replica_slot_num = slot_num
            self.member_joined()


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
            self.acceptor_accepted[(ballot_num,slot)] = proposal
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
        self.scout_quorum = len(node.member_peers) / 2 + 1
        self.retransmit_timer = None

    def start(self):
        self.node.logger.info("scout starting")
        self.send_prepare()

    def send_prepare(self):
        self.node.send(self.node.member_peers, 'PREPARE',  # p1a
                       scout_id=self.scout_id,
                       ballot_num=self.scout_ballot_num)
        self.retransmit_timer = self.node.set_timer(self.PREPARE_RETRANSMIT, self.send_prepare)

    def finished(self, adopted, ballot_num):
        self.node.cancel_timer(self.retransmit_timer)
        self.node.logger.info("finished - adopted" if adopted else "finished - preempted")
        self.node.scout_finished(adopted, ballot_num, self.scout_pvals)

    def do_PROMISE(self, acceptor, ballot_num, accepted):  # p1b
        if ballot_num == self.scout_ballot_num:
            self.node.logger.info("got matching promise; need %d" % self.scout_quorum)
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
        self.commander_quorum = len(node.member_peers) / 2 + 1

    def start(self):
        self.node.send(self.node.member_peers, 'ACCEPT',  # p2a
                       commander_id=self.commander_id,
                       ballot_num=self.commander_ballot_num,
                       slot=self.commander_slot,
                       proposal=self.commander_proposal)

    def do_ACCEPTED(self, acceptor, ballot_num):  # p2b
        if ballot_num == self.commander_ballot_num:
            self.commander_accepted.add(acceptor)
            if len(self.commander_accepted) >= self.commander_quorum:
                self.node.send(self.node.member_peers, 'DECISION',
                               slot=self.commander_slot,
                               proposal=self.commander_proposal)
                self.node.commander_finished(self.commander_id, ballot_num, False)
        else:
            self.node.commander_finished(self.commander_id, ballot_num, True)


class Leader(deterministic_network.Node):

    HEARTBEAT_INTERVAL  = 1

    def __init__(self):
        super(Leader, self).__init__()
        self.leader_ballot_num = Ballot(0, self.unique_id)
        self.leader_active = False
        self.leader_last_heartbeat = 0
        self.leader_proposals = defaultlist()
        self.leader_commanders = {}
        self.leader_scout = None

    def leader_start(self):
        def heartbeat():
            if self.leader_active:
                if self.core.rnd.random() < 0.99:  # fail occasionally
                    self.send(self.member_peers, 'HEARTBEAT', leader=self.address)
            else:
                if self.leader_last_heartbeat < self.core.now - 2 * self.HEARTBEAT_INTERVAL:
                    if not self.leader_scout:
                        self.spawn_scout(self.leader_ballot_num)
            self.set_timer(self.HEARTBEAT_INTERVAL, heartbeat)
        heartbeat()

    def do_HEARTBEAT(self, leader):
        if not self.leader_active:
            self.leader_last_heartbeat = self.core.now

    def spawn_scout(self, ballot_num):
        assert not self.leader_scout
        def start():
            sct = self.leader_scout = Scout(self, ballot_num)
            sct.start()
        self.set_timer(self.core.rnd.uniform(0, 1), start)

    def scout_finished(self, adopted, ballot_num, pvals):
        self.leader_scout = None
        if adopted:
            # pvals is a defaultlist of (slot, proposal) by ballot num; we need the
            # highest ballot number for each slot.  TODO: this is super inefficient!
            last_by_slot = defaultlist()
            for b, s in reversed(sorted(pvals.keys())):
                p = pvals[b, s]
                if last_by_slot[s] is not None:
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
        if ballot_num > self.leader_ballot_num:
            self.logger.info("leader preempted by %s" % (ballot_num.leader,))
            self.leader_active = False
            self.leader_last_heartbeat = self.core.now
            self.leader_ballot_num = Ballot(ballot_num.n + 1, self.unique_id)

    def spawn_commander(self, ballot_num, slot, proposal):
        cmd = Commander(self, ballot_num, slot, proposal)
        if cmd.commander_id in self.leader_commanders:
            return
        self.leader_commanders[cmd.commander_id] = cmd
        cmd.start()

    def do_PROPOSE(self, slot, proposal):
        if self.leader_proposals[slot] is None:
            self.leader_proposals[slot] = proposal
            if self.leader_active:
                self.spawn_commander(self.leader_ballot_num, slot, proposal)

    def do_PROMISE(self, scout_id, acceptor, ballot_num, accepted):
        sct = self.leader_scout
        if sct and scout_id == sct.scout_id:
            sct.do_PROMISE(acceptor, ballot_num, accepted)

    def do_ACCEPTED(self, commander_id, acceptor, ballot_num):
        cmd = self.leader_commanders.get(commander_id)
        if cmd:
            cmd.do_ACCEPTED(acceptor, ballot_num)



class ClusterMember(Replica, Acceptor, Leader):

    def __init__(self, execute_fn, initial_state=None, peers=None):
        super(ClusterMember, self).__init__()
        self.replica_execute_fn = execute_fn
        self.member_initial_state = initial_state
        self.member_peers = peers

    def start(self):
        self.replica_start(initial_state=self.member_initial_state)

    def member_joined(self):
        self.leader_start()


class ClusterSeed(deterministic_network.Node):
    """A node which simply provides an initial state, and exits once a decision
    is made."""

    def __init__(self, initial_state):
        super(ClusterSeed, self).__init__()
        self.initial_state = initial_state

    def do_GET_STATE(self, requester):
        self.send([requester], 'SET_STATE',
                    state=self.initial_state, slot_num=0)

    def do_DECISION(self, slot, proposal):
        self.stop()
