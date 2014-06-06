from collections import defaultdict
from collections import namedtuple
import functools
import heapq
import itertools
import logging
import Queue
import random
import threading
import time

# data types
Proposal = namedtuple('Proposal', ['caller', 'client_id', 'input'])
Ballot = namedtuple('Ballot', ['n', 'leader'])
ScoutId = namedtuple('ScoutId', ['address', 'ballot_num'])
CommanderId = namedtuple('CommanderId', ['address', 'slot', 'proposal'])

# message types
Accepted = namedtuple('Accepted', ['commander_id', 'acceptor', 'ballot_num'])
Accept = namedtuple('Accept', ['commander_id', 'ballot_num', 'slot', 'proposal'])
Catchup = namedtuple('Catchup', ['slot'])
Decision = namedtuple('Decision', ['slot', 'proposal'])
Invoked = namedtuple('Invoked', ['client_id', 'output'])
Invoke = namedtuple('Invoke', ['caller', 'client_id', 'input_value'])
Join = namedtuple('Join', [])
Active = namedtuple('Active', [])
Prepare = namedtuple('Prepare', ['scout_id', 'ballot_num'])
Promise = namedtuple('Promise', ['scout_id', 'acceptor', 'ballot_num', 'accepted'])
Propose = namedtuple('Propose', ['slot', 'proposal'])
Welcome = namedtuple('Welcome', ['state', 'slot_num', 'decisions'])

# constants - all of these should really be in terms of RTT's
JOIN_RETRANSMIT = 0.7
CATCHUP_INTERVAL = 0.6
ACCEPT_RETRANSMIT = 1.0
PREPARE_RETRANSMIT = 1.0
INVOKE_RETRANSMIT = 0.5
LEADER_TIMEOUT = 1.0

NULL_BALLOT = Ballot(-1, -1)  # sorts before all real ballots
NOOP_PROPOSAL = Proposal(None, None, None)  # no-op to fill otherwise empty slots

class NetworkLogger(logging.LoggerAdapter):

    def process(self, msg, kwargs):
        return "T=%.3f %s" % (self.extra['network'].now, msg), kwargs

    def getChild(self, name):
        return self.__class__(self.logger.getChild(name),
                              {'network': self.extra['network']})


class Component(object):

    def __init__(self, node):
        self.node = node
        self.node.register(self)
        self.logger = node.logger.getChild(type(self).__name__)

    def stop(self):
        self.node.unregister(self)


class Node(object):
    unique_ids = itertools.count()

    def __init__(self, network, address):
        self.network = network
        self.unique_id = next(self.unique_ids)
        self.address = address or 'N%d' % self.unique_id
        self.components = []
        self.logger = NetworkLogger(
            logging.getLogger(self.address), {'network': self.network})
        self.set_timer = functools.partial(self.network.set_timer, self.address)
        self.send = functools.partial(self.network.send, self)
        self.logger.info('starting')

    def register(self, component):
        self.components.append(component)

    def unregister(self, component):
        self.components.remove(component)

    def receive(self, sender, message):
        handler_name = 'do_%s' % type(message).__name__.upper()

        for comp in self.components[:]:
            if not hasattr(comp, handler_name):
                continue
            comp.logger.debug("received %s from %s", message, sender)
            fn = getattr(comp, handler_name)
            fn(sender=sender, **message._asdict())

    def event(self, message, **kwargs):
        method = 'on_' + message + '_event'
        for comp in self.components:
            if hasattr(comp, method):
                getattr(comp, method)(**kwargs)


class Timer(object):

    def __init__(self, expires, address, callback):
        self.expires = expires
        self.address = address
        self.callback = callback
        self.cancelled = False

    def __cmp__(self, other):
        return cmp(self.expires, other.expires)

    def cancel(self):
        self.cancelled = True


class Network(object):
    PROP_DELAY = 0.03
    PROP_JITTER = 0.02
    DROP_PROB = 0.05

    def __init__(self, seed):
        self.nodes = {}
        self.rnd = random.Random(seed)
        self.timers = []
        self.now = 1000.0

    def new_node(self, address=None):
        node = Node(self, address=address)
        self.nodes[node.address] = node
        return node

    def run(self):
        while self.timers:
            next_timer = self.timers[0]
            if next_timer.expires > self.now:
                self.now = next_timer.expires
            heapq.heappop(self.timers)
            if next_timer.cancelled:
                continue
            if not next_timer.address or next_timer.address in self.nodes:
                next_timer.callback()

    def stop(self):
        self.timers = []

    def set_timer(self, address, seconds, callback):
        timer = Timer(self.now + seconds, address, callback)
        heapq.heappush(self.timers, timer)
        return timer

    def send(self, sender, destinations, message):
        sender.logger.debug("sending %s to %s", message, destinations)
        for dest in (d for d in destinations if d in self.nodes):
            if self.rnd.uniform(0, 1.0) > self.DROP_PROB:
                delay = self.PROP_DELAY + self.rnd.uniform(-self.PROP_JITTER, self.PROP_JITTER)
                self.set_timer(dest, delay, functools.partial(self.nodes[dest].receive,
                                                              sender.address, message))


class Replica(Component):

    def __init__(self, node, execute_fn):
        super(Replica, self).__init__(node)
        self.execute_fn = execute_fn
        self.proposals = {}

    def start(self, state, slot_num, decisions, peers):
        self.state = state
        self.slot_num = slot_num
        # next slot num for a proposal (may lead slot_num)
        self.next_slot = slot_num
        self.decisions = decisions.copy()
        self.peers = peers
        self.latest_leader = None
        self.latest_leader_timeout = None

        # TODO: Can be replaced with 'assert slot_num not in self._decisions'
        # if decision value cannot be None
        assert decisions.get(slot_num) is None

        self.catchup()

    # making proposals

    def do_INVOKE(self, sender, caller, client_id, input_value):
        proposal = Proposal(caller, client_id, input_value)
        if proposal not in self.proposals.viewvalues():
            self.propose(proposal)
        else:
            # It's the only drawback of using dict instead of defaultlist
            slot = next(s for s, p in self.proposals.iteritems()
                        if p == proposal)
            self.logger.info(
                "proposal %s already proposed in slot %d", proposal, slot)

    def propose(self, proposal, slot=None):
        """Send (or resend, if slot is specified) a proposal to the leader"""
        if not slot:
            slot, self.next_slot = self.next_slot, self.next_slot + 1
        self.proposals[slot] = proposal
        # find a leader we think is working - either the latest we know of, or
        # ourselves (which may trigger a scout to make us the leader)
        leader = self.latest_leader or self.node.address
        self.logger.info("proposing %s at slot %d to leader %s" % (proposal, slot, leader))
        self.node.send([leader], Propose(slot=slot, proposal=proposal))

    # catching up with the rest of the cluster

    def catchup(self):
        """Try to catch up on un-decided slots"""
        # TODO: some way to gossip about `next_slot` with other replicas
        if self.slot_num != self.next_slot:
            self.logger.debug("catching up on %d .. %d" %
                              (self.slot_num, self.next_slot - 1))
        for slot in xrange(self.slot_num, self.next_slot):
            if slot in self.decisions:
                continue
            # ask peers for information regardless
            self.node.send(self.peers, Catchup(slot=slot))
            # TODO: Can be replaced with 'if slot in self._proposals and slot not in self._decisions'
            # TODO: if proposal value cannot be None
            if self.proposals.get(slot):
                # resend a proposal we initiated
                self.propose(self.proposals[slot], slot)
            else:
                # make an empty proposal in case nothing has been decided
                self.propose(NOOP_PROPOSAL, slot)
        self.node.set_timer(CATCHUP_INTERVAL, self.catchup)

    def do_CATCHUP(self, sender, slot):
        # if we have a decision for this proposal, spread the knowledge
        # TODO: Can be replaced with 'if slot in self._decisions' if decision
        # value cannot be None
        if self.decisions.get(slot):
            self.node.send([sender], Decision(slot=slot, proposal=self.decisions[slot]))

    # handling decided proposals

    def do_DECISION(self, sender, slot, proposal):
        # TODO: Can be replaced with 'if slot in self._decisions' if decision
        # value cannot be None
        if self.decisions.get(slot) is not None:
            assert self.decisions[slot] == proposal, \
                "slot %d already decided: %r!" % (
                    slot, self.decisions[slot])
            return
        self.decisions[slot] = proposal
        self.next_slot = max(self.next_slot, slot + 1)

        # re-propose our proposal in a new slot if it lost its slot
        our_proposal = self.proposals.get(slot)
        if our_proposal is not None and our_proposal != proposal:
            self.propose(our_proposal)

        # execute any pending, decided proposals, eliminating duplicates
        while True:
            commit_proposal = self.decisions.get(self.slot_num)
            if not commit_proposal:
                break  # not decided yet
            commit_slot, self.slot_num = self.slot_num, self.slot_num + 1

            self.commit(commit_slot, commit_proposal)

    def on_decision_event(self, slot, proposal):
        self.do_DECISION(sender=self.node.address, slot=slot, proposal=proposal)

    def commit(self, slot, proposal):
        """Actually commit a proposal that is decided and in sequence"""
        decided_proposals = [p for s,
                             p in self.decisions.iteritems() if s < slot]
        if proposal in decided_proposals:
            self.logger.info(
                "not committing duplicate proposal %r at slot %d", proposal, slot)
            return  # duplicate

        self.logger.info("committing %r at slot %d" % (proposal, slot))
        self.node.event('commit', slot=slot, proposal=proposal)

        if proposal.caller is not None:
            # perform a client operation
            self.state, output = self.execute_fn(self.state, proposal.input)
            self.node.send([proposal.caller], Invoked(client_id=proposal.client_id, output=output))

    # tracking the leader
    def on_leader_changed_event(self, new_leader):
        self.latest_leader = new_leader
        self.leader_alive()

    def do_ACTIVE(self, sender):
        if sender != self.latest_leader:
            return
        self.leader_alive()

    def leader_alive(self):
        if self.latest_leader_timeout:
            self.latest_leader_timeout.cancel()

        def reset_leader():
            idx = self.peers.index(self.latest_leader)
            self.latest_leader = self.peers[(idx + 1) % len(self.peers)]
            self.logger.debug(
                "leader timed out; defaulting to the next one, %s", self.latest_leader)
        self.latest_leader_timeout = self.node.set_timer(LEADER_TIMEOUT, reset_leader)

    # adding new cluster members

    def do_JOIN(self, sender):
        if sender in self.peers:
            self.node.send([sender], Welcome(
                state=self.state, slot_num=self.slot_num, decisions=self.decisions))


class Acceptor(Component):

    def __init__(self, node):
        super(Acceptor, self).__init__(node)
        self.ballot_num = NULL_BALLOT
        self.accepted = defaultdict()  # { (b, s) : p }

    def do_PREPARE(self, sender, scout_id, ballot_num):  # p1a
        if ballot_num > self.ballot_num:
            self.ballot_num = ballot_num
            # we've accepted the sender, so it might be the next leader
            self.node.event('leader_changed', new_leader=sender)

        self.node.send([scout_id.address], Promise(
            scout_id=scout_id, acceptor=self.node.address,
            ballot_num=self.ballot_num, accepted=self.accepted))

    # p2a
    def do_ACCEPT(self, sender, commander_id, ballot_num, slot, proposal):
        if ballot_num >= self.ballot_num:
            self.ballot_num = ballot_num
            self.accepted[(ballot_num, slot)] = proposal

        self.node.send([commander_id.address], Accepted(
            commander_id=commander_id, acceptor=self.node.address, ballot_num=self.ballot_num))


class Commander(Component):

    def __init__(self, node, leader, ballot_num, slot, proposal, commander_id, peers):
        super(Commander, self).__init__(node)
        self.leader = leader
        self.ballot_num = ballot_num
        self.slot = slot
        self.proposal = proposal
        self.commander_id = commander_id
        self.accepted = set([])
        self.peers = peers
        self.quorum = len(peers) / 2 + 1
        self.timer = None

    def start(self):
        self.node.send(set(self.peers) - self.accepted, Accept(
                            commander_id=self.commander_id, ballot_num=self.ballot_num,
                            slot=self.slot, proposal=self.proposal))
        self.timer = self.node.set_timer(ACCEPT_RETRANSMIT, self.start)

    def finished(self, ballot_num, preempted):
        self.leader.commander_finished(
            self.commander_id, ballot_num, preempted)
        if self.timer:
            self.timer.cancel()
        self.stop()

    def do_ACCEPTED(self, sender, commander_id, acceptor, ballot_num):  # p2b
        if commander_id != self.commander_id:
            return
        if ballot_num == self.ballot_num:
            self.accepted.add(acceptor)
            if len(self.accepted) < self.quorum:
                return
            # make sure that this node hears about the decision, otherwise the
            # slot can get "stuck" if all of the DECISION messages get lost
            self.node.event('decision', slot=self.slot, proposal=self.proposal)
            self.node.send(self.peers, Decision(slot=self.slot, proposal=self.proposal))
            self.finished(ballot_num, False)
        else:
            self.finished(ballot_num, True)


class Scout(Component):

    def __init__(self, node, leader, ballot_num, peers):
        super(Scout, self).__init__(node)
        self.leader = leader
        self.scout_id = ScoutId(self.node.address, ballot_num)
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
        self.node.send(self.peers, Prepare(
            scout_id=self.scout_id,
            ballot_num=self.ballot_num))
        self.retransmit_timer = self.node.set_timer(PREPARE_RETRANSMIT, self.send_prepare)

    def finished(self, adopted, ballot_num):
        self.logger.info(
            "finished - adopted" if adopted else "finished - preempted")
        self.leader.scout_finished(adopted, ballot_num, self.pvals)
        self.retransmit_timer.cancel()
        self.stop()

    # p1b
    def do_PROMISE(self, sender, scout_id, acceptor, ballot_num, accepted):
        if scout_id != self.scout_id:
            return
        if ballot_num == self.ballot_num:
            self.logger.info("got matching promise; need %d" % self.quorum)
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


class Leader(Component):

    def __init__(self, node, unique_id, peers, commander_cls=Commander, scout_cls=Scout):
        super(Leader, self).__init__(node)
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
                self.node.send(self.peers, Active())
            self.node.set_timer(LEADER_TIMEOUT / 2.0, active)
        active()

    def spawn_scout(self):
        assert not self.scout
        sct = self.scout = self.scout_cls(
            self.node, self, self.ballot_num, self.peers)
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
            self.node.event('leader_changed', new_leader=self.node.address)
            self.active = True
        else:
            self.preempted(ballot_num)

    def spawn_commander(self, ballot_num, slot):
        proposal = self.proposals[slot]
        commander_id = CommanderId(self.node.address, slot, proposal)
        assert commander_id not in self.commanders
        cmd = self.commander_cls(self.node,
                                 self, ballot_num, slot, proposal, commander_id, self.peers)
        self.commanders[commander_id] = cmd
        cmd.start()

    def commander_finished(self, commander_id, ballot_num, preempted):
        del self.commanders[commander_id]
        if preempted:
            self.preempted(ballot_num)

    def preempted(self, ballot_num):
        self.logger.info("leader preempted by %s, but I'm %d" %
                         (ballot_num.leader, self.ballot_num.leader))
        self.active = False
        self.ballot_num = Ballot((ballot_num or self.ballot_num).n + 1, self.ballot_num.leader)

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


class Bootstrap(Component):

    def __init__(self, node, peers, execute_fn,
                 replica_cls=Replica, acceptor_cls=Acceptor, leader_cls=Leader,
                 commander_cls=Commander, scout_cls=Scout):
        super(Bootstrap, self).__init__(node)
        self.execute_fn = execute_fn
        self.peers = peers
        self.peers_cycle = itertools.cycle(peers)
        self.timer = None
        self.replica_cls = replica_cls
        self.acceptor_cls = acceptor_cls
        self.leader_cls = leader_cls
        self.commander_cls = commander_cls
        self.scout_cls = scout_cls

    def start(self):
        self.join()

    def join(self):
        """Try to join the cluster"""
        self.node.send([next(self.peers_cycle)], Join())
        self.timer = self.node.set_timer(JOIN_RETRANSMIT, self.join)

    def do_WELCOME(self, sender, state, slot_num, decisions):
        self.acceptor_cls(self.node)
        replica = self.replica_cls(self.node, execute_fn=self.execute_fn)
        leader = self.leader_cls(self.node, unique_id=self.node.unique_id,
                                 peers=self.peers, commander_cls=self.commander_cls,
                                 scout_cls=self.scout_cls)
        leader.start()
        # TODO: just pass these to the constructor
        replica.start(state=state, slot_num=slot_num,
                      decisions=decisions, peers=self.peers)

        if self.timer:
            self.timer.cancel()

        self.stop()


class Seed(Component):

    def __init__(self, node, initial_state, execute_fn, peers, bootstrap_cls=Bootstrap):
        super(Seed, self).__init__(node)
        self.initial_state = initial_state
        self.execute_fn = execute_fn
        self.peers = peers
        self.bootstrap_cls = bootstrap_cls
        self.seen_peers = set([])
        self.exit_timer = None

    def do_JOIN(self, sender):
        self.seen_peers.add(sender)
        if len(self.seen_peers) <= len(self.peers) / 2:
            return

        # cluster is ready - welcome everyone
        self.node.send(list(self.seen_peers), Welcome(
            state=self.initial_state, slot_num=1, decisions={}))

        # stick around for long enough that we don't hear any new JOINs from
        # the newly formed cluster
        if self.exit_timer:
            self.exit_timer.cancel()
        self.exit_timer = self.node.set_timer(JOIN_RETRANSMIT * 2, self.finish)

    def finish(self):
        # hand over this node to a bootstrap component
        bs = self.bootstrap_cls(self.node, peers=self.peers, execute_fn=self.execute_fn)
        bs.start()
        self.stop()


class Request(Component):

    client_ids = itertools.count(start=100000)

    def __init__(self, node, n, callback):
        super(Request, self).__init__(node)
        self.client_id = self.client_ids.next()
        self.n = n
        self.output = None
        self.callback = callback

    def start(self):
        self.node.send([self.node.address], Invoke(caller=self.node.address,
                                                   client_id=self.client_id, input_value=self.n))
        self.invoke_timer = self.node.set_timer(INVOKE_RETRANSMIT, self.start)

    def do_INVOKED(self, sender, client_id, output):
        if client_id != self.client_id:
            return
        self.logger.debug("received output %r" % (output,))
        self.invoke_timer.cancel()
        self.callback(output)
        self.stop()


class Member(object):

    def __init__(self, state_machine, network, peers, seed=None,
                 seed_cls=Seed, bootstrap_cls=Bootstrap):
        self.network = network
        self.node = network.new_node()
        if seed is not None:
            self.component = seed_cls(self.node, initial_state=seed, peers=peers,
                                      execute_fn=state_machine)
        else:
            self.component = bootstrap_cls(self.node, execute_fn=state_machine, peers=peers)
        self.current_request = None

    def start(self):
        self.component.start()
        self.thread = threading.Thread(target=self.network.run)
        self.thread.start()

    def invoke(self, input_value, request_cls=Request):
        assert self.current_request is None
        q = Queue.Queue()

        self.current_request = request_cls(self.node, input_value, q.put)
        self.current_request.start()
        output = q.get()
        self.current_request = None
        return output
