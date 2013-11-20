import random
import logging
from bb_network import Member, Client, sequence_generator

# Fix in final copy:
#  - include repeated classes
#  - merge ClusterMember and Member
#  - remove logging stuff

def log_acceptor_state(fn):
    def wrap(self, **kwargs):
        round = kwargs['round']
        rv = fn(self, **kwargs)
        self.logger.debug("ACCEPTOR STATE@%d - %r" % (round, dict(
            last_accept=self.last_accepts[round],
            promise=self.promises[round])))
        return rv
    return wrap

def log_proposer_state(fn):
    def wrap(self, **kwargs):
        rv = fn(self, **kwargs)
        for round in range(0, len(self.values)):
            self.logger.debug("PROPOSER STATE@%d - %r" % (round, dict(
                active_proposal=self.active_proposals[round])))
        return rv
    return wrap

def log_learner_state(fn):
    def wrap(self, **kwargs):
        round = kwargs['round']
        rv = fn(self, **kwargs)
        self.logger.debug("LEARNER STATE@%d - %r" % (round, dict(
            accept_reqs_by_n=self.accept_reqs_by_n[round],
            values=self.values[round])))
        return rv
    return wrap


class defaultlist(list):

    def __getitem__(self, i):
        if i >= len(self):
            return None
        return list.__getitem__(self, i)

    def __setitem__(self, i, v):
        if i >= len(self):
            self.extend([None]*(i - len(self) + 1))
        list.__setitem__(self, i, v)


class ClusterMember(Member):

    cluster = []

    def __init__(self, execute_fn, address):
        Member.__init__(self, execute_fn, address)
        self.cluster.append(address)

        # acceptor
        self.last_accepts = defaultlist()
        self.promises = defaultlist()

        # proposer
        self.biggest_n = (0,0)
        self.active_proposals = defaultlist()

        # learner
        self.accept_reqs_by_n = defaultlist()
        self.last_invoked = -1
        self.waiting_clients = defaultlist()
        self.values = defaultlist()

    @property
    def quorum(self):
        return len(self.cluster)/2+1

    def next_n(self):
        n = (self.biggest_n[0]+1, self.pid)
        self.biggest_n = n
        return n[0]*65536+n[1]

    @log_proposer_state
    def do_INVOKE(self, input, caller):
        round = len(self.values)
        self.waiting_clients[round] = caller
        self.values.append(None)
        n =  self.next_n()
        quorum = random.sample(self.cluster, self.quorum)
        self.logger.info("beginning round %d with value %r and quorum %r" %
                (round, input, quorum))
        self.active_proposals[round] = dict(
                n=n, value=input, quorum=quorum, promises={}, have_accepted=False)
        self.send(quorum, 'PREPARE', proposer=self.address, n=n, round=round)
        # if this request fails, make_request will re-invoke us

    @log_acceptor_state
    def do_PREPARE(self, proposer, n, round):
        promise = self.promises[round]
        if promise is None or n > promise:
            self.promises[round] = n
            last_accept = self.last_accepts[round]
            prev_n = last_accept['n'] if last_accept else -1
            prev_value = last_accept['value'] if last_accept else None
            self.send([proposer], 'PROMISE', round=round, responder=self.address,
                    prev_n=prev_n, prev_value=prev_value)

    @log_proposer_state
    def do_PROMISE(self, round, responder, prev_n, prev_value):
        active_proposal = self.active_proposals[round]
        if not active_proposal or active_proposal['have_accepted']:
            return
        promises = active_proposal['promises']
        promises[responder] = dict(prev_n=prev_n, prev_value=prev_value)
        if len(promises) >= self.quorum:
            self.logger.info("received %d responses in round %d: %r" % (len(promises), round, promises))
            # find the value of the largest-numbered proposal returned to us, defaulting
            # to our own value if none is given
            value = active_proposal['value']
            largest_n = -1
            for prom in promises.values():
                if prom['prev_n'] > largest_n:
                    value, largest_n = prom['prev_value'], prom['prev_n']
            quorum = random.sample(self.cluster, self.quorum)
            self.send(quorum, 'PROPOSE', round=round, n=active_proposal['n'], value=value, proposer=self.address)

    @log_acceptor_state
    def do_PROPOSE(self, round, n, value, proposer):
        promise = self.promises[round]
        if n < promise:
            return
        last_accept = self.last_accepts[round]
        if not last_accept or last_accept['n'] < n:
            self.last_accepts[round] = {'n': n, 'value': value}
        self.send(self.cluster, 'ACCEPT', round=round, n=n, value=value)

    @log_learner_state
    def do_ACCEPT(self, round, n, value):
        accept_reqs_by_n = self.accept_reqs_by_n[round]
        if accept_reqs_by_n is None:
            accept_reqs_by_n = self.accept_reqs_by_n[round] = {}
        count = accept_reqs_by_n[n] = accept_reqs_by_n.get(n, 0) + 1
        if count < self.quorum:
            return
        self.logger.info("received %d accepts in round %d for proposal n=%d/value=%r; learning" % (count, round, n, value))
        self.learn(round, value)

    def learn(self, round, value):
        if self.values[round] != value:
            assert self.values[round] is None, "values[%d] is already %r" % (round, self.values[round])
            self.values[round] = value
        # catch up on the log as far as we can go, sending INVOKED messages
        # where necessary
        for i in xrange(self.last_invoked+1, len(self.values)):
            if self.values[i] is None:
                break
            output = self.invoke(self.values[i])
            self.last_invoked = i
            if self.waiting_clients[i]:
                self.send([self.waiting_clients[i]], 'INVOKED', output=output)


if __name__ == "__main__":
    logging.basicConfig(format="%(asctime)s %(name)s proc=%(processName)s %(message)s", level=logging.DEBUG)
    members = [ClusterMember(sequence_generator, address='memb%d' % i) for i in range(3)]
    client = Client('client', members[0].address)
    [member.start(initial_value=0) for member in members]
    print client.invoke(4)
    print client.invoke(1)
    [member.join() for member in members]
