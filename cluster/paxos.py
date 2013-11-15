import Queue
import threading
import logging
import random
import uuid

class defaultlist(list):

    def __getitem__(self, i):
        if i >= len(self):
            return None
        return list.__getitem__(self, i)

    def __setitem__(self, i, v):
        if i >= len(self):
            self.extend([None]*(i - len(self) + 1))
        list.__setitem__(self, i, v)

def log_acceptor_state(fn):
    def wrap(self, **kwargs):
        round = kwargs['round']
        rv = fn(self, **kwargs)
        logging.debug("ACCEPTOR STATE@%d - %r" % (round, dict(
            last_accept=self.last_accepts[round],
            promise=self.promises[round])))
        return rv
    return wrap

def log_proposer_state(fn):
    def wrap(self, **kwargs):
        round = kwargs['round']
        rv = fn(self, **kwargs)
        logging.debug("PROPOSER STATE@%d - %r" % (round, dict(
            active_proposal=self.active_proposals[round])))
        return rv
    return wrap

def log_learner_state(fn):
    def wrap(self, **kwargs):
        round = kwargs['round']
        rv = fn(self, **kwargs)
        with self._values_cond:
            logging.debug("LEARNER STATE@%d - %r" % (round, dict(
                accept_reqs_by_n=self.accept_reqs_by_n[round],
                values=self.values[round])))
        return rv
    return wrap

class Processor(threading.Thread):

    network = []
    N = 0
    _N_lock = threading.Lock()

    def __init__(self):
        # acceptor
        self.last_accepts = defaultlist()
        self.promises = defaultlist()

        # proposer
        self.active_proposals = defaultlist()

        # learner
        self.accept_reqs_by_n = defaultlist()
        self.values = defaultlist()
        self._values_cond = threading.Condition()

        # network
        Processor.network.append(self)
        self.queue = Queue.Queue()

        # thread
        threading.Thread.__init__(self)
        self.daemon = 1
        self.number = len(Processor.network)
        self.name = "Processor-%d" % self.number
        self.start()

    def run(self):
        try:
            while True:
                action, kwargs = self.queue.get()
                if not action:
                    break
                logging.debug("received %r with args %r" % (action, kwargs))
                getattr(self, 'do_%s' % action)(**kwargs)
        except Exception:
            logging.exception("Processor failed", exc_info=True)

    def join(self):
        self.queue.put((None, None))
        threading.Thread.join(self)

    def __repr__(self):
        return '[%s]' % (self.name,)

    def next_n(self):
        with Processor._N_lock:
            Processor.N += 1
            return Processor.N

    @property
    def quorum(self):
        return len(Processor.network)/2+1

    # external interface

    def make_request(self, value):
        # this can be called from any thread, and will block until the Processor
        # on which it is called learns the desired value.  We don't know in which
        # round the value will appear, so we watch them all.
        with self._values_cond:
            while value not in self.values:
                # pick a round number based on the values received so far.  If
                # this fails, that's because another proposal has won in that
                # round, so we'll see a change to the values array and try
                # again.  This adds an element to the values array to prevent
                # two instances from requesting the same round on the same processor.
                round=len(self.values)
                self.values[round] = None
                self.send(self, 'REQUEST', value=value, round=round)
                self._values_cond.wait()
                if self.values[round] is not None and self.values[round] != value:
                    logging.warning("request failed at round %d; retrying" % round)
            return self.values.index(value)

    # msssages

    def send(self, destinations, action, **kwargs):
        if isinstance(destinations, Processor):
            destinations = [destinations]
        for dest in destinations:
            dest.queue.put((action, kwargs))

    # actions

    @log_proposer_state
    def do_REQUEST(self, value, round):
        n =  self.next_n()
        quorum = random.sample(Processor.network, self.quorum)
        logging.info("beginning round %d with value %r and quorum %r" %
                (round, value, [p.name for p in quorum]))
        self.active_proposals[round] = dict(
                n=n, value=value, quorum=quorum, promises={}, have_accepted=False)
        self.send(quorum, 'PREPARE', proposer=self, n=n, round=round)
        # if this request fails, make_request will re-invoke us

    @log_acceptor_state
    def do_PREPARE(self, proposer, n, round):
        promise = self.promises[round]
        if promise is None or n > promise:
            self.promises[round] = n
            last_accept = self.last_accepts[round]
            prev_n = last_accept['n'] if last_accept else -1
            prev_value = last_accept['value'] if last_accept else None
            self.send(proposer, 'PROMISE', round=round, responder=self,
                    prev_n=prev_n, prev_value=prev_value)

    @log_proposer_state
    def do_PROMISE(self, round, responder, prev_n, prev_value):
        active_proposal = self.active_proposals[round]
        if not active_proposal or active_proposal['have_accepted']:
            return
        promises = active_proposal['promises']
        promises[responder] = dict(prev_n=prev_n, prev_value=prev_value)
        if len(promises) >= self.quorum:
            logging.info("received %d responses in round %d: %r" % (len(promises), round, promises))
            # find the value of the largest-numbered proposal returned to us, defaulting
            # to our own value if none is given
            value = active_proposal['value']
            largest_n = -1
            for prom in promises.values():
                if prom['prev_n'] > largest_n:
                    value, largest_n = prom['prev_value'], prom['prev_n']
            quorum = random.sample(Processor.network, self.quorum)
            self.send(quorum, 'PROPOSE', round=round, n=active_proposal['n'], value=value, proposer=self)

    @log_acceptor_state
    def do_PROPOSE(self, round, n, value, proposer):
        promise = self.promises[round]
        if n < promise:
            return
        last_accept = self.last_accepts[round]
        if not last_accept or last_accept['n'] < n:
            self.last_accepts[round] = {'n': n, 'value': value}
        self.send(Processor.network, 'ACCEPT', round=round, n=n, value=value)

    @log_learner_state
    def do_ACCEPT(self, round, n, value):
        accept_reqs_by_n = self.accept_reqs_by_n[round]
        if accept_reqs_by_n is None:
            accept_reqs_by_n = self.accept_reqs_by_n[round] = {}
        count = accept_reqs_by_n[n] = accept_reqs_by_n.get(n, 0) + 1
        if count < self.quorum:
            return
        logging.info("received %d accepts in round %d for proposal n=%d/value=%r; learning" % (count, round, n, value))
        with self._values_cond:
            if self.values[round] != value:
                assert self.values[round] is None, "values[%d] is already %r" % (round, self.values[round])
                self.values[round] = value
                self._values_cond.notify()


def main():
    logging.basicConfig(format="%(asctime)s %(threadName)s %(message)s", level=logging.DEBUG)
    # logging to a file for line-by-line inspection
    if False:
        fmtr = logging.Formatter("%(asctime)s %(threadName)s %(message)s")
        hdlr = logging.FileHandler('paxos.log', 'w')
        hdlr.setFormatter(fmtr)
        logging.getLogger('').addHandler(hdlr)
    processors = [ Processor() for _ in range(10) ]
    reqs = [uuid.uuid4().hex for x in range(15)]
    values = defaultlist()
    def make_req(r):
        proc = random.choice(processors)
        round = proc.make_request(r)
        values[round] = r
    req_thds = [threading.Thread(target=make_req, args=(r,)) for r in reqs]
    for t in req_thds:
        t.start()
    for t in req_thds:
        t.join()
    for p in processors:
        p.join()
    print filter(None, values)
    assert len(filter(None, values)) == len(reqs)

main()
