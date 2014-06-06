from . import JOIN_RETRANSMIT, Welcome
from .bootstrap import Bootstrap
from .network import Component


class Seed(Component):

    """A component which simply provides an initial state.  It waits
    until it has heard JOIN requests from enough nodes to form a cluster, then
    WELCOMEs them all with the given initial state."""

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
        self.send(list(self.seen_peers), Welcome(
                  state=self.initial_state,
                  slot_num=1,
                  decisions={}))

        # stick around for long enough that we don't hear any new JOINs from
        # the newly formed cluster
        if self.exit_timer:
            self.exit_timer.cancel()
        self.exit_timer = self.set_timer(JOIN_RETRANSMIT * 2, self.finish)

    def finish(self):
        # hand over this node to a bootstrap component
        bs = self.bootstrap_cls(self.node, peers=self.peers, execute_fn=self.execute_fn)
        bs.start()
        self.stop()
