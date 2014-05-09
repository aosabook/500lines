from . import JOIN_RETRANSMIT
from .member import Component


class Seed(Component):

    """A component which simply provides an initial state.  It waits
    until it has heard JOIN requests from enough nodes to form a cluster, then
    WELCOMEs them all with the given initial state."""

    def __init__(self, member, initial_state, peers):
        super(Seed, self).__init__(member)
        self.initial_state = initial_state
        self.peers = peers
        self.seen_peers = set([])
        self.exit_timer = None

    def do_JOIN(self, requester):
        self.seen_peers.add(requester)
        if len(self.seen_peers) <= len(self.peers) / 2:
            return

        # cluster is ready - welcome everyone
        self.send(list(self.seen_peers), 'WELCOME',
                  state=self.initial_state,
                  slot_num=1,
                  decisions={},
                  peers=self.peers)

        # stick around for long enough that we don't hear any new JOINs from
        # the newly formed cluster
        if self.exit_timer:
            self.exit_timer.cancel()
        self.exit_timer = self.set_timer(JOIN_RETRANSMIT * 2, self.stop)
