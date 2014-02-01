from util import defaultlist
from member import Component
from replica import Replica

class Seed(Component):

    """A component which simply provides an initial state and view.  It waits
    until it has heard JOIN requests from enough nodes to form a cluster, then
    WELCOMEs them all to the same view with the given initial state."""

    def __init__(self, member, initial_state):
        super(Seed, self).__init__(member)
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

