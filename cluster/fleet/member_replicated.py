from .member import Member
from .replica import Replica
from .heartbeat import Heartbeat
from .bootstrap import Bootstrap
from .acceptor import Acceptor
from .leader import Leader
from .scout import Scout
from .commander import Commander
from .seed import Seed


class ClusterMember(Member):

    def __init__(self, node, execute_fn, peers,
                 replica_cls=Replica, acceptor_cls=Acceptor, leader_cls=Leader,
                 commander_cls=Commander, scout_cls=Scout, heartbeat_cls=Heartbeat,
                 bootstrap_cls=Bootstrap):
        super(ClusterMember, self).__init__(node)
        # only start the bootstrap component initially, then hand off to the rest

        def bootstrapped(state, slot_num, decisions, view_id, peers, peer_history):
            self.replica = replica_cls(self, execute_fn)
            self.acceptor = acceptor_cls(self)
            self.leader = leader_cls(self, node.unique_id, peer_history, commander_cls=commander_cls, scout_cls=scout_cls)
            self.heartbeat = heartbeat_cls(self, node.now)
            # start up the replica, now that its information is ready
            self.replica.start(state, slot_num, decisions, view_id, peers, peer_history)

        self.bootstrap = bootstrap_cls(self, peers, bootstrapped)

    def start(self):
        self.bootstrap.start()


class ClusterSeed(Member):

    def __init__(self, node, initial_state, seed_cls=Seed):
        super(ClusterSeed, self).__init__(node)
        self.seed = seed_cls(self, initial_state)
