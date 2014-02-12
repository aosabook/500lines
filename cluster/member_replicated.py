from member import Member
from replica import Replica
from heartbeat import Heartbeat
from acceptor import Acceptor
from leader import Leader
from scout import Scout
from commander import Commander
from seed import Seed


class ClusterMember(Member):

    def __init__(self, node, execute_fn, peers,
                 replica_cls=Replica, acceptor_cls=Acceptor, leader_cls=Leader,
                 commander_cls=Commander, scout_cls=Scout, heartbeat_cls=Heartbeat):
        super(ClusterMember, self).__init__(node)
        self.replica = replica_cls(self, execute_fn, peers)
        self.acceptor = acceptor_cls(self)
        self.leader = leader_cls(self, node.unique_id, commander_cls=commander_cls, scout_cls=scout_cls)
        self.heartbeat = heartbeat_cls(self)
 
    def start(self):
        self.replica.start()


class ClusterSeed(Member):

    def __init__(self, node, initial_state, seed_cls=Seed):
        super(ClusterSeed, self).__init__(node)
        self.seed = seed_cls(self, initial_state)
