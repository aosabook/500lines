from itertools import cycle
from . import JOIN_RETRANSMIT, Join
from .network import Component
from .replica import Replica
from .acceptor import Acceptor
from .leader import Leader
from .scout import Scout
from .commander import Commander


class Bootstrap(Component):

    def __init__(self, node, peers, execute_fn,
                 replica_cls=Replica, acceptor_cls=Acceptor, leader_cls=Leader,
                 commander_cls=Commander, scout_cls=Scout):
        super(Bootstrap, self).__init__(node)
        self.execute_fn = execute_fn
        self.peers = peers
        self.peers_cycle = cycle(peers)
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
        self.send([next(self.peers_cycle)], Join())
        self.timer = self.set_timer(JOIN_RETRANSMIT, self.join)

    def do_WELCOME(self, sender, state, slot_num, decisions):
        self.acceptor_cls(self.node)
        replica = self.replica_cls(self.node, execute_fn=self.execute_fn)
        leader = self.leader_cls(self.node, unique_id=self.node.unique_id,
                                 peers=self.peers,
                                 commander_cls=self.commander_cls,
                                 scout_cls=self.scout_cls)
        leader.start()
        # TODO: just pass these to the constructor
        replica.start(state=state, slot_num=slot_num,
                            decisions=decisions, peers=self.peers)

        if self.timer:
            self.timer.cancel()

        self.stop()
