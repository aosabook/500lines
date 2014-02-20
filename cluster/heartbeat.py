import protocol
from member import Component


class Heartbeat(Component):

    def __init__(self, member, clock):
        super(Heartbeat, self).__init__(member)
        self.running = False
        self.last_heard_from = {}
        self.peers = None
        self.clock = clock

    def on_view_change_event(self, viewchange):
        self.peers = set(viewchange.peers)
        for peer in self.peers:
            self.last_heard_from[peer] = self.clock()
        if not self.running:
            self.heartbeat()
            self.running = True

    def do_HEARTBEAT(self, sender):
        self.last_heard_from[sender] = self.clock()

    def heartbeat(self):
        # send heartbeats to other nodes
        self.send(self.last_heard_from.keys(), 'HEARTBEAT', sender=self.address)

        # determine if any peers are down, and notify if so; note that this
        # notification will occur repeatedly until a view change
        too_old = self.clock() - 2 * protocol.HEARTBEAT_INTERVAL
        active_peers = set(p for p in self.last_heard_from if self.last_heard_from[p] >= too_old)
        if active_peers != self.peers:
            self.event('peers_down', down=self.peers - active_peers)

        self.set_timer(protocol.HEARTBEAT_INTERVAL, self.heartbeat)
