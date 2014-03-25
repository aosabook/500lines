import Queue
import threading
from . import request
from . import member
from . import member_replicated
from . import network


class Listener(member.Component):

    def __init__(self, member, event_queue):
        super(Listener, self).__init__(member)
        self.event_queue = event_queue

    def on_view_change_event(self, slot, view_id, peers):
        self.event_queue.put(("membership_change", peers))


class Ship(object):

    def __init__(self, state_machine, port=10001, peers=None, seed=None):
        peers = peers or ['255.255.255.255-%d' % port]
        self.node = network.Node(port)
        if seed is not None:
            self.cluster_member = member_replicated.ClusterSeed(
                self.node, seed)
        else:
            self.cluster_member = member_replicated.ClusterMember(
                self.node, state_machine, peers=peers)
        self.event_queue = Queue.Queue()
        self.current_request = None
        self.listener = Listener(self.cluster_member, self.event_queue)

    def start(self):
        def run():
            self.cluster_member.start()
            self.node.run()

        self.thread = threading.Thread(target=run)
        self.thread.setDaemon(1)
        self.thread.start()

    def invoke(self, input_value):
        assert self.current_request is None
        q = Queue.Queue()

        def done(output):
            self.current_request = None
            q.put(output)
        self.current_request = request.Request(self.cluster_member, input_value, done)
        self.current_request.start()
        return q.get()

    def events(self):
        while True:
            evt = self.event_queue.get()
            yield evt
