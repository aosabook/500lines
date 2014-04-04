import Queue
import threading
from . import request
from . import member_replicated
from . import network


class Ship(object):

    def __init__(self, state_machine, port=10001, peers=None, seed=None,
            node_cls=network.Node, clusterseed_cls=member_replicated.ClusterSeed,
            clustermember_cls=member_replicated.ClusterMember):
        peers = peers or ['255.255.255.255-%d' % port]
        self.node = node_cls(port)
        if seed is not None:
            self.cluster_member = clusterseed_cls(self.node, initial_state=seed,
                                                  peers=peers)
        else:
            self.cluster_member = clustermember_cls(self.node,
                                                    execute_fn=state_machine,
                                                    peers=peers)
        self.current_request = None

    def start(self):
        def run():
            self.cluster_member.start()
            self.node.run()

        self.thread = threading.Thread(target=run)
        self.thread.setDaemon(1)
        self.thread.start()

    def invoke(self, input_value, request_cls=request.Request):
        assert self.current_request is None
        q = Queue.Queue()

        def done(output):
            self.current_request = None
            q.put(output)
        self.current_request = request_cls(self.cluster_member, input_value, done)
        self.current_request.start()
        return q.get()
