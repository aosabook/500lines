import Queue
import threading
from . import request
from . import bootstrap
from . import seed


class Member(object):

    def __init__(self, state_machine, network, peers, seed=None,
                 seed_cls=seed.Seed, bootstrap_cls=bootstrap.Bootstrap):
        self.network = network
        self.node = network.new_node()
        if seed is not None:
            self.component = seed_cls(
                self.node, initial_state=seed, peers=peers, execute_fn=state_machine)
        else:
            self.component = bootstrap_cls(
                self.node, execute_fn=state_machine, peers=peers)
        self.current_request = None

    def start(self):
        def run():
            self.component.start()
            self.network.run()

        self.thread = threading.Thread(target=run)
        self.thread.setDaemon(1)
        self.thread.start()

    def invoke(self, input_value, request_cls=request.Request):
        assert self.current_request is None
        q = Queue.Queue()

        def done(output):
            self.current_request = None
            q.put(output)
        self.current_request = request_cls(self.node, input_value, done)
        self.current_request.start()
        return q.get()
