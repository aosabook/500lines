import logging
from multiprocessing import Process, Queue

# Remove from final copy:
#  - logging

network = {}

class Node(object):

    def __init__(self, address):
        self.q = Queue()
        self.address = address
        self.logger = logging.getLogger('node.%s' % address)
        network[self.address] = self.q

    def send(self, destinations, action, **kwargs):
        self.logger.debug("sending %s with args %s to %s" % (action, kwargs, destinations))
        for dest in destinations:
            network[dest].put((action, kwargs))


class Member(Node, Process):

    def __init__(self, execute_fn, address):
        Node.__init__(self, address)
        Process.__init__(self, name=address)
        self.execute_fn = execute_fn

    def run(self):
        while True:
            action, kwargs = self.q.get()
            if not action:
                return
            self.logger.debug("received %r with args %r" % (action, kwargs))
            getattr(self, 'do_%s' % action)(**kwargs)

    def join(self):
        self.q.put((None, None))
        Process.join(self)

    def start(self, initial_value=None):
        self.state = initial_value
        Process.start(self)

    def invoke(self, input):
        self.state, output = self.execute_fn(self.state, input)
        return output

    def do_INVOKE(self, input, caller):
        self.send([caller], 'INVOKED', output=self.invoke(input))


class Client(Node):

    def __init__(self, address, member_address):
        Node.__init__(self, address)
        self.member_address = member_address

    def invoke(self, input):
        self.send([self.member_address], 'INVOKE', input=input, caller=self.address)
        action, kwargs = self.q.get()
        self.logger.debug("received %r with args %r" % (action, kwargs))
        return kwargs['output']


def sequence_generator(state, input):
    return state+input, range(state, state+input)


if __name__ == "__main__":
    logging.basicConfig(format="%(asctime)s %(name)s proc=%(processName)s %(message)s", level=logging.DEBUG)
    member = Member(sequence_generator, address='memb1')
    client = Client('client', member.address)
    member.start(initial_value=0)
    print client.invoke(4)
    print client.invoke(1)
    member.join()
