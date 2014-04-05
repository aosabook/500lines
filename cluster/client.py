import sys
from member import Member
from member import Component


# TODO: eliminate - this library doesn't have distinct client nodes
class Client(Member):

    def __init__(self, node):
        super(Client, self).__init__(node)
        self.current_request = None

    def start(self):
        def re_invoke(n):
            self.invoke(n, lambda output: re_invoke(n+1))
        self.node.set_timer(1, lambda: re_invoke(1))

    def invoke(self, n, callback):
        assert self.current_request is None
        def done(output):
            self.current_request = None
            callback(output)
        self.current_request = Request(self, n, done)
        self.current_request.start()


class Request(Component):

    client_ids = xrange(1000000, sys.maxint).__iter__()
    RETRANSMIT_TIME = 0.5

    def __init__(self, member, n, callback):
        super(Request, self).__init__(member)
        self.cid = self.client_ids.next()
        self.n = n
        self.output = None
        self.callback = callback

    def start(self):
        self.send([self.member.node.network.rnd.choice(self.member.node.network.nodes.keys())], 'INVOKE',
                  caller=self.address, cid=self.cid, input=self.n)
        self.invoke_timer = self.set_timer(self.RETRANSMIT_TIME, self.start)

    def do_INVOKED(self, cid, output):
        if cid != self.cid:
            return
        self.logger.debug("received output %r" % (output,))
        self.cancel_timer(self.invoke_timer)
        self.callback(output)
        self.stop()
