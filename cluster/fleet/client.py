import sys
from member import Component


class Request(Component):

    client_ids = iter(xrange(1000000, sys.maxint))
    RETRANSMIT_TIME = 0.5

    def __init__(self, member, n, callback):
        super(Request, self).__init__(member)
        self.client_id = self.client_ids.next()
        self.n = n
        self.output = None
        self.callback = callback

    def start(self):
        self.send([self.address], 'INVOKE', caller=self.address, client_id=self.client_id, input_value=self.n)
        self.invoke_timer = self.set_timer(self.RETRANSMIT_TIME, self.start)

    def do_INVOKED(self, client_id, output):
        if client_id != self.client_id:
            return
        self.logger.debug("received output %r" % (output,))
        self.cancel_timer(self.invoke_timer)
        self.callback(output)
        self.stop()
