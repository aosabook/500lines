import sys
from member import Component


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
        self.send([self.address], 'INVOKE', caller=self.address, cid=self.cid, input=self.n)
        self.invoke_timer = self.set_timer(self.RETRANSMIT_TIME, self.start)

    def do_INVOKED(self, cid, output):
        if cid != self.cid:
            return
        self.logger.debug("received output %r" % (output,))
        self.cancel_timer(self.invoke_timer)
        self.callback(output)
        self.stop()
