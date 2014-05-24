from .. import bootstrap
from .. import JOIN_RETRANSMIT
from .. import Join, Welcome
from . import utils


class Tests(utils.ComponentTestCase):

    def setUp(self):
        super(Tests, self).setUp()
        self.cb_args = None
        self.bs = bootstrap.Bootstrap(
            self.member, ['p1', 'p2', 'p3'], self.bootstrapped_cb)

    def bootstrapped_cb(self, state, slot_num, decisions):
        self.cb_args = state, slot_num, decisions

    def test_retransmit(self):
        """After start(), the bootstrap sends JOIN to each node in sequence until hearing WELCOME"""
        self.bs.start()
        for recip in 'p1', 'p2', 'p3', 'p1':
            self.assertMessage([recip], Join())
            self.node.tick(JOIN_RETRANSMIT)
        self.assertMessage(['p2'], Join())

        self.node.fake_message(Welcome(state='st', slot_num='sl', decisions='dec'))
        self.assertEqual(self.cb_args, ('st', 'sl', 'dec'))
        self.assertTimers([])
        self.assertUnregistered()
