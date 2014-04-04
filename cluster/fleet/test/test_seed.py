from .. import seed
from .. import JOIN_RETRANSMIT
from . import utils
import mock


class Tests(utils.ComponentTestCase):

    def setUp(self):
        super(Tests, self).setUp()
        self.leader = mock.Mock()
        self.seed = seed.Seed(self.member, 'state', ['p1', 'p2', 'p3'])

    def test_JOIN(self):
        self.node.fake_message('JOIN', requester='p1')
        self.assertNoMessages()  # no quorum
        self.node.fake_message('JOIN', requester='p3')
        self.assertMessage(['p1', 'p3'], 'WELCOME',
                           state='state', slot_num=1, decisions={},
                           peers=['p1', 'p2','p3'])

        self.node.tick(JOIN_RETRANSMIT)
        self.node.fake_message('JOIN', requester='p2')
        self.assertMessage(['p1', 'p2', 'p3'], 'WELCOME',
                           state='state', slot_num=1, decisions={},
                           peers=['p1', 'p2','p3'])

        self.node.tick(JOIN_RETRANSMIT * 2)
        self.node.fake_message('JOIN', requester='p2')
        self.assertNoMessages()
        self.assertUnregistered()
