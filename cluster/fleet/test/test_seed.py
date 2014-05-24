from .. import seed
from .. import JOIN_RETRANSMIT
from .. import Join, Welcome
from . import utils
import mock


class Tests(utils.ComponentTestCase):

    def setUp(self):
        super(Tests, self).setUp()
        self.leader = mock.Mock()
        self.seed = seed.Seed(self.member, 'state', ['p1', 'p2', 'p3'])

    def test_JOIN(self):
        """Seed waits for quorum, then sends a WELCOME in response to every JOIN until
        2* JOIN_RETRANSMIT seconds have passed with no JOINs"""
        self.node.fake_message(Join(), sender='p1')
        self.assertNoMessages()  # no quorum
        self.node.fake_message(Join(), sender='p3')
        self.assertMessage(['p1', 'p3'], Welcome(
                           state='state', slot_num=1, decisions={}))

        self.node.tick(JOIN_RETRANSMIT)
        self.node.fake_message(Join(), sender='p2')
        self.assertMessage(['p1', 'p2', 'p3'], Welcome(
                           state='state', slot_num=1, decisions={}))

        self.node.tick(JOIN_RETRANSMIT * 2)
        self.node.fake_message(Join(), sender='p2')
        self.assertNoMessages()
        self.assertUnregistered()
