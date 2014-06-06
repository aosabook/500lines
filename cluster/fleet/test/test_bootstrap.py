import mock
from .. import bootstrap
from .. import replica
from .. import acceptor
from .. import leader
from .. import commander
from .. import scout
from .. import JOIN_RETRANSMIT
from .. import Join, Welcome
from . import utils

class Tests(utils.ComponentTestCase):

    def setUp(self):
        super(Tests, self).setUp()
        self.cb_args = None
        self.execute_fn = mock.Mock()

        self.Replica = mock.Mock(autospec=replica.Replica)
        self.Acceptor = mock.Mock(autospec=acceptor.Acceptor)
        self.Leader = mock.Mock(autospec=leader.Leader)
        self.Commander = mock.Mock(autospec=commander.Commander)
        self.Scout = mock.Mock(autospec=scout.Scout)

        self.bs = bootstrap.Bootstrap(
            self.node, ['p1', 'p2', 'p3'], self.execute_fn,
            replica_cls=self.Replica, acceptor_cls=self.Acceptor,
            leader_cls=self.Leader, commander_cls=self.Commander,
            scout_cls=self.Scout)

    def test_retransmit(self):
        """After start(), the bootstrap sends JOIN to each node in sequence until hearing WELCOME"""
        self.bs.start()
        for recip in 'p1', 'p2', 'p3', 'p1':
            self.assertMessage([recip], Join())
            self.node.tick(JOIN_RETRANSMIT)
        self.assertMessage(['p2'], Join())

        self.node.fake_message(Welcome(state='st', slot_num='sl', decisions={}))
        self.Acceptor.assert_called_with(self.node)
        self.Replica.assert_called_with(self.node, execute_fn=self.execute_fn)
        self.Leader.assert_called_with(self.node, unique_id=self.node.unique_id,
                                       peers=['p1', 'p2', 'p3'],
                                       commander_cls=self.Commander,
                                       scout_cls=self.Scout)
        self.Replica().start.assert_called()
        self.Leader().start.assert_called()
        self.assertTimers([])
        self.assertUnregistered()
