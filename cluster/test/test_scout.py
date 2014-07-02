from cluster import *
from . import utils
import mock

PROPOSAL1 = Proposal(caller='test', client_id=111, input='one')
PROPOSAL2 = Proposal(caller='test', client_id=222, input='two')


class Tests(utils.ComponentTestCase):

    def setUp(self):
        super(Tests, self).setUp()
        self.sct = Scout(self.node, Ballot(10, 10),
                         peers=['p1', 'p2', 'p3'])

    @mock.patch.object(Scout, 'send_prepare')
    def test_start(self, send_prepare):
        """Start() just calls send_prepare()"""
        self.sct.start()
        send_prepare.assert_called_once_with()

    def test_send_prepare(self):
        """send_prepare does what it says, repeatedly"""
        self.sct.send_prepare()
        self.assertMessage(['p1', 'p2', 'p3'], Prepare(ballot_num=Ballot(10, 10)))
        self.assertNoMessages()
        self.network.tick(PREPARE_RETRANSMIT)
        self.assertMessage(['p1', 'p2', 'p3'], Prepare(ballot_num=Ballot(10, 10)))

    def test_PROMISE(self):
        """After a quorum of matching PROMISEs, the scout finishes accepted"""
        self.sct.send_prepare()
        self.assertMessage(['p1', 'p2', 'p3'], Prepare(ballot_num=Ballot(10, 10)))
        for acceptor in 'p1', 'p3':
            accepted = {
                'p1': {(Ballot(5, 5), 1): PROPOSAL1, (Ballot(6, 6), 2): PROPOSAL2},
                'p3': {(Ballot(5, 99), 1): PROPOSAL1, (Ballot(6, 99), 2): PROPOSAL2},
            }[acceptor]
            self.verifyPromiseAccepted(accepted)
            self.node.fake_message(Promise(ballot_num=Ballot(10, 10),
                                           accepted=accepted), sender=acceptor)
        self.assertMessage(['F999'], Adopted(ballot_num=Ballot(10, 10), pvals={
            (Ballot(5, 5), 1): PROPOSAL1,
            (Ballot(6, 6), 2): PROPOSAL2,
            (Ballot(5, 99), 1): PROPOSAL1,
            (Ballot(6, 99), 2): PROPOSAL2,
        }))
        self.assertUnregistered()

    def test_PROMISE_preempted(self):
        """PROMISEs with different ballot_nums mean preemption"""
        self.sct.send_prepare()
        self.assertMessage(['p1', 'p2', 'p3'], Prepare(ballot_num=Ballot(10, 10)))
        accepted = {}
        self.verifyPromiseAccepted(accepted)
        self.node.fake_message(Promise(
                    ballot_num=Ballot(99, 99),
                    accepted=accepted), sender='p2')
        self.assertMessage(['F999'], Preempted(slot=None, preempted_by=Ballot(99, 99)))
