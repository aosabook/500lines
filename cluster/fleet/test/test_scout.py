from fleet import *
from . import utils
import mock

PROPOSAL1 = Proposal(caller='test', client_id=111, input='one')
PROPOSAL2 = Proposal(caller='test', client_id=222, input='two')


class Tests(utils.ComponentTestCase):

    def setUp(self):
        super(Tests, self).setUp()
        self.leader = mock.Mock()
        self.sct = Scout(self.node, self.leader, Ballot(10, 10),
                         peers=['p1', 'p2', 'p3'])

    @mock.patch.object(Scout, 'send_prepare')
    def test_start(self, send_prepare):
        """Start() just calls send_prepare()"""
        self.sct.start()
        send_prepare.assert_called_once_with()

    def test_send_prepare(self):
        """send_prepare does what it says, repeatedly"""
        self.sct.send_prepare()
        self.assertMessage(['p1', 'p2', 'p3'], Prepare(
                           scout_id=self.sct.scout_id, ballot_num=Ballot(10, 10)))
        self.assertNoMessages()
        self.node.tick(PREPARE_RETRANSMIT)
        self.assertMessage(['p1', 'p2', 'p3'], Prepare(
                           scout_id=self.sct.scout_id, ballot_num=Ballot(10, 10)))

    def test_finished(self):
        """The finished method stops the retransmissions, tells the leader, and stops"""
        self.sct.pvals = {'pvals': 1}
        self.sct.send_prepare()
        self.assertMessage(['p1', 'p2', 'p3'], Prepare(
                           scout_id=self.sct.scout_id, ballot_num=Ballot(10, 10)))
        self.sct.finished(True, Ballot(20, 20))
        self.leader.scout_finished.assert_called_once_with(
            True, Ballot(20, 20), {'pvals': 1})
        self.node.tick(PREPARE_RETRANSMIT)
        self.assertNoMessages()
        self.assertUnregistered()

    @mock.patch.object(Scout, 'finished')
    def test_PROMISE(self, finished):
        """After a quorum of matching PROMISEs, the scout finishes accepted"""
        for acceptor in 'p1', 'p3':
            self.failIf(finished.called)
            accepted = {
                'p1': {(Ballot(5, 5), 1): PROPOSAL1, (Ballot(6, 6), 2): PROPOSAL2},
                'p3': {(Ballot(5, 99), 1): PROPOSAL1, (Ballot(6, 99), 2): PROPOSAL2},
            }[acceptor]
            self.node.fake_message(Promise(
                    scout_id=self.sct.scout_id,
                    acceptor=acceptor,
                    ballot_num=Ballot(10, 10),
                    accepted=accepted))
        finished.assert_called_once_with(True, Ballot(10, 10))
        self.assertEqual(dict(self.sct.pvals), {
            (Ballot(5, 5), 1): PROPOSAL1,
            (Ballot(6, 6), 2): PROPOSAL2,
            (Ballot(5, 99), 1): PROPOSAL1,
            (Ballot(6, 99), 2): PROPOSAL2,
        })

    @mock.patch.object(Scout, 'finished')
    def test_PROMISE_wrong_scout_id(self, finished):
        """PROMISEs with different scout_id's are ignored"""
        wrong_scout_id = ScoutId(address='OTHER', ballot_num=Ballot(99, 99))
        for acceptor in 'p1', 'p3':
            self.node.fake_message(Promise(
                    scout_id=wrong_scout_id,
                    acceptor=acceptor,
                    ballot_num=Ballot(10, 10),
                    accepted={}))
        self.failIf(finished.called)

    @mock.patch.object(Scout, 'finished')
    def test_PROMISE_preempted(self, finished):
        """PROMISEs with different ballot_nums mean preemption"""
        self.node.fake_message(Promise(
                    scout_id=self.sct.scout_id,
                    acceptor='p2',
                    ballot_num=Ballot(99, 99),
                    accepted={}))
        finished.assert_called_once_with(False, Ballot(99, 99))
