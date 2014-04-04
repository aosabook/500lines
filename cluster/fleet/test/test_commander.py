from .. import commander
from .. import Ballot, CommanderId, Proposal, ACCEPT_RETRANSMIT
from . import utils
import mock


class Tests(utils.ComponentTestCase):

    def setUp(self):
        super(Tests, self).setUp()
        self.cb_args = None
        self.leader = mock.Mock(name='leader')
        self.slot = 10
        self.proposal = Proposal(caller='cli', client_id=123, input='inc')
        self.commander_id = CommanderId(
            address=self.node.address, slot=self.slot, proposal=self.proposal)
        self.ballot_num = Ballot(91, 82)
        self.cmd = commander.Commander(
            self.member, leader=self.leader, ballot_num=self.ballot_num,
            slot=self.slot, proposal=self.proposal,
            commander_id=self.commander_id, peers=['p1', 'p2', 'p3'])
        self.accept_kwargs = dict(
            commander_id=self.commander_id, ballot_num=self.ballot_num, slot=self.slot,
            proposal=self.proposal)

    def test_retransmit(self):
        """After start(), the commander sends ACCEPT repeatedly to all peers which have not responded"""
        self.cmd.start()
        self.assertMessage(['p1', 'p2', 'p3'], 'ACCEPT', **self.accept_kwargs)
        self.node.tick(ACCEPT_RETRANSMIT)
        self.assertMessage(['p1', 'p2', 'p3'], 'ACCEPT', **self.accept_kwargs)

        self.node.fake_message(
            'ACCEPTED', commander_id=self.commander_id, acceptor='p2',
            ballot_num=self.ballot_num)
        self.node.tick(ACCEPT_RETRANSMIT)
        self.assertMessage(['p1', 'p3'], 'ACCEPT', **self.accept_kwargs)
        self.node.tick(ACCEPT_RETRANSMIT)
        self.assertMessage(['p1', 'p3'], 'ACCEPT', **self.accept_kwargs)
        self.node.fake_message(
            'ACCEPTED', commander_id=self.commander_id, acceptor='p1',
            ballot_num=self.ballot_num)

        # quorum (3/2+1 = 2) reached
        self.assertMessage(['p1', 'p2', 'p3'], 'DECISION',
                           slot=self.slot, proposal=self.proposal)

        self.leader.commander_finished.assert_called_with(
            self.commander_id, self.ballot_num, False)
        self.assertTimers([])
        self.assertUnregistered()

    def test_wrong_commander_id(self):
        """Commander ignores ACCEPTED messages for other commanders"""
        self.cmd.start()
        self.assertMessage(['p1', 'p2', 'p3'], 'ACCEPT', **self.accept_kwargs)
        other_commander_id = CommanderId(
            address='OTHER', slot=self.slot, proposal=self.proposal)
        self.node.fake_message(
            'ACCEPTED', commander_id=other_commander_id, acceptor='p1',
            ballot_num=self.ballot_num)
        self.node.tick(ACCEPT_RETRANSMIT)
        # p1 still in the list
        self.assertMessage(['p1', 'p2', 'p3'], 'ACCEPT', **self.accept_kwargs)

    def test_preempted(self):
        """If the commander receives an ACCEPTED response with a different ballot number, then it
        is preempted"""
        self.cmd.start()
        self.assertMessage(['p1', 'p2', 'p3'], 'ACCEPT', **self.accept_kwargs)
        other_ballot_num = Ballot(99, 99)
        self.node.fake_message(
            'ACCEPTED', commander_id=self.commander_id, acceptor='p1',
            ballot_num=other_ballot_num)

        self.leader.commander_finished.assert_called_with(
            self.commander_id, other_ballot_num, True)
        self.assertTimers([])
        self.assertUnregistered()
