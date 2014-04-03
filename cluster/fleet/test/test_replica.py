from .. import replica
from .. import Proposal
from . import utils
import mock

PROPOSAL1 = Proposal(caller='test', client_id=111, input='one')
PROPOSAL2 = Proposal(caller='test', client_id=222, input='two')
PROPOSAL3 = Proposal(caller='test', client_id=333, input='tre')
PROPOSAL4 = Proposal(caller='test', client_id=444, input='qua')

class Tests(utils.ComponentTestCase):

    def setUp(self):
        super(Tests, self).setUp()
        self.execute_fn = mock.Mock(name='execute_fn', spec=lambda state, input: None)
        self.rep = replica.Replica(self.member, self.execute_fn)
        self.rep.start('state', 2, {1: PROPOSAL1}, ['p1', 'p2'])
        self.assertNoMessages()

    def tearDown(self):
        self.assertNoMessages()

    @mock.patch.object(replica.Replica, 'propose')
    def test_INVOKE_new(self, propose):
        """An INVOKE with a new proposal results in a proposal"""
        self.node.fake_message('INVOKE', caller=PROPOSAL2.caller, client_id=PROPOSAL2.client_id,
                               input_value=PROPOSAL2.input)
        propose.assert_called_with(PROPOSAL2)

    @mock.patch.object(replica.Replica, 'propose')
    def test_INVOKE_repeat(self, propose):
        """An INVOKE with a proposal that has already been seen is ignored."""
        self.rep.proposals[1] = PROPOSAL1
        self.failIf(propose.called)

    def test_propose_new(self):
        """A proposeal without a specified slot gets the next slot and is
        proposed to the first peer"""
        self.rep.propose(PROPOSAL2)
        self.assertEqual(self.rep.next_slot, 3)
        self.assertMessage(['p1'], 'PROPOSE', slot=2, proposal=PROPOSAL2)

    def test_propose_resend(self):
        """A proposeal with a specified slot is re-transmitted with the same slot"""
        self.rep.next_slot = 3
        self.rep.propose(PROPOSAL2, 2)
        self.assertEqual(self.rep.next_slot, 3)
        self.assertMessage(['p1'], 'PROPOSE', slot=2, proposal=PROPOSAL2)

    def test_catchup_noop(self):
        """If slot_num == next_slot, there's no catchup to do"""
        self.rep.catchup()
        self.assertNoMessages()

    @mock.patch.object(replica.Replica, 'propose')
    def test_catchup_missed(self, propose):
        """If next_slot is greater than slot, then send CATCHUPs and (re-)propose"""
        self.rep.next_slot = 5
        self.rep.proposals = {2: PROPOSAL2, 3: PROPOSAL3}
        self.rep.decisions = {3: PROPOSAL3, 4: PROPOSAL4}
        # slot 2: proposed, undecided
        # slot 3: proposed, decided
        # slot 4: not proposed, decided
        self.rep.catchup()
        self.assertMessage(['p1', 'p2'], 'CATCHUP', slot=2, sender='F999')
        self.assertMessage(['p1', 'p2'], 'CATCHUP', slot=3, sender='F999')
        self.assertMessage(['p1', 'p2'], 'CATCHUP', slot=4, sender='F999')
        self.assertEqual(propose.call_args_list, [
            mock.call(PROPOSAL2, 2),
            mock.call(Proposal(None, None, None), 3),  # TODO: doesn't make sense
            mock.call(Proposal(None, None, None), 4),
        ])

    def test_CATCHUP_decided(self):
        """On CATCHUP for a decided proposal, re-send the DECISION"""
        self.node.fake_message('CATCHUP', slot=1, sender='p2')
        self.assertMessage(['p2'], 'DECISION', slot=1, proposal=PROPOSAL1)

    def test_CATCHUP_undecided(self):
        """On CATCHUP for an undecided proposal, do nothing"""
        self.node.fake_message('CATCHUP', slot=3, sender='p2')
        self.assertNoMessages()

    @mock.patch.object(replica.Replica, 'commit')
    def test_DECISION_gap(self, commit):
        """On DECISION for a slot we can't commit yet, decisions and next_slot are updated but
        no commit occurs"""
        self.node.fake_message('DECISION', slot=3, proposal=PROPOSAL3)
        self.assertEqual(self.rep.next_slot, 4)
        self.assertEqual(self.rep.decisions[3], PROPOSAL3)
        self.assertFalse(commit.called)

    @mock.patch.object(replica.Replica, 'commit')
    def test_DECISION_commit(self, commit):
        """On DECISION for the next slot, commit it"""
        self.node.fake_message('DECISION', slot=2, proposal=PROPOSAL2)
        self.assertEqual(self.rep.next_slot, 3)
        self.assertEqual(self.rep.decisions[2], PROPOSAL2)
        commit.assert_called_once_with(2, PROPOSAL2)

    @mock.patch.object(replica.Replica, 'commit')
    def test_DECISION_commit_cascade(self, commit):
        """On DECISION that allows multiple commits, they happen in the right order"""
        self.node.fake_message('DECISION', slot=3, proposal=PROPOSAL3)
        self.assertFalse(commit.called)
        self.node.fake_message('DECISION', slot=2, proposal=PROPOSAL2)
        self.assertEqual(self.rep.next_slot, 4)
        self.assertEqual(self.rep.decisions[2], PROPOSAL2)
        self.assertEqual(self.rep.decisions[3], PROPOSAL3)
        self.assertEqual(commit.call_args_list, [
            mock.call(2, PROPOSAL2),
            mock.call(3, PROPOSAL3),
        ])

    @mock.patch.object(replica.Replica, 'commit')
    def test_DECISION_repeat(self, commit):
        """On DECISION for a committed slot with a matching proposal, do nothing"""
        self.node.fake_message('DECISION', slot=1, proposal=PROPOSAL1)
        self.assertEqual(self.rep.next_slot, 2)
        self.assertFalse(commit.called)

    @mock.patch.object(replica.Replica, 'commit')
    def test_DECISION_repeat_conflict(self, commit):
        """On DECISION for a committed slot with a *non*-matching proposal, do nothing"""
        self.assertRaises(AssertionError, lambda:
            self.node.fake_message('DECISION', slot=1, proposal=PROPOSAL2))

    def test_join(self):
        """A JOIN from a cluster member gets a warm WELCOME."""
        self.node.fake_message('JOIN', requester='p2')
        self.assertMessage(['p2'], 'WELCOME', state='state', slot_num=2,
                           decisions={1: PROPOSAL1}, peers=['p1', 'p2'])
