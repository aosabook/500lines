from cluster import *
from . import utils
import mock

PROPOSAL1 = Proposal(caller='cli', client_id=123, input='one')
PROPOSAL2 = Proposal(caller='cli', client_id=125, input='two')
PROPOSAL3 = Proposal(caller='cli', client_id=127, input='tre')

Commander = mock.create_autospec(Commander)
Scout = mock.create_autospec(Scout)


class Tests(utils.ComponentTestCase):

    def setUp(self):
        super(Tests, self).setUp()
        Scout.reset_mock()
        Commander.reset_mock()
        self.ldr = Leader(self.node, ['p1', 'p2'],
                          commander_cls=Commander,
                          scout_cls=Scout)

    def assertScoutStarted(self, ballot_num):
        Scout.assert_called_with(self.node, ballot_num, ['p1', 'p2'])
        scout = Scout(self.node, ballot_num, ['p1', 'p2'])
        scout.start.assert_called_with()

    def assertNoScout(self):
        self.assertEqual(self.ldr.scout, None)

    def assertCommanderStarted(self, ballot_num, slot, proposal):
        Commander.assert_called_with(self.node, ballot_num, slot, proposal, ['p1', 'p2'])
        cmd = self.ldr.commanders[slot]
        cmd.start.assert_called_with()

    def assertNoCommander(self, slot):
        if slot in self.ldr.commanders:
                self.fail("commander running for slot %d" % slot)

    def activate_leader(self):
        self.ldr.active = True

    def fake_proposal(self, slot, proposal):
        self.ldr.proposals[slot] = proposal

    # tests

    def test_propose_inactive(self):
        """A PROPOSE received while inactive spawns a scout"""
        self.node.fake_message(Propose(slot=10, proposal=PROPOSAL1))
        self.assertScoutStarted(Ballot(0, 'F999'))

    def test_propose_scouting(self):
        """A PROPOSE received while already scouting is ignored."""
        self.node.fake_message(Propose(slot=10, proposal=PROPOSAL1))
        first_scout = self.ldr.scout
        self.node.fake_message(Propose(slot=10, proposal=PROPOSAL1))
        self.failUnless(self.ldr.scout is first_scout)

    def test_propose_active(self):
        """A PROPOSE received while active spawns a commander."""
        self.activate_leader()
        self.node.fake_message(Propose(slot=10, proposal=PROPOSAL1))
        self.assertCommanderStarted(Ballot(0, 'F999'), 10, PROPOSAL1)

    def test_propose_already(self):
        """A PROPOSE for a slot already in use is ignored"""
        self.activate_leader()
        self.fake_proposal(10, PROPOSAL2)
        self.node.fake_message(Propose(slot=10, proposal=PROPOSAL1))
        self.assertNoCommander(10)

    def test_commander_finished_successful(self):
        """When a commander finishes successfully, nothing more happens"""
        self.activate_leader()
        self.node.fake_message(Propose(slot=10, proposal=PROPOSAL1))
        self.node.fake_message(Decided(slot=10))
        self.assertNoCommander(10)

    def test_commander_finished_preempted(self):
        """When a commander is preempted, the commander is removed, the
        ballot num is incremented, and the leader is inactive, but no scout is
        spawned"""
        self.activate_leader()
        self.node.fake_message(Propose(slot=10, proposal=PROPOSAL1))
        self.node.fake_message(Preempted(slot=10, preempted_by=Ballot(22, 'XXXX')))
        self.assertNoCommander(10)
        self.assertEqual(self.ldr.ballot_num, Ballot(23, 'F999'))
        self.assertNoScout()
        self.assertFalse(self.ldr.active)

    @mock.patch.object(Leader, 'merge_pvals')
    def test_scout_finished_adopted(self, merge_pvals):
        """When a scout finishes and the leader is adopted, pvals are merged and the
        leader becomes active"""
        self.ldr.spawn_scout()
        self.node.fake_message(Adopted(ballot_num=Ballot(0, 'F999'), pvals={'p': 'vals'}))
        self.assertNoScout()
        merge_pvals.assert_called_with({'p': 'vals'})
        self.assertTrue(self.ldr.active)

    @mock.patch.object(Leader, 'merge_pvals')
    def test_scout_finished_preempted(self, merge_pvals):
        """When a scout finishes and the leader is preempted, the leader is inactive
        and its ballot_num is updated."""
        self.ldr.spawn_scout()
        self.node.fake_message(Preempted(slot=None, preempted_by=Ballot(22, 'F999')))
        self.assertNoScout()
        merge_pvals.assert_not_called()
        self.assertEqual(self.ldr.ballot_num, Ballot(23, 'F999'))
        self.assertFalse(self.ldr.active)

    def test_merge_pvals_empty(self):
        """Merging no pvals has no effect"""
        self.ldr.merge_pvals({})
        self.assertEqual(self.ldr.proposals, {})

    def test_merge_pvals_no_overlaps(self):
        """Merging pvals with no slot overlaps simply creates a new dictionary."""
        self.ldr.merge_pvals({
            (Ballot(10, 10), 10): PROPOSAL1,
            (Ballot(10, 10), 11): PROPOSAL2,
        })
        self.assertEqual(self.ldr.proposals, {10: PROPOSAL1, 11: PROPOSAL2})

    def test_merge_pvals_highest_ballot_wins(self):
        """Merging pvals where the slot numbers overlap chooses the one with
        the highest ballot num"""
        self.ldr.merge_pvals({
            (Ballot(10, 10), 10): PROPOSAL1,
            (Ballot(99, 99), 10): PROPOSAL3,
            (Ballot(99, 10), 10): PROPOSAL2,
        })
        self.assertEqual(self.ldr.proposals, {10: PROPOSAL3})
