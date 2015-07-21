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
        Scout.assert_called_once_with(self.node, ballot_num, ['p1', 'p2'])
        scout = Scout(self.node, ballot_num, ['p1', 'p2'])
        scout.start.assert_called_once_with()

    def assertNoScout(self):
        self.assertFalse(self.ldr.scouting)

    def assertCommanderStarted(self, ballot_num, slot, proposal):
        Commander.assert_called_once_with(self.node, ballot_num, slot, proposal, ['p1', 'p2'])
        cmd = Commander(self.node, ballot_num, slot, proposal, ['p1', 'p2'])
        cmd.start.assert_called_with()

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
        self.node.fake_message(Propose(slot=10, proposal=PROPOSAL1))
        self.assertScoutStarted(Ballot(0, 'F999'))

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
        self.assertEqual(Commander.mock_calls, [])

    def test_commander_finished_preempted(self):
        """When a commander is preempted, the ballot num is incremented, and
        the leader is inactive, but no scout is spawned"""
        self.activate_leader()
        self.node.fake_message(Propose(slot=10, proposal=PROPOSAL1))
        self.node.fake_message(Preempted(slot=10, preempted_by=Ballot(22, 'XXXX')))
        self.assertEqual(self.ldr.ballot_num, Ballot(23, 'F999'))
        self.assertNoScout()
        self.assertFalse(self.ldr.active)

    def test_scout_finished_adopted(self):
        """When a scout finishes and the leader is adopted, accepted proposals
        are merged and the leader becomes active"""
        self.ldr.spawn_scout()
        self.ldr.proposals[9] = PROPOSAL2
        self.node.fake_message(Adopted(ballot_num=Ballot(0, 'F999'),
            accepted_proposals={10: PROPOSAL3}))
        self.assertNoScout()
        self.assertTrue(self.ldr.active)
        self.assertEqual(self.ldr.proposals, {
            9: PROPOSAL2,
            10: PROPOSAL3,
        })

    def test_scout_finished_preempted(self):
        """When a scout finishes and the leader is preempted, the leader is inactive
        and its ballot_num is updated."""
        self.ldr.spawn_scout()
        self.node.fake_message(Preempted(slot=None, preempted_by=Ballot(22, 'F999')))
        self.assertNoScout()
        self.assertEqual(self.ldr.ballot_num, Ballot(23, 'F999'))
        self.assertFalse(self.ldr.active)
