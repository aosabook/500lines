from .. import acceptor
from .. import Ballot, ScoutId, CommanderId, Proposal
from . import utils


class Tests(utils.ComponentTestCase):

    def setUp(self):
        super(Tests, self).setUp()
        self.ac = acceptor.Acceptor(self.member)

    def assertState(self, ballot_num, accepted):
        self.assertEqual(self.ac.ballot_num, ballot_num)
        self.assertEqual(self.ac.accepted, accepted)

    def test_prepare_new_ballot(self):
        """On PREPARE with a new ballot, Acceptor returns a PROMISE with the new ballot"""
        proposal = Proposal('cli', 123, 'INC')
        self.ac.accepted = {(Ballot(19, 19), 33): proposal}
        self.ac.ballot_num = Ballot(10, 10)
        self.node.fake_message('PREPARE',
                               scout_id=ScoutId(
                                   address='SC', ballot_num=Ballot(19, 19)),
                               # newer than the acceptor's ballot_num
                               ballot_num=Ballot(19, 19))
        self.assertMessage(['SC'], 'PROMISE',
                           scout_id=ScoutId(
                               address='SC', ballot_num=Ballot(19, 19)),
                           acceptor='F999',
                           # replies with updated ballot_num
                           ballot_num=Ballot(19, 19),
                           # including accepted ballots
                           accepted={(Ballot(19, 19), 33): proposal})
        self.assertState(Ballot(19, 19), {(Ballot(19, 19), 33): proposal})

    def test_prepare_old_ballot(self):
        """On PREPARE with an old ballot, Acceptor returns a PROMISE with the
        existing (newer) ballot"""
        self.ac.ballot_num = Ballot(10, 10)
        self.node.fake_message('PREPARE',
                               scout_id=ScoutId(
                                   address='SC', ballot_num=Ballot(5, 10)),
                               # older than the acceptor's ballot_num
                               ballot_num=Ballot(5, 10))
        self.assertMessage(['SC'], 'PROMISE',
                           scout_id=ScoutId(
                               address='SC', ballot_num=Ballot(5, 10)),
                           acceptor='F999',
                           # replies with newer ballot_num
                           ballot_num=Ballot(10, 10),
                           accepted={})
        self.assertState(Ballot(10, 10), {})

    def test_accept_new_ballot(self):
        """On ACCEPT with a new ballot, Acceptor returns ACCEPTED with the new ballot number
        and records the proposal as accepted"""
        proposal = Proposal('cli', 123, 'INC')
        cmd_id = CommanderId(address='CMD', slot=33, proposal=proposal)
        self.ac.ballot_num = Ballot(10, 10)
        self.node.fake_message('ACCEPT',
                               commander_id=cmd_id,
                               ballot_num=Ballot(19, 19),
                               slot=33,
                               proposal=proposal)
        self.assertMessage(['CMD'], 'ACCEPTED',
                           commander_id=cmd_id,
                           acceptor='F999',
                           # replies with updated ballot_num
                           ballot_num=Ballot(19, 19))
        # and state records acceptance of proposal
        self.assertState(Ballot(19, 19), {(Ballot(19, 19), 33): proposal})

    def test_accept_old_ballot(self):
        """On ACCEPT with an old ballot, Acceptor returns ACCEPTED with the
        already-accepted ballot, and does *not* accept the ballot."""
        proposal = Proposal('cli', 123, 'INC')
        cmd_id = CommanderId(address='CMD', slot=33, proposal=proposal)
        self.ac.ballot_num = Ballot(10, 10)
        self.node.fake_message('ACCEPT',
                               commander_id=cmd_id,
                               ballot_num=Ballot(5, 5),
                               slot=33,
                               proposal=proposal)
        self.assertMessage(['CMD'], 'ACCEPTED',
                           commander_id=cmd_id,
                           acceptor='F999',
                           # replies with newer ballot_num
                           ballot_num=Ballot(10, 10))
        # and doesn't accept the proposal
        self.assertState(Ballot(10, 10), {})
