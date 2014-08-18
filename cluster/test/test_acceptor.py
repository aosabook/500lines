from cluster import *
from . import utils


class Tests(utils.ComponentTestCase):

    def setUp(self):
        super(Tests, self).setUp()
        self.ac = Acceptor(self.node)

    def assertState(self, ballot_num, accepted_proposals):
        self.assertEqual(self.ac.ballot_num, ballot_num)
        self.assertEqual(self.ac.accepted_proposals, accepted_proposals)

    def test_prepare_new_ballot(self):
        """On PREPARE with a new ballot, Acceptor returns a PROMISE with the new ballot
        and send an ACCEPTING message"""
        proposal = Proposal('cli', 123, 'INC')
        self.ac.accepted_proposals = {33: (Ballot(19, 19), proposal)}
        self.ac.ballot_num = Ballot(10, 10)
        self.node.fake_message(Prepare(
                               # newer than the acceptor's ballot_num
                               ballot_num=Ballot(19, 19)), sender='SC')
        self.assertMessage(['F999'], Accepting(leader='SC'))
        accepted_proposals = {33: (Ballot(19, 19), proposal)}
        self.verifyAcceptedProposals(accepted_proposals)
        self.assertMessage(['SC'], Promise(
                           # replies with updated ballot_num
                           ballot_num=Ballot(19, 19),
                           # including accepted_proposals ballots
                           accepted_proposals=accepted_proposals))
        self.assertState(Ballot(19, 19), {33: (Ballot(19, 19), proposal)})

    def test_prepare_old_ballot(self):
        """On PREPARE with an old ballot, Acceptor returns a PROMISE with the
        existing (newer) ballot and does not send an ACCEPTING"""
        self.ac.ballot_num = Ballot(10, 10)
        self.node.fake_message(Prepare(
                               # older than the acceptor's ballot_num
                               ballot_num=Ballot(5, 10)), sender='SC')
        accepted_proposals = {}
        self.verifyAcceptedProposals(accepted_proposals)
        self.assertMessage(['SC'], Promise(
                           # replies with newer ballot_num
                           ballot_num=Ballot(10, 10),
                           accepted_proposals=accepted_proposals))
        self.assertState(Ballot(10, 10), {})

    def test_accept_new_ballot(self):
        """On ACCEPT with a new ballot, Acceptor returns ACCEPTED with the new ballot number
        and records the proposal as accepted_proposals"""
        proposal = Proposal('cli', 123, 'INC')
        self.ac.ballot_num = Ballot(10, 10)
        self.node.fake_message(Accept(
                               slot=33,
                               ballot_num=Ballot(19, 19),
                               proposal=proposal), sender='CMD')
        self.assertMessage(['CMD'], Accepted(
                           slot=33,
                           # replies with updated ballot_num
                           ballot_num=Ballot(19, 19)))
        # and state records acceptance of proposal
        self.assertState(Ballot(19, 19), {33: (Ballot(19, 19), proposal)})

    def test_accept_old_ballot(self):
        """On ACCEPT with an old ballot, Acceptor returns ACCEPTED with the
        already-accepted ballot, and does *not* accept the ballot."""
        proposal = Proposal('cli', 123, 'INC')
        self.ac.ballot_num = Ballot(10, 10)
        self.node.fake_message(Accept(
                               slot=33,
                               ballot_num=Ballot(5, 5),
                               proposal=proposal), sender='CMD')
        self.assertMessage(['CMD'], Accepted(
                           slot=33,
                           # replies with newer ballot_num
                           ballot_num=Ballot(10, 10)))
        # and doesn't accept the proposal
        self.assertState(Ballot(10, 10), {})
