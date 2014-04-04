from collections import namedtuple

__all__ = ['Ship']

Proposal = namedtuple('Proposal', ['caller', 'client_id', 'input'])
Ballot = namedtuple('Ballot', ['n', 'leader'])
ScoutId = namedtuple('ScoutId', ['address', 'ballot_num'])
CommanderId = namedtuple('CommanderId', ['address', 'slot', 'proposal'])

JOIN_RETRANSMIT = 0.7
CATCHUP_INTERVAL = 0.6
ACCEPT_RETRANSMIT = 1
PREPARE_RETRANSMIT = 1
INVOKE_RETRANSMIT = 0.5

# replicas should be able to re-propose a view change before the new node
# re-transmits the JOIN
assert CATCHUP_INTERVAL < JOIN_RETRANSMIT

from .ship import Ship
