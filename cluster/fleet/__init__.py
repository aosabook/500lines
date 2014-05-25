from collections import namedtuple

__all__ = ['Ship']

# data types
Proposal = namedtuple('Proposal', ['caller', 'client_id', 'input'])
Ballot = namedtuple('Ballot', ['n', 'leader'])
ScoutId = namedtuple('ScoutId', ['address', 'ballot_num'])
CommanderId = namedtuple('CommanderId', ['address', 'slot', 'proposal'])

# message types
Accepted = namedtuple('Accepted', ['commander_id', 'acceptor', 'ballot_num'])
Accept = namedtuple('Accept', ['commander_id', 'ballot_num', 'slot', 'proposal'])
Catchup = namedtuple('Catchup', ['slot'])
Decision = namedtuple('Decision', ['slot', 'proposal'])
Invoked = namedtuple('Invoked', ['client_id', 'output'])
Invoke = namedtuple('Invoke', ['caller', 'client_id', 'input_value'])
Join = namedtuple('Join', [])
Active = namedtuple('Active', [])
Prepare = namedtuple('Prepare', ['scout_id', 'ballot_num'])
Promise = namedtuple('Promise', ['scout_id', 'acceptor', 'ballot_num', 'accepted'])
Propose = namedtuple('Propose', ['slot', 'proposal'])
Welcome = namedtuple('Welcome', ['state', 'slot_num', 'decisions'])

# constants - all of these should really be in terms of RTT's
JOIN_RETRANSMIT = 0.7
CATCHUP_INTERVAL = 0.6
ACCEPT_RETRANSMIT = 1.0
PREPARE_RETRANSMIT = 1.0
INVOKE_RETRANSMIT = 0.5
LEADER_TIMEOUT = 1.0

from .ship import Ship
