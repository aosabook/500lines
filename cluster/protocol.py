from collections import namedtuple

Proposal = namedtuple('Proposal', ['caller', 'cid', 'input'])
Ballot = namedtuple('Ballot', ['n', 'leader'])
ScoutId = namedtuple('ScoutId', ['address', 'ballot_num'])
CommanderId = namedtuple('CommanderId', ['address', 'slot', 'proposal'])
ViewChange = namedtuple('ViewChange', ['viewid', 'peers'])

HEARTBEAT_INTERVAL = 0.5
JOIN_RETRANSMIT = 0.2
REPROPOSE_INTERVAL = 0.7
