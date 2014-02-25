from collections import namedtuple

Proposal = namedtuple('Proposal', ['caller', 'cid', 'input'])
Ballot = namedtuple('Ballot', ['viewid', 'n', 'leader'])
ScoutId = namedtuple('ScoutId', ['address', 'ballot_num'])
CommanderId = namedtuple('CommanderId', ['address', 'slot', 'proposal'])
ViewChange = namedtuple('ViewChange', ['viewid', 'peers'])

HEARTBEAT_INTERVAL = 0.5
HEARTBEAT_GONE_COUNT = 3
JOIN_RETRANSMIT = 0.7
CATCHUP_INTERVAL = 0.6
ACCEPT_RETRANSMIT = 1
PREPARE_RETRANSMIT = 1
ALPHA = 10

# replicas should be able to re-propose a view change before the new node
# re-transmits the JOIN
assert CATCHUP_INTERVAL < JOIN_RETRANSMIT

class defaultlist(list):

    def set_len(self, l):
        if l > len(self):
            self.extend([None] * (l - len(self)))

    def __getitem__(self, i):
        self.set_len(i + 1)
        return list.__getitem__(self, i)

    def __setitem__(self, i, v):
        self.set_len(i + 1)
        list.__setitem__(self, i, v)


def view_primary(viewid, peers):
    return peers[viewid % len(peers)]
