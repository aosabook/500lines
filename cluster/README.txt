Author: Dustin J. Mitchell
Project: Cluster (a working title!)
Requirements: Python

This directory holds a simple implementation of a replicated state machine
that can also feed clients information on cluster membership.

* statemachine.py -- illustrates the state machine client API
* network.py -- handles network communication
* deterministic_network.py -- similar, but deterministic
* client.py -- a simple client
* member_single.py -- a non-clustered member server, talking to the client
* member_replicated.py -- a replicated state machine with consensus and a fixed membership
  This is based on "Paxos Made Moderately Complex" (van Renesse, 2011)
