Clustering by Consensus
=======================

Running a service or application in production means providing a high level of availability in the face of component failures.
Each tier of a highly-available service will span a "cluster" of multiple machines, such that if one or more machines fail, the service will continue to operate.

Some services cluster naturally, with each operation handled indpendently by a member of the cluster.
If one member fails, operations are routed to the remaining members.
Most web services operate in this fashion.

Many services require some state to be shared immediately between cluster members.
For example, XXX

XXX

But first, let's look at the simplest form of the Paxos algorithm.
This algorithm achieves consensus on a single value which is then fixed for all time.
This might not seem so useful, but we will see that it forms the basis of more practical, if more complicated, implementations.

This description is not meant to be formal, but to provide enough background to understand the code.
Consult the references for more formal descriptions of the protocol, including invariants and correctness proofs.

Consensus by Paxos
------------------

Paxos begins with one member of the cluster deciding to propose a value.
This cluster member has a desired value, but may also learn that a different value has already been decided.
The process is called a "round" and ends with a decision either on the existing value or the proposer's new value.

The proposer beings by sending a ``PREPARE`` message with a ballot number to the acceptors.
In practice, all cluster nodes act as acceptors, which act as the cluster's memory.
The proposer waits to hear from a majority (more generally, a quorum) of the acceptors.

The ``PREPARE`` message contains a ballot number, and the protocol requires this to be unique across the cluster and strictly increasing.
In practice, this is easily accomplished: each ballot number is a tuple of an integer and a unique ID for the cluster member, such as its IP address.

On receiving to a ``PREPARE`` with a ballot number *N*, each acceptor promises to ignore all proposals with a ballot number less than *N*.
It sends a ``PROMISE`` message including a highest ballot number *N* it has promised (which may be higher than that in the ``PREPARE``) and an accepted value *V*.

If the acceptor has already made a promise for a larger ballot number, it includes that number in the ``PROMISE``, indicating that the proposer has been pre-empted.

When the proposer has heard from a quorum of the acceptors, then it sends an ``ACCEPT`` to the acceptors.
This message includes a ballot number and a value.
If the proposer was not pre-empted, then it sends its own desired value.
Otherwise, it sends the value from the highest-numbered ``PROMISE`` it received.
This ensures an important invariant of the algorithm: once a value is decided in a round, all subsequent rounds will decide on hthe same value.

Unless it would violate a promise, each acceptor records the value from the ``PREPARE`` message as accepted.
It then replies with an ``ACCEPTED`` message containing the accepted ballot number.

The overall effect is two round-trips: one to secure a promise and one to get agreement on the value.
When multiple proposers make a proposal at the same time, it is common for neither proposal to win.
Both proposers then re-propose, and the deadlock can continue indefinitely if the timing works out just right.
In a bad -- but not uncommon -- case, it can take dozens of round-trips to reach consensus.

Multi-Paxos
...........

Reaching consensus on a single, static value is particularly useful on its own.
Clustered systems want to agree on a particular state that evolves over time in response to events.
This is implemented as a distributed state machine, where the transitions between states are decided by consensus.
Each transition is given a "slot number", and each member of the cluster executes transitions in order as they are decided.

In principle, each slot is decided in its own instance of Paxos, tagged with the slot number.
In practice, the high cost of each Paxos instance requires some optimizations which blur the lines of this simple principle.
For example, ballot numbers are global to the protocol, and the first phase (``PREPARE``/``PROMISE``) is performed for all undecided slots at once.

XXX more? Leader election?

The library in this chapter implements a simple form of Multi-Paxos.

Software Design
---------------

Users of this library will depend on its correctness, so it's important to structure the code so that we can see -- and test -- its correspondance to the specification.
Complex protocols can exhibit complex failures, too, so we will build debugging support that will allow test runs to be replayed exactly.

Network
.......

A network protocol like Paxos needs the ability to send and receive messages and a means of setting timers for re-transmitting messages.

We'll build a simple simulated network providing these capabilities, that also simulates packet loss and message propagation delays.
This simulator is *deterministic*: unlike a real network, it will behave exactly the same on every run.
This is invaluable in developing a protocol, as a buggy execution can be easily re-run with additional debugging output or under a debugger.
In this simulator, the determinism comes from using a fixed seed value for Python's `random` module.

Timers are handled using Python's `heapq` module, allowing efficient selection of the next event.
Setting a timer involves pushing a Timer object onto the heap.
Since removing items from a heap is inefficient, cancelled timers are left in place but marked as cancelled.

INCLUDE deterministic_network.Timer

The Timer class simply stores some information, provides a ``__cmp__`` method to sort the heap, and implements a ``cancel`` method.

INCLUDE deterministic_network.Network

Running the simulation, then, just involves popping timers from the heap and executing them if they have not been cancelled and if the node that set the timer still exists.

Message transmission uses the timer functionality to schedule a later delivery of the message at each node, using a random simulated delay.
Since nodes may be added and removed from the network, the `_receive` function ignores any messages for nonexistent nodes.

Nodes
.....

Each host in the network is represented as a `Node`:

INCLUDE deterministic_network.Node

Each has a distinct address and a list of components.  

--------

Software Design
 + Network
 + Component Model
 + named tuples but kwargs
 + separation of concerns
 + events
 + tests
 + logging
 + library interface

Implementation Challenges
 + Follow the Leader
 + Catching Up

Improvements
 + Consistent memory usage
 + View changes
