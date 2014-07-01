Clustering by Consensus
***********************

Running a service or application in production means providing a high level of availability in the face of component failures.
Each tier of a highly-available service will span a "cluster" of multiple machines, such that if one or more machines fail, the service will continue to operate.

Some services cluster naturally, with each operation handled indpendently by a member of the cluster.
If one member fails, operations are routed to the remaining members.
Many web services operate in this fashion, especially those serving static content which can simply be replicated to many nodes.

Some services require some state to be shared immediately between cluster members.
For example, a distributed object storage engine needs to track which objects are stored on which servers.
Storing this information on a single server would mean that when the server fails, the cluster fails -- not good.
But if the information is stored on several servers, it's challenging to keep them in sync.

Consensus algorithms solve this challenge, providing a mechanism to keep replicated information from diverging, regardless of component failures.

Consensus by Paxos
==================

Paxos is one such algorithm, or really family of algorithms.
It was described by Leslie Lamport in a fanciful 1998 paper entitled "The Part-Time Parliament".

This description is not meant to be formal, but to provide enough background to understand the code.
Lamport's paper has a great deal more detail and is a fun read.
The references at the end of the chapter describe some extensions of the algorithm that have been adapted in this implementation.

Simple Paxos
------------

First, let's look at the simplest form of the Paxos algorithm.
This algorithm achieves consensus on a single value which is then fixed for all time.
This might not seem so useful, but we'll see that it forms the basis of more practical, if more complicated, implementations.

Paxos begins with one member of the cluster (called the proposer) deciding to propose a value.
The proposer has a desired value, but may also learn that a different value has already been decided.
The process is called a "round" and ends with a decision either on the existing value or the proposer's new value.

# TODO: verify all of this

The proposer beings by sending a ``PREPARE`` message with a ballot number to the acceptors.
In practice, all cluster nodes act as acceptors, serving as the cluster's memory.
The proposer waits to hear from a majority (or more generally, a quorum) of the acceptors.

The ``PREPARE`` message contains a ballot number, and the protocol requires this to be unique across the cluster and strictly increasing.
This is easily accomplished by making each ballot number a combination of an increasing integer and a unique ID for the cluster member, such as its IP address.

On receiving to a ``PREPARE`` with a ballot number *N*, each acceptor promises to ignore all proposals with a ballot number less than *N*.
It sends a ``PROMISE`` message including a highest ballot number *N* it has promised and an accepted value *V*.
If the acceptor has already made a promise for a larger ballot number, it includes that number in the ``PROMISE``, indicating that the proposer has been pre-empted.

When the proposer has heard from a quorum of the acceptors, then it sends an ``ACCEPT`` to the acceptors, including a ballot number and a value.
If the proposer was not pre-empted, then it sends its own desired value.
Otherwise, it sends the value from the highest-numbered ``PROMISE`` it received.
This ensures an important invariant of the algorithm: once a value is decided in a round, all subsequent rounds will decide on the same value.

Unless it would violate a promise, each acceptor records the value from the ``PREPARE`` message as accepted.
It then replies with an ``ACCEPTED`` message containing the accepted ballot number.

The overall effect is two round-trips: one to secure a promise and one to get agreement on the value.
When multiple proposers make a proposal at the same time, it is common for neither proposal to win.
Both proposers then re-propose.
Hopefully one wins, but the deadlock can continue indefinitely if the timing works out just right.
In a bad -- but not uncommon -- case, it can take dozens of round-trips to reach consensus.

Multi-Paxos
-----------

Reaching consensus on a single, static value is not particularly useful on its own.
Clustered systems want to agree on a particular state that evolves over time.
The solution is to create a distributed state machine, where the transitions between states are decided by consensus.
Each transition is given a "slot number", and each member of the cluster executes transitions in strict order.

In principle, each slot is decided in its own instance of Paxos, tagged with the slot number.
In practice, the high cost of each Paxos instance requires some optimizations which blur the lines of this simple principle.
For example, ballot numbers are global to the protocol, and the first phase (``PREPARE``/``PROMISE``) is performed for all undecided slots at once.

Getting Moderately Complex
--------------------------

A surprising amount of additional infrastructure is required to make this model effective:
 * starting the cluster from scratch
 * leader election to avoid deadlock
 * a gossip protocol to bring lagging members up to date
 * XXX more..

XXX - introduce components based on Renesee(sp?)

NOTE: even reads need to be state machine operations

Leader Elections
----------------

Introducing Cluster
===================

The "cluster" library in this chapter implements a simple form of Multi-Paxos.
It is designed for providing a consensus service to a larger application.
The application creates and starts a ``Member`` object on each cluster member, providing an application-specific state machine and a list of peers.
The application accesses the shared state through the ``invoke`` method, which kicks off a proposal for a state transition.
Once that proposal is decided and the state machine runs, ``invoke`` returns the machine's output.

Users of this library will depend on its correctness, so it's important to structure the code so that we can see -- and test -- its correspondance to the specification.
Complex protocols can exhibit complex failures, too, so we will build support for reproducing and debugging rare failures.

Message Types
-------------

Cluster's protocol uses 16 different message types.
Using named tuples to describe each message type keeps the code clean and helps avoid some simple errors.
The named tuple constructor will raise an exception if it is not given exactly the right attributes, making typos obvious.
The tuples format themselves nicely in log messages, and as an added bonus don't use much RAM.

Component Model
---------------

Humans are limited by what we can hold in our active memory.
We can't reason about the entire Cluster implementation at once -- it's just too much, and too easy to miss details.
Instead, we break Cluster down into a handful of components, implemented by subclasses of ``Component``.
Each class is responsible for a different part of the protocol.

The components are glued together by the ``Node`` class, which represents a single node on the network.
Components are added to and removed from the node as execution proceeds
Messages that arrive on the node are relayed to all active components, calling a method named after the capitalized message type with a ``do_`` prefix.
These ``do_`` methods receive the message's attributes as keyword arguments for easy access.

The ``Node`` class also provides some convenience methods, using ``functools.partial`` to supply some arguments to the same methods of the ``Network`` class.

Acceptor
........

The ``Acceptor`` class illustrates the component model well.
It implements the acceptor role in the protocol, so it must store the ballot number representing its most recent promise, along with the set of accepted proposals for each slot.
It then responds to ``PREPARE`` and ``ACCEPT`` messages according to the protocol.
The result is a short class that is easy to compare to the protocol.

Replica
.......

The ``Replica`` class is the most complicated component class, as it has a few closely related responsibilities:

* Making new proposals;
* Catching up with missed decisions;
* Invoking the local state machine when proposals are decided;
* Tracking the current leader; and
* Adding newly started nodes to the cluster.

The replica creates new proposals in response to ``INVOKE`` messages, selecting what it believes to be an unused slot and sending a ``PROPOSE`` message to the current leader.
But that's not enough -- the replica must re-transmit that ``PROPOSE`` message, possibly to a different leader, until it is successful.
Furthermore, if the consensus for the selected slot is for a different proposal, the replic must re-propose with a new slot.

We handle missed decisions with a simple gossip protocol: each replica periodically sends a ``CATCHUP`` method requesting information on slots it's not aware of a decision for.
Other replicas send ``DECISION`` messages in response.
The ``CATCHUP`` messages also include the highest known slot, so replicas can learn about slots they didn't even know were proposed.

``DECISION`` messages represent slots on which the cluster has come to consensus.
Here, replicas store away the new decision, then run the state machine until it reaches an undecided slot.

In some circumstances, it's possible for a slot to have no active proposals and no decision.
The state machine is required to execute slots one by one, so the cluster much reach a consensus on something to fill the slot.
To protect against this possibility, replicas make a "no-op" proposal whenever they catch up on a slot.
If such a proposal is eventually decided, then the state machine does nothing for that slot.
Likewise, it's possible for the same proposal to be decided twice.
The replica skips invoking the state machine for any such duplicate proposals, performing no transition for that slot.

Replicas need to know which node is the active leader in order to send ``PROPOSE`` messages to it.
There is a surprising amount of subtlty required to get this right, as we'll see later.
Each replica tracks the active leader using three sources of information:

* When the leader component becomes active, it sends an ``ADOPTED`` message to its local replica.
* When the acceptor component sends a ``PREPARE`` to a new leader, it sends an ``ACCEPTING`` message to its local replica.
* The active leader sends ``ACTIVE`` messages as a heartbeat.
  If no such message arrives before the ``LEADER_TIMEOUT`` expires, the replica assumes the leader is dead and moves on to the next leader.
  In this case, it's important that all replicas choose the *same* new leader.

Finally, when a node joins the network, the bootstrap component sends a ``JOIN`` message.
The replica responds with a ``WELCOME`` message containing its most recent state, allowing the new node to come up to speed quickly.

Leader, Scout, and Commander
............................

The leader's primary task is to take in proposals messages and produce decisions.
A leader is "active" when it has already carried out the ``PREPARE``/``PROMISE`` portion of the protocol.
An active leader can immediately send an ``ACCEPT`` message in response to a ``PROPOSE``.

In keeping with the component model, the leader delegates to the scout and commander components to carry out each portion of the protocol.

The leader creates a scout component when it wants to become active, in response to receiving a ``PROPOSE``.
The scout sends (and re-sends, if necessary) a ``PREPARE`` message, and collects ``PROMISE`` responses until it has heard from a majority of its peers or until it has been preempted.
It communicates the result back to the leader with an ``ADOPTED`` or ``PREEMPTED`` message, respectively.

The leader creates a commander component for each slot where it has an active proposal.
Like a scout, a commander sends and re-sends ``ACCEPT`` messages and waits for a majority of acceptors to reply with ``ACCEPTED``, or for news of its preemption.
When a proposal is accepted, the commander broadcasts a ``DECISION`` message to all nodes.
It responds to the leader with either ``DECIDED`` or ``PREEMPTED``.

.. note::

    A surprisingly subtle bug appeared here during development.
    At the time, the network simulator introduced packet loss even on messages within a node.
    When *all* ``DECISION`` messages were lost, the protocol could not proceed.
    The replica continued to re-transmit ``PROPOSE`` messages, but the leader ignored them as it already had a proposal for that slot.
    The replica's catch-up process could not find the result, as no replica had heard of the decision.
    The solution was to ensure that local messages are always delivered.


Bootstrap
.........

When a node joins the cluster, it must determine the current cluster state before it can participate.
The bootstrap component handles this by sending ``JOIN`` messages to each peer in turn until it receives a ``WELCOME``.

An early version of the implementation started each node with a full set of components (replica, leader, and acceptor), each of which began in a "startup" phase, waiting for information from the ``WELCOME`` message.
This spread the initialization logic around every component, requiring separate testing of each one.
The final design has the bootstrap component creating each of the other components once startup is complete, passing the initial state to their constructors.

Seed
....

In normal operation, when a node joins the cluster, it expects to find the cluster already running, with at least one node willing to respond to a ``JOIN`` message.
But how does the cluster get started?
An option is for the bootstrap component to decide, after attempting to contact every other node, that it is the first in the cluster.
But this has two problems.
First, for a large cluster it means a long wait while each ``JOIN`` times out.
More importantly, in the event of a network partition, a new node might be unable to contact any others and start a new cluster.
When the network heals and that node can communicate with the other nodes, there are two clusters with different decisions for the same slots!

To avoid this outcome, creating a new cluster is a user-specified operation.
Exactly one node in the cluster runs the seed component, with the others running bootstrap as usual.
The seed waits until it has received ``JOIN`` messages from a majority of its peers, then sends a ``WELCOME`` with an initial state for the state machine and an empty set of decisions.
The seed component then stops itself and starts a bootstrap component to join the newly-seeded cluster.

Request
.......

The request component manages a request to the distributed state machine.
The component simply sends ``INVOKE`` messages to the local replica until it receives a corresponding ``INVOKED``.

Network
-------

Any network protocol needs the ability to send and receive messages and a means of calling functions at a time in the future.

The ``Network`` class provides simple simulated network with these capabilities and also simulates packet loss and message propagation delays.

Timers are handled using Python's `heapq` module, allowing efficient selection of the next event.
Setting a timer involves pushing a ``Timer`` object onto the heap.
Since removing items from a heap is inefficient, cancelled timers are left in place but marked as cancelled.

Message transmission uses the timer functionality to schedule a later delivery of the message at each node, using a random simulated delay.
We again use ``functools.partial`` to set up a future call to the destination node's ``receive`` method with appropriate arguments.

Running the simulation just involves popping timers from the heap and executing them if they have not been cancelled and if the destination node is still active.

Debugging Support
-----------------

When developing a complex system such as this, the bugs quickly transition from trivial ``NameError``\s to obscure failures that only manifest after several minutes of (simulated) proocol operation.
Chasing down bugs like this involves working backward from the point where the error became obvious.
Interactive debuggers are useless here, as they can only step forward in time.

The most important debugging feature in cluster is a *deterministic* simulator.
Unlike a real network, it will behave exactly the same way on every run, given the same seed for the random number generator.
This means that we can add additional debugging checks or output to the code and re-run the simulation to see the same failure in more detail.

Of course, much of that detail is in the messages sent and received by the different nodes and components, so those are automatically logged in their entirety.
That logging includes the component sending or receiving the message, as well as the simulated timestamp, injected via the ``SimTimeLogger`` class.

A resilient protocol such as this one can often run for a long time after some bug has been triggered.
XXX HERE (assertions)

Testing
-------

Separation of Concerns
......................

* components don't talk to one another

Dependency Injection
....................


Implementation Challenges
=========================

Data Aliasing
-------------

(need for .copy())

Catching Up
-----------

Follow the Leader
-----------------

Further Extensions
==================

Consistent memory usage
-----------------------

View changes
------------
