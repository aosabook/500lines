Clustering by Consensus
***********************

In this chapter, we'll explore implementation of a network protocol designed to support reliable a distributed computation.
Network protocols can be difficult to implement correctly, so we'll look at some techniques for minimizing bugs and for catching and fixing the remaining few.
Building reliable software, too, requires some special development and debugging techniques.

The focus is on the protocol implementation, but as a motivating example we'll consider a simple bank account management service.
In this application, each account has a current balance and is identified with an account number, like a simple key-value store.
However, operations on those accounts are more complex than for an ordinary key-value store.
The "transfer" operation operates on two keys at once -- the source and destination accounts -- and must be rejected if the source account's balance is too low.

If the service is hosted on a single server, this is easy to implement: serialize the transfer method using a lock, and verify the souce account's balance in that method.
However, a bank cannot rely on a single server for its critical acocunt balances!
Instead, the service is *distributed* over multiple servers, with each running a separate copy of the code.
The servers communicate between themselves to maintain the illusion of a single, distributed service.
This introduces some new failure modes:

 * If two servers receive execute different transfer requests from the same account at the same time, the account may be overdrawn.
 * If two servers update the same account balance at the same time, one might overwrite the changes made by the other.
 * If communication between some servers fails, each server may determine that the others have failed and continue to process transfers that conflict with the others.

Distributed State Machines
==========================

Fundamentally, these failures occur when servers use their local state to perform operations, without first ensuring that state matches the state on other servers.
The technique for avoiding such problems is called a "distributed state machine".
The idea is that each server executes the same deterministic state machine on the same inputs.
By the nature of state machines, then, each server will see exactly the same outputs.
Operations such as "transfer" or "balance-check" provide the inputs to the state machine.

The state machine for this application is simple:

    def execute_operation(state, input):
        if input.operation == 'transfer':
            if state.accounts[input.source_account] < input.amount:
                return state, False
            state.accounts[input.source_account] -= input.amount
            state.accounts[input.destination_account] += input.amount
            return state, True
        elif input.operation == 'balance-check':
            return state, state.accounts[input.account]

Note that the "balance-check" operation does not modify the state, but is still implemented as a state transition.
This guarantees that the returned balance is the latest information in the cluster, and not based on the state on a single server.

So, the distributed state machine technique ensures that the same operations occur on each host.
But the problem remains of ensuring that every server agrees on the inputs to the state machine.
This is a problem of *consensus*, and we'll address it with derivative of the Paxos algorithm.

Consensus by Paxos
==================

Paxos was described by Leslie Lamport in a fanciful paper, first submitted in 1990 and eventually published in 1998, entitled "The Part-Time Parliament".
Lamport's paper has a great deal more detail than we will get into here, and is a fun read.
The references at the end of the chapter describe some extensions of the algorithm that we have adapted in this implementation.

The simplest form of Paxos provides a way for a set of servers to agree on one fact, for all time.
MultiPaxos builds on this foundation by agreeing on a numbered sequence of facts, one at a time.
To implement a distributed state machine, we use MultiPaxos to agree on each state-machine input, and execute them in sequence.

Simple Paxos
------------

So let's start with "Simple Paxos".
For sake of illustration, let's agree on today's lottery number.
While there may be another lottery next week, *today's* lottery number, once decided, will never change.

A round of Paxos begins with one member of the cluster (called the proposer) deciding to propose a value, let's say 7-3-22.
The proposer selects a "ballot number" *N* greater than any other it has ever sent, and sends a ``PREPARE`` message with a ballot number to the acceptors (all of the other nodes).
The proposer the waits to hear from a majority of the acceptors.

On receiving to a ``PREPARE`` with a ballot number *N*, each acceptor promises to ignore all proposals with a ballot number less than *N*.
That is, unless it has already made that promise for a higher ballot number.
It replies with a ``PROMISE`` message including the value it has already accepted, if any.

When the proposer receives a ``PROMISE`` including an already-accepted value, it must continue with that acceped value, and not its own.
Continuing the example, however, other value has been accepted, so the acceptors all send back a ``PROMISE`` with no value.

When the proposer has heard back from a majority of the acceptors, it sends an ``ACCEPT``, including the ballot number and value to all acceptors.
Unless it would violate a promise, each acceptor records the value from the ``ACCEPT`` message as accepted.
It then replies with an ``ACCEPTED`` message containing the accepted ballot number.
The round is complete and the value decided when the proposer has heard from a majority of acceptors.

If another server proposes a different value, say 11-13-15, with lower ballot number, then the acceptors will ignore the ``PREPARE`` messages.
If that server uses a higher ballot number than has already been promised, then the acceptors will reply with the already-agreed value, 7-3-22.
The server will use that value in the second phase, agreeing again on the same value.

The overall effect is two round-trips: one to secure a promise and one to get agreement on the value.
When multiple proposers make a proposal at the same time, it is common for neither proposal to win as each proposal secures a promise to ignore the other before its proposal is accepted.
In this case, both proposers then re-propose.
Hopefully one wins, but the deadlock can continue indefinitely if the timing works out just right.
In bad -- but not uncommon -- cases, it can take dozens of round-trips to reach consensus.

Multi-Paxos
-----------

Multi-Paxos is, in effect, a sequence of simple Paxos instances (slots), each numbered sequentially.

To avoid the hight cost of two round trips per decision, multi-Paxos treats the ``PREPARE``/``PROMISE`` interaction as authoritative for the current and all future slots.
Once a proposer has received promises without values from a majority of acceptors, it only executes the second phase (``ACCEPT``/``ACCEPTED``) for subsequent slots, at least until a proposal isn't accepted.

Getting Moderately Complex
--------------------------

A surprising amount of additional infrastructure is required to make this model effective:
 * starting the cluster from scratch
 * leader election to avoid deadlock
 * a gossip protocol to bring lagging members up to date (XXX may be removed)

XXX - introduce components based on Renesee(sp?)

Leader Elections
----------------

XXX make the bridge from just optimizing the first phase by getting it out of the way, to "electing a leader" with the first phase.
I'm still fuzzy on this myself, so I need to re-read some of the relevant papers to see how they describe it.

Introducing Cluster
===================

The "Cluster" library in this chapter implements a simple form of Multi-Paxos.
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
Each class has a different role in the protocol.

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
Replicas distinguish *decided* proposals, on which the cluster has agreed, from *committed* proposals, which the local state machine has processed.
When propsals are decided out of order, the committed proposals may lag behind, waiting for the next slot to be decided.

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

The most important debugging feature in Cluster is a *deterministic* simulator.
Unlike a real network, it will behave exactly the same way on every run, given the same seed for the random number generator.
This means that we can add additional debugging checks or output to the code and re-run the simulation to see the same failure in more detail.

Of course, much of that detail is in the messages sent and received by the different nodes and components, so those are automatically logged in their entirety.
That logging includes the component sending or receiving the message, as well as the simulated timestamp, injected via the ``SimTimeLogger`` class.

A resilient protocol such as this one can often run for a long time after some bug has been triggered.
For example, during development, a data aliasing error caused all replicas to share the same ``decisions`` dictionary.
This meant that once a decision was handled on one node, all other nodes saw it as already decided.
Even with this serious bug, the cluster produced correct results for several transactions before deadlocking.

Assertions are an important tool to catch this sort of error early.
Assertions should include any invariants from the algorithm design, but when the code doesn't behave as we expect, asserting our expectations is a great way to see where things go astray.

Identifying the right assumptions we make while reading code is a part of the art of debugging.
In this case, the problem was that the ``DECISION`` for the next slot to commit was being ignored because it was already in ``self.decisions``.
The underlying assumption being violated was that the next slot to be committed was not yet decided.
Asserting this at the beginning of ``do_DECISION`` identified the flaw and led quickly to the fix.

Many other assertions were added during development of the protocol, but in the interests of space, only a few remain.

Testing
-------

Sometime in the last 10 years, code without tests finally became as crazy as driving without a seatbelt.
Code without tests is probably incorrect, and modifying the code is risky without a way to see if its behavior has changed.

Testing is most effective when the code is organized for testability.
There are a few active schools of thought in this area, but the approach we've taken is to divide the code into small, minimally connected units that can be tested in isolation.
This agrees nicely with the component model, where each component has a specific purpose and can operate in isolation from the others.

Cluster is written to maximize that isolation.
All communication between components takes place via messages, with the exception of creating new components.
For the most part, then, components can be tested by sending messages to them and observing their responses.

Dependency Injection
....................

We use a technique called "dependency injection" to handle creation of new components.
Each component which creates other components takes a list of class objects as constructor arguments, defaulting to the actual classes.
For example, ``Leader``'s constructor looks like

.. code-block::

    def __init__(self, node, peers, commander_cls=Commander, scout_cls=Scout):
        # ..
        self.commander_cls = commander_cls
        self.scout_cls = scout_cls

The ``spawn_scout`` method (and, similarly, ``spawn_commander``) create the new component with

.. code-block::

    sct = self.scout_cls(self.node, self.ballot_num, self.peers)

The magic of this technique is that, in testing, ``Leader`` can be given stub classes and thus tested separately from ``Scout`` and ``Commander``.

Unit Testing
............

XXX include test_leader.py? parts of it? I wasn't counting that in the 500 lines..

One pitfall of a focus on small units is that it does not test the interfaces between units.
For example, unit tests for the acceptor component verify the format of the ``accepted`` attribute of the ``PROMISE`` message, and the unit tests for the scout component supply well-formatted values for the attribute.
Neither test checks that those formats match.

One approach to fixing this issue is to make the interfaces self-enforcing.
In Cluster, the use of named tuples and keyword arguments avoids any disagreement over messages' attributes.
Because the only interaction between components is via messages, this covers a substantial part of the interface.

For specific issues such as the format of ``accepted``, both the real and test data can be verified using the same function, in this case ``verifyPromiseAccepted``.
The tests for the acceptor use this method to verify each returned ``PROMISE``, and the tests for the scout use it to verify every fake ``PROMISE``.

Integration Testing
...................

The final bulwark against interface problems and design errors is integration testing.
An integration test assembles multiple units together and tests their combined effect.
In our case, that means building a network of several nodes, injecting some requests into it, and verifying the results.
If there are any interface issues not discovered in unit testing, they should cause the integration tests to fail quickly.

Because the protocol is intended to handle node failure gracefully, we test a few failure scenarios as well, including the untimely failure of the active leader.

Integration tests are harder to write than unit tests, because they are less well isolated.
For Cluster, this is clearest in testing the failed master, as the active leader depends on every detail of the protocol's operation.
Even with a deterministic network, a change in one message alters the random number generator's state and thus unpredictably changes later events.
Rather than hard-coding the expected leader, the test code must dig into the internal state of each leader to find one that believes itself to be active.

Implementation Challenges
=========================

Catching Up
-----------

In "pure" MultiPaxos, nodes which fail to receive messages can be many slots behind the rest of the cluster.
As long as the state of the distributed state machine is never accessed except via state machine transitions, this design is functional.
To read from the state, the client requests a state-machine transition that does not actually alter the state, but which returns the desired value.
This transition is executed cluster-wide, ensuring that it returns the same value everywhere, based on the state at the slot in which it is proposed.

Even in the optimal case, this is slow, requiring several round trips just to read a value.
If a distributed object store made such a request for every object access, its performance would be dismal.
But when the node receiving the request is lagging behind, the request delay is much greater as that node must catch up to the rest of the cluster before making a successful proposal.

XXX I may rip this bit of the code out and move it to a "Further Extensions" section, if I can demonstrate that the implementation is slow but correct without it

Follow the Leader
-----------------

XXX based on the "Leader Elections" section above, this describes the sensitivity of the implementation to rough agreement on the identity of the current leader.
Basically, if there's even a little disagreement over the current leader, it triggers a "fight" which nearly deadlocks the implementation.

Further Extensions
==================

Consistent memory usage
-----------------------

A cluster-management library provides reliability in the presence of unreliable components.
It shouldn't add unreliability of its own.
Unfortunately, Cluster will not run for long without failing due to ever-growing memory use and message size.

In the protocol definition, acceptors and replicas form the "memory" of the protocol, so they need to remember everything.
These components never know when they will receive a request for an old slot, perhaps from a lagging replica or leader.
To maintain correctness, then, they keep a list of every decision, ever, since the cluster was started.
Worse, these decisions are transmitted between replicas in ``WELCOME`` messages, making these messages enormous in a long-lived cluster.

One technique to address this issue is to periodically "checkpoint" each node's state, keeping information some limited number of decisions on-hand.
Nodes which are so out of date that they have not committed all slots up to the checkpoint must "reset" themselves by leaving and re-joining the cluster.

View Changes
------------

Operations engineers need to be able to resize clusters to meet load and availability requirements.
A simple test project might begin with a minimal cluster of three nodes, where any one can fail without impact.
When that project goes "live", though, the additional load would require a larger cluster.

Cluster, as written, cannot change the set of peers in a cluster without restarting the entire cluster.
Ideally, the cluster would be able to maintain a consensus about its membership, just as it does about state machine transitions.
"The Part-Time Parliament" (citation) has a cryptic paragraph about how this might work:

    The Paxons decided to add and remove members of Parliament by decree.
    This posed a circularity problem: membership in Parliament was determined by which decrees were passed, but passing a decree required knowing what constituted a majority set, which in turn depended upon who was a member of Parliament.
    The circularity was broken by letting the membership of Parliament used in passing decree n be specified by the law as of decree n − 3.
    A president could not try to pass decree 3255 until he knew all decrees through decree 3252.

Translated into the terms used by Cluster, this means that the set of cluster members (the *view*) can be changed by special view-change proposals.
Every slot has a view: either the view from the previous slot, or the new view decided in that slot.

For the protocol to work correctly, all nodes need to agree on the view used to decide each slot.
And this is where Lamport's circularity arises: what view is used to decide a view change?
Not the new view: since it hasn't been decided, not all nodes agree on it.
Selecting the view from the previous slot makes sense.
In fact, this means that *all* slots must be decided in the view of the previous slot, as the cluster hasn't decided that a slot is a view change until the slot is decided.

This model works, but serializes the protocol: slots must be decided one by one, even in the presence of slow or failing nodes.
Slot n can't be decided until the view for slot n-1 is known, which requires deciding that slot.

Lamport's suggested solution is to use the view from 3 slots back instead.
There's nothing special about the number 3 -- only that it allows a window of slots for parallelization without requiring too many decisions to make a view change.

In early drafts of this implementation (dutifully preserved in the git history!), I implemented support for view changes (using α in place of 3).
This seemingly simple change introduced a great deal of complexity:
* tracking the view for each of the last α committed slots and correctly sharing this with new nodes
* ignoring proposals for which no slot is available
* detecting failed nodes,
* properly serializing multiple competing view changes, and
* communciating view information between the leader and replica.

The result was far too large for this book!

References
==========

(I'm not sure what the book's citation style is, but these are unambiguous enough for the review)

* Lamport - "The Part-Time Parliament"
* Lamport - "Paxos Made Simple"
* Renesse - "Paxos Made Moderately Complex" (the origin of the component names)
* Chandra, Griesemer, and Redstone - "Paxos Made Live - An Engineering Perspective" (regarding snapshots, in particular)
* Mazieres - "Paxos Made Practical" (view changes, although not of the type described here)
* Liskov - "From Viewstamped Replication to Byzantine Fault Tolerance" (another, different look at view changes)
* http://stackoverflow.com/questions/21353312/in-part-time-parliament-why-does-using-the-membership-from-decree-n-3-work-to
