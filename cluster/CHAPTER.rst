Clustering by Consensus
***********************

In this chapter, we'll explore implementation of a network protocol designed to support reliable distributed computation.
Network protocols can be difficult to implement correctly, so we'll look at some techniques for minimizing bugs and for catching and fixing the remaining few.
Building reliable software, too, requires some special development and debugging techniques.

Motivating Example
==================

The focus of this chapter is on the protocol implementation, but as a motivating example let's consider a simple bank account management service.
In this service, each account has a current balance and is identified with an account number.
Users access the accounts by requesting operations like "deposit", "transfer", or "get-balance".
The "transfer" operation operates on two accounts at once -- the source and destination accounts -- and must be rejected if the source account's balance is too low.

If the service is hosted on a single server, this is easy to implement: use a lock to make sure that transfer operations don't run in parallel, and verify the souce account's balance in that method.
However, a bank cannot rely on a single server for its critical account balances!
Instead, the service is *distributed* over multiple servers, with each running a separate instance of exactly the same code.
The servers communicate between themselves to maintain the illusion of a single, distributed service.
Users can then contact any server to perform an operation.
This introduces some new failure modes:

 * If two servers receive execute different transfer requests from the same account at the same time, the account may be overdrawn.
 * If two servers update the same account balance at the same time, one might overwrite the changes made by the other.
 * If communication between the servers fails, different servers may calculate different balances based on incomplete information, breaking the illusion of a single service.

Distributed State Machines
==========================

Fundamentally, these failures occur when servers use their local state to perform operations, without first ensuring that the local state matches the state on other servers.
The technique for avoiding such problems is called a "distributed state machine".
The idea is that each server executes the same deterministic state machine on the same inputs.
By the nature of state machines, then, each server will see exactly the same outputs.
Operations such as "transfer" or "get-balance" provide the inputs to the state machine.

The state machine for this application is simple:

    def execute_operation(state, input):
        if input.operation == 'transfer':
            if state.accounts[input.source_account] < input.amount:
                return state, False
            state.accounts[input.source_account] -= input.amount
            state.accounts[input.destination_account] += input.amount
            return state, True
        elif input.operation == 'get-balance':
            return state, state.accounts[input.account]

Note that the "get-balance" operation does not modify the state, but is still implemented as a state transition.
This guarantees that the returned balance is the latest information in the cluster, and not based on the state on a single server.

So, the distributed state machine technique ensures that the same operations occur on each host.
But the problem remains of ensuring that every server agrees on the inputs to the state machine.
This is a problem of *consensus*, and we'll address it with derivative of the Paxos algorithm.

Consensus by Paxos
==================

Paxos was described by Leslie Lamport in a fanciful paper, first submitted in 1990 and eventually published in 1998, entitled "The Part-Time Parliament".
Lamport's paper has a great deal more detail than we will get into here, and is a fun read.
The references at the end of the chapter describe some extensions of the algorithm that we have adapted in this implementation.

The simplest form of Paxos provides a way for a set of servers to agree on one value, for all time.
MultiPaxos builds on this foundation by agreeing on a numbered sequence of facts, one at a time.
To implement a distributed state machine, we use MultiPaxos to agree on each state-machine input, and execute them in sequence.

Simple Paxos
------------

So let's start with "Simple Paxos", also known as the Synod protocol.
For sake of illustration, we'll use it to agree on today's lottery number.
While there may be another lottery drawing next week, *today's* lottery number, once decided, will never change.

The protocol operates in a series of ballots, each led by a single member of the cluster, called the proposer.
Each ballot has a unique ballot number based on an integer and the proposer's identity.
The proposer's goal is to get a majority of cluster members to accept its value, but only if another value has not already been decided.

A ballot begins with the proposer sending a ``Prepare`` message with the ballot number *N* to the acceptors and waiting to hear from a majority.

The ``Prepare`` message is a request for the accepted value (if any) with the highest ballot number less than *N*.
Acceptors respond with a ``Promise`` containing any value they have already accepted, and promising not to accept any ballot numbered less than *N* in the future.
If the acceptor has already made a promise for a larger ballot number, it includes that number in the ``Promise``, indicating that the proposer has been pre-empted.

When the proposer has heard back from a majority of the acceptors, it sends an ``Accept`` message, including the ballot number and value to all acceptors.
If the proposer did not receive any existing value from any acceptor, then it sends its own desired value.
Otherwise, it sends the value from the highest-numbered ballot.
Unless it would violate a promise, each acceptor records the value from the ``Accept`` message as accepted and replies with an ``Accepted`` message.
The ballot is complete and the value decided when the proposer has heard its ballot number from a majority of acceptors.

Returning to the example, initially no other value has been accepted, so the acceptors all send back a ``Promise`` with no value, and the proposer sends an ``Accept`` containing its value, 7-3-22.

If another proposer later initiates lower-numbered ballot, the acceptors will not accept it.
If that ballot has a larger ballot number, then the ``Promise`` from the acceptors will contain 7-3-22, and the proposer will send that value in the ``Accept`` message.
The new ballot will be accepted, but in favor of the same value as the first ballot.

In fact, the protocol will never allow two different values to be decided, even if the ballots overlap, messages are delayed, or a minority of acceptors fail.

When multiple proposers make a ballot at the same time, it is easy for neither ballot to be accepted.
Both proposers then re-propose, and hopefully one wins, but the deadlock can continue indefinitely if the timing works out just right.
In a bad -- but not uncommon -- case, it can take dozens of round-trips to reach consensus.

Multi-Paxos
-----------

Reaching consensus on a single, static value is not particularly useful on its own.
Clustered systems want to agree on a particular state that evolves over time.
In the case of the bank account service, the state is the collection of account balances.
We use Paxos to agree on each operation, treated as a state machine transition.

Multi-Paxos is, in effect, a sequence of simple Paxos instances (slots), each numbered sequentially.
Each state transition is given a "slot number", and each member of the cluster executes transitions in strict numeric order.
To change the cluster's state (to process a withdrawal, for example), we try to achieve consensus on that operation in the next slot.

Running Paxos for every slot, with its minimum of two round trips, would be too slow.
Multi-Paxos optimizes by using the same set of ballot numbers for all slots, and performing the ``Prepare``/``Promise`` phase for all slots at once.

Paxos Made .. Pretty Hard, Actually
-----------------------------------

Implementing Multi-Paxos in practical software is notoriously difficult, spawning a number of papers mocking Lamport's "Paxos Made Simple" with titles like "Paxos Made Practical".

First, the multiple-proposers problem described above can become problematic in a busy environment, as each cluster member attempts to get its state machine operation decided in each slot.
The fix is to elect a "leader" which is responsible for submitting ballots for each slot.
Thus, in normal operation with only one leader, ballot conflicts do not occur.

The ``Prepare``/``Promise`` phase can function as a kind of leader election.
Whichever cluster member owns the most recently promised ballot number is considered the leader.
As we'll see below, leader elections are actually quite complex.

Although simple Paxos guarantees that the cluster will not reach conflicting decisions, it cannot guarantee that any decision will be made.
Fixing this requires carefully orchestrated re-transmissions: enough to eventually make progress, but not so many that the cluster buries itself in a packet storm.

Another problem is the dissemination of decisions.
A simple broadcast of a ``Decision`` message can take care of this for the normal case.
If the message is lost, though, a node can remain permanently ignorant of the decision and unable to apply state machine transitions for later slots.
So an implementation needs some mechanism for sharing information about decided proposals.

Our use of a distributed state machine presents another interesting challenge: start-up.
When a new node starts, it needs to catch up on the existing state of the cluster.
Although it can do so by catching up on decisions for all slots since the first, in a mature cluster this may involve millions of slots.
Furthermore, we need some way to initialize a new cluster.

Introducing Cluster
===================

The "Cluster" library in this chapter implements a simple form of Multi-Paxos.
It is designed as a library to provide a consensus service to a larger application.
The application creates and starts a ``Member`` object on each cluster member, providing an application-specific state machine and a list of peers.
The application accesses the shared state through the ``invoke`` method, which kicks off a proposal for a state transition.
Once that proposal is decided and the state machine runs, ``invoke`` returns the machine's output.

Users of this library will depend on its correctness, so it's important to structure the code so that we can see -- and test -- its correspondance to the specification.
Complex protocols can exhibit complex failures, too, so we will build support for reproducing and debugging rare failures.

Message Types
-------------

Cluster's protocol uses 15 different message types.
Using named tuples to describe each message type keeps the code clean and helps avoid some simple errors.
The named tuple constructor will raise an exception if it is not given exactly the right attributes, making typos obvious.
The tuples format themselves nicely in log messages, and as an added bonus don't use as much memory as a dictionary.

Component Model
---------------

Humans are limited by what we can hold in our active memory.
We can't reason about the entire Cluster implementation at once -- it's just too much, and too easy to miss details.
Instead, we break Cluster down into a handful of components, implemented as subclasses of ``Component``.
Each class is responsible for a different part of the protocol.
The division of the components is based on that given in (Renesse, 2011).

The components are glued together by the ``Node`` class, which represents a single node on the network.
Components are added to and removed from the node as execution proceeds.
Messages that arrive on the node are relayed to all active components, calling a method named after the message type with a ``do_`` prefix.
These ``do_`` methods receive the message's attributes as keyword arguments for easy access.

The ``Node`` class also provides some convenience methods, using ``functools.partial`` to supply some arguments to the same methods of the ``Network`` class.

Acceptor
........

The ``Acceptor`` class illustrates the component model well.
It implements the acceptor role in the protocol, so it must store the ballot number representing its most recent promise, along with the set of accepted proposals for each slot.
It then responds to ``Prepare`` and ``Accept`` messages according to the protocol.
The result is a short class that is easy to compare to the protocol.

Replica
.......

The ``Replica`` class is the most complicated component class, as it has a few closely related responsibilities:

* Making new proposals;
* Invoking the local state machine when proposals are decided;
* Tracking the current leader; and
* Adding newly started nodes to the cluster.

The replica creates new proposals in response to ``Invoke`` messages from clients, selecting what it believes to be an unused slot and sending a ``Propose`` message to the current leader.
Furthermore, if the consensus for the selected slot is for a different proposal, the replic must re-propose with a new slot.

``Decision`` messages represent slots on which the cluster has come to consensus.
Here, replicas store the new decision, then run the state machine until it reaches an undecided slot.
Replicas distinguish *decided* slots, on which the cluster has agreed, from *committed* slots, which the local state machine has processed.
When slots are decided out of order, the committed proposals may lag behind, waiting for the next slot to be decided.

In some circumstances, it's possible for a slot to have no active proposals and no decision.
The state machine is required to execute slots one by one, so the cluster much reach a consensus on something to fill the slot.
To protect against this possibility, replicas make a "no-op" proposal whenever they catch up on a slot.
If such a proposal is eventually decided, then the state machine does nothing for that slot.

Likewise, it's possible for the same proposal to be decided twice.
The replica skips invoking the state machine for any such duplicate proposals, performing no transition for that slot.

Replicas need to know which node is the active leader in order to send ``Propose`` messages to it.
There is a surprising amount of subtlty required to get this right, as we'll see later.
Each replica tracks the active leader using three sources of information:

* When the leader component becomes active, it sends an ``Adopted`` message to its local replica.
* When the acceptor component sends a ``Promise`` to a new leader, it sends an ``Accepting`` message to its local replica.
* The active leader sends ``Active`` messages as a heartbeat.
  If no such message arrives before the ``LEADER_TIMEOUT`` expires, the replica assumes the leader is dead and moves on to the next leader.
  In this case, it's important that all replicas choose the *same* new leader, which we accomplish by sorting the members and selecting the next one in the list.

Finally, when a node joins the network, the bootstrap component sends a ``Join`` message.
The replica responds with a ``Welcome`` message containing its most recent state, allowing the new node to come up to speed quickly.

Leader, Scout, and Commander
............................

The leader's primary task is to take ``Propose`` messages requesting new ballots and produce decisions.
A leader is "active" when it has successfully carried out the ``Prepare``/``Promise`` portion of the protocol.
An active leader can immediately send an ``Accept`` message in response to a ``Propose``.

In keeping with the component model, the leader delegates to the scout and commander components to carry out each portion of the protocol.

The leader creates a scout component when it wants to become active, in response to receiving a ``Propose``.
The scout sends (and re-sends, if necessary) a ``Prepare`` message, and collects ``Promise`` responses until it has heard from a majority of its peers or until it has been preempted.
It communicates the result back to the leader with an ``Adopted`` or ``Preempted`` message, respectively.

The leader creates a commander component for each slot where it has an active proposal.
Like a scout, a commander sends and re-sends ``Accept`` messages and waits for a majority of acceptors to reply with ``Accepted``, or for news of its preemption.
When a proposal is accepted, the commander broadcasts a ``Decision`` message to all nodes.
It responds to the leader with either ``Decided`` or ``Preempted``.

.. note::

    A surprisingly subtle bug appeared here during development.
    At the time, the network simulator introduced packet loss even on messages within a node.
    When *all* ``Decision`` messages were lost, the protocol could not proceed.
    The replica continued to re-transmit ``Propose`` messages, but the leader ignored them as it already had a proposal for that slot.
    The replica's catch-up process could not find the result, as no replica had heard of the decision.
    The solution was to ensure that local messages are always delivered, as is the case for real network stacks.


Bootstrap
.........

When a node joins the cluster, it must determine the current cluster state before it can participate.
The bootstrap component handles this by sending ``Join`` messages to each peer in turn until it receives a ``Welcome``.

An early version of the implementation started each node with a full set of components (replica, leader, and acceptor), each of which began in a "startup" phase, waiting for information from the ``Welcome`` message.
This spread the initialization logic around every component, requiring separate testing of each one.
The final design has the bootstrap component creating each of the other components once startup is complete, passing the initial state to their constructors.

Seed
....

In normal operation, when a node joins the cluster, it expects to find the cluster already running, with at least one node willing to respond to a ``Join`` message.
But how does the cluster get started?
An option is for the bootstrap component to decide, after attempting to contact every other node, that it is the first in the cluster.
But this has two problems.
First, for a large cluster it means a long wait while each ``Join`` times out.
More importantly, in the event of a network partition, a new node might be unable to contact any others and start a new cluster.
When the network heals and that node can communicate with the other nodes, there are two clusters with different decisions for the same slots!

To avoid this outcome, creating a new cluster is a user-specified operation.
Exactly one node in the cluster runs the seed component, with the others running bootstrap as usual.
The seed waits until it has received ``Join`` messages from a majority of its peers, then sends a ``Welcome`` with an initial state for the state machine and an empty set of decisions.
The seed component then stops itself and starts a bootstrap component to join the newly-seeded cluster.

Request
.......

The request component manages a request to the distributed state machine.
The component simply sends ``Invoke`` messages to the local replica until it receives a corresponding ``Invoked``.

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
In this case, the problem was that the ``Decision`` for the next slot to commit was being ignored because it was already in ``self.decisions``.
The underlying assumption being violated was that the next slot to be committed was not yet decided.
Asserting this at the beginning of ``do_Decision`` identified the flaw and led quickly to the fix.

Many other assertions were added during development of the protocol, but in the interests of space, only a few remain.

Testing
-------

Sometime in the last 10 years, code without tests finally became as crazy as driving without a seatbelt.
Code without tests is probably incorrect, and modifying the code is risky without a way to see if its behavior has changed.

Testing is most effective when the code is organized for testability.
There are a few active schools of thought in this area, but the approach we've taken is to divide the code into small, minimally connected units that can be tested in isolation.
This agrees nicely with the component model, where each component has a specific purpose and can operate in isolation from the others.

Cluster is written to maximize that isolation: all communication between components takes place via messages, with the exception of creating new components.
For the most part, then, components can be tested by sending messages to them and observing their responses.

Unit Testing
............

The unit tests for Cluster (all of which are availble in the book's Github repository) are simple and short:

.. code-block::

    def test_propose_active(self):
        """A PROPOSE received while active spawns a commander."""
        self.activate_leader()
        self.node.fake_message(Propose(slot=10, proposal=PROPOSAL1))
        self.assertCommanderStarted(Ballot(0, 'F999'), 10, PROPOSAL1)

This method tests a single behavior (commander spawning) of a single unit (the ``Leader`` class).
It follows the well-known "arrange, act, assert" pattern: set up an active leader, send it a message, and check the result.

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

Interface Correctness
.....................

One pitfall of a focus on small units is that it does not test the interfaces between units.
For example, unit tests for the acceptor component verify the format of the ``accepted`` attribute of the ``Promise`` message, and the unit tests for the scout component supply well-formatted values for the attribute.
Neither test checks that those formats match.

One approach to fixing this issue is to make the interfaces self-enforcing.
In Cluster, the use of named tuples and keyword arguments avoids any disagreement over messages' attributes.
Because the only interaction between components is via messages, this covers a substantial part of the interface.

For specific issues such as the format of ``accepted_proposals``, both the real and test data can be verified using the same function, in this case ``verifyPromiseAccepted``.
The tests for the acceptor use this method to verify each returned ``Promise``, and the tests for the scout use it to verify every fake ``Promise``.

Integration Testing
...................

The final bulwark against interface problems and design errors is integration testing.
An integration test assembles multiple units together and tests their combined effect.
In our case, that means building a network of several nodes, injecting some requests into it, and verifying the results.
If there are any interface issues not discovered in unit testing, they should cause the integration tests to fail quickly.

Because the protocol is intended to handle node failure gracefully, we test a few failure scenarios as well, including the untimely failure of the active leader.

Integration tests are harder to write than unit tests, because they are less well isolated.
For Cluster, this is clearest in testing the failed leader, as any node could be the active leader.
Even with a deterministic network, a change in one message alters the random number generator's state and thus unpredictably changes later events.
Rather than hard-coding the expected leader, the test code must dig into the internal state of each leader to find one that believes itself to be active.

Fuzz Testing
............

It's very difficult to test resilient code: it is likely to be resilient to its own bugs, so integration tests may not detect even very serious bugs.
It is also hard to imagine and construct tests for every possible failure mode.

A common approach to this sort of problem is "fuzz testing": running the code repeatedly with randomly changing inputs until something breaks.
When something *does* break, all of the debugging support becomes critical: if the failure can't be reproduced, and the logging information isn't sufficient to find the bug, then you can't fix it!

I performed some manual fuzz testing of cluster during development, but a full fuzz-testing infrastructure is beyond the scope of this project.

Implementation Challenges
=========================

Follow the Leader
-----------------

A cluster with many active leaders is a very noisy place, with scouts sending ever-increasing ballot numbers to acceptors, and no ballots being decided.
A cluster with no active leader is quiet, but equally nonfunctional.
Balancing the implementation so that a cluster almost always agrees on exactly one leader is remarkably difficult.

It's easy enough to avoid fighting leaders: when preempted, a leader just accepts its new inactive status.
However, this easily leads to a case where there are no active leaders, so an inactive leader will try to become active every time it gets a ``Propose`` message.

If the whole cluster doesn't agree on which member is the active leader, there's trouble: different replicas send ``Propose`` messages to different leaders, leading to battling scouts.
So it's important that leader elections be decided quickly, and that all cluster members find out about the result as quickly as possible.

Cluster handles this by detecting a leader change as quickly as possible: when an acceptor sends a ``Promise``, chances are good that the promised member will be the next leader.
Failures are detected with a heartbeat protocol.

Further Extensions
==================

Catching Up
-----------

In "pure" MultiPaxos, nodes which fail to receive messages can be many slots behind the rest of the cluster.
As long as the state of the distributed state machine is never accessed except via state machine transitions, this design is functional.
To read from the state, the client requests a state-machine transition that does not actually alter the state, but which returns the desired value.
This transition is executed cluster-wide, ensuring that it returns the same value everywhere, based on the state at the slot in which it is proposed.

Even in the optimal case, this is slow, requiring several round trips just to read a value.
If a distributed object store made such a request for every object access, its performance would be dismal.
But when the node receiving the request is lagging behind, the request delay is much greater as that node must catch up to the rest of the cluster before making a successful proposal.

A simple solution is to implement a gossip-style protocol, where each replica periodically contacts other replicas to share the highest slot it knows about and to request information on unknown slots.
Then even when a ``Decision`` message was lost, the replica would quickly find out about the decision from one of its peers.

Consistent memory usage
-----------------------

A cluster-management library provides reliability in the presence of unreliable components.
It shouldn't add unreliability of its own.
Unfortunately, Cluster will not run for long without failing due to ever-growing memory use and message size.

In the protocol definition, acceptors and replicas form the "memory" of the protocol, so they need to remember everything.
These components never know when they will receive a request for an old slot, perhaps from a lagging replica or leader.
To maintain correctness, then, they keep a list of every decision, ever, since the cluster was started.
Worse, these decisions are transmitted between replicas in ``Welcome`` messages, making these messages enormous in a long-lived cluster.

One technique to address this issue is to periodically "checkpoint" each node's state, keeping information some limited number of decisions on-hand.
Nodes which are so out of date that they have not committed all slots up to the checkpoint must "reset" themselves by leaving and re-joining the cluster.

Persistent Storage
------------------

While it's OK for a minority of cluster members to fail, it's not OK for an acceptor to "forget" any of the values it has accepted or promises it has made.

Unfortunately, this is exactly what happens when a cluster member fails and restarts: the newly initialized Acceptor instance has no record of the promises its predecessor made.
The problem is that the newly-started instance takes the place of the old

There are two alternatives to solve this issue.
The simpler solution involves writing acceptor state to disk and re-reading that state on startup.
The more complex solution is to remove failed cluster members from the cluster, and require that new members be added to the cluster.
This kind of dynamic adjustment of the cluster membership is called a "view change".

View Changes
------------

Operations engineers need to be able to resize clusters to meet load and availability requirements.
A simple test project might begin with a minimal cluster of three nodes, where any one can fail without impact.
When that project goes "live", though, the additional load would require a larger cluster.

Cluster, as written, cannot change the set of peers in a cluster without restarting the entire cluster.
Ideally, the cluster would be able to maintain a consensus about its membership, just as it does about state machine transitions.
This means that the set of cluster members (the *view*) can be changed by special view-change proposals.
But the Paxos algorithm depends on universal agreement about the members in the cluster, so we must define the view for each slot.

Lamport addresses this challeng in the final paragraph of "Paxos Made Simple":

    We can allow a leader to get *α* commands ahead by letting the set of servers that execute instance *i+α* of the consensus algorithm be specified by the state after execution of the *i*\th state machine command.  (Lamport, 2001)

The idea is that each instance of Paxos (slot) uses the view from α slots earlier.
This allows the cluster to work on, at most, α slots at any one time, so a very small value of α limits concurrency, while a very large value of α makes view changes slow to take effect.

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
