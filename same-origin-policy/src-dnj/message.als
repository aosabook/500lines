/**
	* message.als
	* 	A model of messaging between endpoints
	*/
module message

//open util/ordering[Msg] as ord
open stepper[Msg] as ord


abstract sig Resource {}
abstract sig EndPoint {
	owns : set Resource
}
// One-way message
abstract sig Msg {
	-- each message is associated with a sender and a receiver
	from, to : EndPoint,
	-- each message may contain a number of resources
	args : set Resource,
	returns : set Resource
}

// Returns the messages that the given endpoint ep receives
fun receives[ep : EndPoint] : set Msg {
	ep.~to
}

// Returns the messages that the given endpoint ep sends
fun sends[ep : EndPoint] : set Msg {
	ep.~from
}

// Returns the resources the given endpoint ep can access
fun accesses[ep : EndPoint] : set Resource {
	-- "ep" can only access a resource "r" iff 
	-- (1) it owns "r" or 
	-- (2) if "ep" receives a message that carries "r" or
	-- (3) if "ep" sends a message that returns "r" 
	ep.owns + receives[ep].args + sends[ep].returns
}

// A payload in a message must be owned by the sender or received by the sender
// as part of a previous message			
fact ResourceConstraints {
	all ep : EndPoint, msg : sends[ep] |
		let prevMsgs = prevs[msg] |
			msg.args in ep.owns + (prevMsgs & receives[ep]).args + (prevMsgs & sends[ep]).returns
}

// Generates an instance with a non-empty message between two different end
// points.
// bound: exactly 2 EndPoints, 1 Msg and 1 Resource
run Gen {
	-- let's have the sender and receiver be different
	all msg : Msg | msg.from not in msg.to
	-- let's have the message have a non-empty payload
	all msg : Msg | #msg.args > 0
} for 3
