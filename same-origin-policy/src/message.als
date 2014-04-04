/**
	* message.als
	* 	A model of messaging between endpoints
	*/
module message

//open util/ordering[Msg] as ord
open stepper[Msg] as ord

sig Resource {}
abstract sig EndPoint {
	owns : set Resource
}
abstract sig Msg {
	-- each message is associated with a sender and a receiver
	from, to : EndPoint,
	-- each message may contain a number of resources
	payload : set Resource
}

// Returns the messages that the given endpoint ep receives
fun receives[ep : EndPoint] : set Msg {
	ep.~to
}

// Returns the messages that the given endpoint ep sends
fun sends[ep : EndPoint] : set Msg {
	ep.~from
}

// Returns the payloads of all the given msgs
fun payloads[msgs : set Msg] : set Resource {
	msgs.payload
}

// Returns the resources the given endpoint ep can access
fun accesses[ep : EndPoint] : set Resource {
	-- "ep" can only access a resource "r" iff it owns "r" or if "ep" receives a
	-- message that carries "r"
	ep.owns + payloads[receives[ep]]
}

// A payload in a message must be owned by the sender or received by the sender
// as part of a previous message			
fact ResourceConstraints {
	all ep : EndPoint, msg : sends[ep] |
		msg.payload in ep.owns + payloads[ord/prevs[msg] & receives[ep]]
}

// Generates an instance with a non-empty message between two different end
// points.
// bound: exactly 2 EndPoints, 1 Msg and 1 Resource
run Gen {
	-- let's have the sender and receiver be different
	all msg : Msg | msg.from not in msg.to
	-- let's have the message have a non-empty payload
	all msg : Msg | #msg.payload > 0
} for exactly 2 EndPoint, exactly 1 Msg, exactly 1 Resource
