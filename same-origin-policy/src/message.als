/**
	* message.als
	* 	A model of messaging between endpoints
	*/
module message

open util/ordering[Msg] as ord


sig Resource {}
abstract sig EndPoint {
	owns : set Resource
}
abstract sig Msg {
	-- each message is associated with a sender and a receiver
	sender, receiver : EndPoint,
	-- each message may contain a number of resources
	payloads : set Resource
}

// Returns the messages that the given endpoint ep receives
fun receives[ep : EndPoint] : set Msg {
	ep.~receiver
}

// Returns the messages that the given endpoint ep sends
fun sends[ep : EndPoint] : set Msg {
	ep.~sender
}

// Returns the payloads of all the given msgs
fun payloadSet[msgs : set Msg] : set Resource {
	msgs.payloads
}

// Returns the resources the given endpoint ep can access
fun accesses[ep : EndPoint] : set Resource {
	-- "ep" can only access a resource "r" iff it owns "r" or if "ep" receives a
	-- message that carries "r"
	ep.owns + payloadSet[receives[ep]]
}

// A payload in a message must be owned by the sender or received by the sender
// as part of a previous message			
fact ResourceConstraints {
	all ep : EndPoint, msg : sends[ep] |
		msg.payloads in ep.owns + payloadSet[ord/prevs[msg] & receives[ep]]
}
