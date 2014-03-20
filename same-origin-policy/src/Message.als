/**
	* Message.als
	* 	A model of messaging between modules
	*/
module Message

open util/ordering[Msg] as MO


sig Resource {}
abstract sig Module {
	owns : set Resource
}
abstract sig Msg {
	-- each message is associated with a sender and a receiver
	sender, receiver : Module,
	-- each message may contain a number of resources
	payloads : set Resource
}

// Returns the messages that the given module m receives
fun receives[m : Module] : set Msg {
	m.~receiver
}

// Returns the messages that the given module m sends
fun sends[m : Module] : set Msg {
	m.~sender
}

// Returns the payloads of all the given msgs.
fun payloadSet[msgs : set Msg] : set Resource {
	msgs.payloads
}

// Returns the resources the given module m can access
fun accesses[m : Module] : set Resource {
	-- "m" can only access a resource "r" iff it owns "r" or if "m" receives a
	-- message that carries "r"
	m.owns + payloadSet[m.receives]
}

// A payload in a message must be owned by the sender or received by the sender
// as part of a previous mesasge			
fact ResourceConstraints {
	all m : Module, msg : m.sends |
		msg.payloads in m.owns + payloadSet[msg.prevs & m.receives]
}
