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
	// Each message is associated with a sender and a receiver
	sender, receiver : Module,
	// Each message may contain a number of resources
	payloads : set Resource
}

// Set of messages that m receives
fun receives[m : Module] : set Msg {
	m.~receiver	
}

// Set of messages that m sends
fun sends[m : Module] : set Msg {
	m.~sender
}

fun payloadSet[msgs : set Msg] : set Resource {
	msgs.payloads
}

fun accesses[m : Module] : set Resource {
	// "m" can only access a resource "r" iff it owns "r" or
	// "m" receives a message that carries "r"
	m.owns + payloadSet[m.receives]
}

fact ResourceConstraints {
	// A payload in a message must be
	all m : Module, msg : m.sends |
		msg.payloads in 
			// owned by the sender OR
			m.owns + 
			// received by the sender as part of a previous mesasge
			payloadSet[msg.prevs & m.receives]
}
