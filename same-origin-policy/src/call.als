/**
	* call.als
	* 	A model of synchronous call events
	*/
module call

open event

abstract sig Data {}
abstract sig Component {
	owns : set Data
}
abstract sig Call extends Event {
	-- each call is invoked by one component one another
	from, to : Component,
	-- each call may carry a number of data elements as arguments or return values
	args, returns : set Data,
}

// Returns the messages that the given component c receives
fun receives[o : Component] : set Call {
	o.~to
}

// Returns the messages that the given component c sends
fun sends[o : Component] : set Call {
	o.~from
}

// Returns the data elements the given component c can access
fun accesses[o : Component] : set Data {
	-- "c" can only access a data "d" iff 
	-- (1) it owns "d" or 
	-- (2) if "c" receives a message that carries "d" or
	-- (3) if "c" sends a message that returns "d" 
	o.owns + receives[o].args + sends[o].returns
}

// A payload in a message must be owned by the sender or received by the sender
// as part of a previous message			
fact DataConstraints {
	all m : Component, c : sends[m] |
		c.args in m.owns + (c.prevs & receives[m]).args + (c.prevs & sends[m]).returns
}

// Generates an instance with a non-empty message between two different end
// points.
// bound: exactly 2 Components, 1 Call and 1 Data
run Gen {
	-- let's have the sender and receiver be different
	all c : Call | c.from not in c.to
	-- let's have the message have a non-empty payload
	all c : Call | #c.args > 0
} for 3
