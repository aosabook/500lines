/**
	* call.als
	* 	A model of synchronous call events
	*/
module call

open event

abstract sig Resource {}
abstract sig Component { owns : set Resource }
abstract sig Call extends Event {
  -- each call is invoked by one component one another
  from, to : Component,
  -- each call may carry a number of data elements as arguments or return values
  args, returns : set Resource,
}{
  -- no self call
  to != from
}	

// Returns the messages that the given component c receives
fun receives[c : Component] : set Call { c.~to }

// Returns the messages that the given component c sends
fun sends[c : Component] : set Call { c.~from }

// Returns the data elements the given component c can access
fun accesses[c : Component] : set Resource {
	-- "c" can only access a data "d" iff 
	-- (1) it owns "d" or 
	-- (2) if "c" receives a message that carries "d" or
	-- (3) if "c" sends a message that returns "d" 
	c.owns + receives[c].args + sends[c].returns
}

// A payload in a message must be owned by the sender or received by the sender
// as part of a previous message			
fact ResourceConstraints {
	all m : Component, c : sends[m] |
		c.args in m.owns + (c.prevs & receives[m]).args + (c.prevs & sends[m]).returns
}

// Generates an instance with a non-empty message between two different end
// points.
// bound: exactly 2 Components, 1 Call and 1 Resource
run Gen {
	-- let's have the sender and receiver be different
	all c : Call | c.from not in c.to
	-- let's have the message have a non-empty payload
	all c : Call | #c.args > 0
} for 3
