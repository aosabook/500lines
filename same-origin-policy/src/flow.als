module flow

open http

sig Data in Resource {
	flows : Component -> Component
}

sig Call in HttpRequest {
	args, returns : set Data,
	from, to : Component
}{
	
	this in HttpRequest implies {
  		from = client
		to = server
		  -- cookies are always part of the header, never the body
		 	 args = cookies + req_body
  		 	returns = ret_set_cookies + ret_body
		}
}

sig Component in Client + Server {
	owns : set Data
}

// Returns the data elements the given component c can access
fun accesses[m : Component] : set Data {
	-- "c" can only access a data "d" iff 
	-- (1) it owns "d" or 
	-- (2) if "c" receives a message that carries "d" or
	-- (3) if "c" sends a message that returns "d" 
	m.owns + (to.m).args + (from.m).returns
}

// A payload in a message must be owned by the sender or received by the sender
// as part of a previous message			
fact FlowConstraints {
	all m : Component, c : from.m |
		c.args in m.owns + (c.prevs & to.m).args + (c.prevs & from.m).returns
}

run {
	some Client.accesses
	some flows
} for 3
