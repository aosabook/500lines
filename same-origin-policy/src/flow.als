/**
	* flow.als
	*   A model of how differet data elements flow from one component to another
	*   as an argument or return type in calls
	*/
module flow

open script

sig Data in Resource + Cookie {}

sig Call in HttpRequest + BrowserOp {
  args, returns : set Data,	-- arguments and return data of this call
  sender, receiver : Component		-- sender, receiver of the call
}{
  this in HttpRequest implies {
    sender = client and receiver = server
    args = req_cookies + req_body
    returns = ret_set_cookies + ret_body
  }

  this in BrowserOp implies sender = from and receiver = to
  this in ReadDOM implies no args and returns = this.result
  this in WriteDOM implies args = this.new_dom and no returns
  this in ModifyDomainProperty implies no args + returns 
}

sig Component in Client + Server + Browser + Script {
	-- set of data that this component iniitally owns
	owns : set Data
}

// Returns the data elements the given component c can access
fun accesses[m : Component] : set Data {
  -- "c" can only access a data "d" iff 
  -- (1) it owns "d" or 
  -- (2) if "c" receives a message that carries "d" or
  -- (3) if "c" sends a message that returns "d" 
  m.owns + (receiver.m).args + (sender.m).returns
}

// A payload in a message must be owned by the sender or received by the sender
// as part of a previous message			
fact FlowConstraint {
  all m : Component, c : sender.m |
    c.args in m.owns + (c.prevs & receiver.m).args + (c.prevs & sender.m).returns
}

run {
	some Client.accesses
} for 3
