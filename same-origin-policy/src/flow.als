/**
	* flow.als
	*   A model of how differet data elements flow from one component to another
	*   as an argument or return type in calls
	*/
module flow

open script

sig Data in Resource + Cookie {}

sig FlowCall in Call {
  args, returns : set Data,	-- arguments and return data of this call
}{
  this in HttpRequest implies 
	args = this.cookies + this.body and
	returns = this.set_cookies + this.resp_body

  this in ReadDOM implies no args and returns = this.result
  this in WriteDOM implies args = this.new_dom and no returns
  this in SetDomain implies no args + returns 
}

sig FlowModule in Module {
	-- set of data that this component iniitally owns
	owns : set Data
}

// Returns the data elements the given component c can access
fun accesses[m : Module] : set Data {
  -- "m" can only access a data "d" iff 
  -- (1) it owns "d" or 
  -- (2) if "m" receives a message that carries "d" or
  -- (3) if "m" sends a message that returns "d" 
  m.owns + (to.m).args + (from.m).returns
}

// A payload in a message must be owned by the sender or received by the sender
// as part of a previous message			
fact FlowConstraint {
  all m : Module, c : from.m |
    c.args in m.owns + (c.prevs & to.m).args + (c.prevs & from.m).returns
}

run {
	some Client.accesses
} for 3
