/**
  *  flow.als
  *    A model of how differet data elements flow from one component to another
  *    as an argument or return type in calls
  */
module flow

open script

sig Data in Resource + Cookie {}

sig FlowCall in Call {
  args, returns: set Data,  -- arguments and return data of this call
}{
  this in HttpRequest implies
    args = this.sentCookies + this.body and
    returns = this.receivedCookies + this.response

  this in ReadDom implies no args and returns = this.result
  this in WriteDom implies args = this.newDom and no returns
  this in SetDomain implies no args + returns
}

sig FlowModule in Endpoint {
  -- set of data that this component iniitally owns
  owns: set Data
}

// Returns the data elements the given component c can access
fun accesses[ep: Endpoint] : set Data {
  -- "ep" can only access a data "d" iff
  -- (1) it owns "d" or
  -- (2) if "ep" receives a message that carries "d" or
  -- (3) if "ep" sends a message that returns "d"
  ep.owns + (to.ep).args + (from.ep).returns
}

// A payload in a message must be owned by the sender or received by the sender
// as part of a previous message
fact FlowConstraint {
  all ep: Endpoint, c: from.ep |
    c.args in ep.owns + (c.prevs & to.ep).args + (c.prevs & from.ep).returns
}

run {
  some Client.accesses
} for 3
