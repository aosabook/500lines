/**
  *  call.als
  *    A generic model of calls
  */
module call[T]

open util/ordering[Time] as ord

sig Time {}
abstract sig Call {
  before, after: Time,
  from, to: T
}

// Returns the set of the calls that occured prior to "c"
fun prevs[c: Call] : set Call {
  { c': Call | c'.before in c.before.prevs }
}

fact {
  -- some call occurs between consecutive time steps
  all t: Time - ord/last | some c: Call | c.before = t and c.after = t.next
  -- every call time takes exactly one time step
  all c: Call | c.after = (c.before).next
}

run {} for 3
