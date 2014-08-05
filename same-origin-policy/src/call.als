/**
  *  call.als
  *    A generic model of calls
  */
module call[T]

open util/ordering[Time] as ord

sig Time {}
abstract sig Call {
  start, end: Time,
  from, to: T
}

// Returns the set of calls that occured prior to "c"
fun prevs[c: Call] : set Call {
  { c': Call | c'.start in c.start.prevs }
}

fact {
  -- some call occurs between consecutive time steps
  all t: Time - ord/last | some c: Call | c.start = t and c.end = t.next
  -- every call takes exactly one time step
  all c: Call | c.end = (c.start).next
}

run {} for 3
