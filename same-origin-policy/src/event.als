/**
	* event.als
	* 	A generic model of timed events
	*/
module event

open util/ordering[Time] as ord

sig Time {}
abstract sig Event { pre, post : Time }

// Returns the set of the events that occured prior to event e
fun prevs[e : Event] : set Event {
	{ e' : Event | e'.pre in e.pre.prevs }
}

fact {
	-- exactly one event occurs between consecutive time steps
	all t : Time - ord/last | 
		some e : Event | e.pre = t and e.post = t.next
	-- every time takes exactly one time step
	all e : Event | e.post = (e.pre).next
}
