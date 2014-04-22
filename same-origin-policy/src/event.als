/**
	* event.als
	* 	A generic model of timed events
	*/
module event

open util/ordering[Time]

sig Time {}
abstract sig Event {
	pre, post : Time 
}

// returns the set of the events that occured prior to e
fun prevs[e : Event] : set Event {
	{ e' : Event | e'.pre in e.pre.prevs }
}

fact {
	// Exactly one event occurs between consectuive time steps
	all t : Time - last | 
		one e : Event | e.pre = t and e.post = t.next
	// Every time takes exactly one time step
	all e : Event | e.post = (e.pre).next
}
