module stepper[Event]

open util/ordering[Step] as SO

sig Step {
	evt : Event
}
fact {
	evt in Step one -> one Event
}

fun first : Event {
	SO/first.evt
}

fun last : Event {
	SO/last.evt
}

fun step[e : Event] : Step {
	evt.e
}

fun prevs[e : Event] : set Event {
	step[e].prevs.evt
}

fun next[e : Event] : set Event {
	step[e].next.evt
}

run {} for 3

