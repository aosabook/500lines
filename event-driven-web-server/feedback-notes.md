- Pretty heavy on LISP
- starting out strong with a good roadmap, lose it around the time you start in on the event loop
- lots of detours into other problems
	- Get the CLOS "detour" out of the way first, maybe?
	- If that works, do the same for the macroexpansion sections (pull "Understanding The Expanders" up above the rest of that section). Dunno. That doesn't feel like the right approach. We should really start out with "here's what we want to say. And now, here's how we arrange what we want to say so that it can transparently make sense to the computer."

- papers cited about event-driven vs threaded model
- maybe establish a thread/event duality early on and contrast async-models (MUST. NOT. BLOCK.) against the thread approach (MUST. SHARE. STATE.)

- warm them up to `defmacro` and `defmethod` (that is, defmacro and multiple dispatch)
- warm them up to "event driven web _framework_" (the idea of switching out handlers using define handlers later)
- warm them up to asynchronous messages

- good direction in terms of difficulty ramping

### Questions
- How do I make reference to someone elses' chapter? (Just write the authors' name)
- timelines? (Next week-ish)
- diagrams? (SVG and a link tag)
    - IDEA: comparative diagrams of event-driven vs. threaded servers (may not actually show much difference)
