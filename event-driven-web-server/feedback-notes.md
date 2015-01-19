- Pretty heavy on LISP
- starting out strong with a good roadmap, lose it around the time you start in on the event loop
- lots of detours into other problems

# what's a map?

-papers cited about event-driven vs threaded model
-you can skip the middle option (the hybrid)
-maybe establish a thread/event duality early on and contrast async-models (MUST. NOT. BLOCK.) against the thread approach (MUST. SHARE. STATE.)

-do I need to mention the accidental complexity in the `buffer!` thing?

- warm them up to `defmacro` and `defmethod` (multiple dispatch and defmacro)
- warm them up to "event driven web _framework_" (the idea of switching out handlers using define handlers later)
- warm them up to asynchronous messages

- clarify that we're using SSEs (and be crystal clear about handler inputs/outputs in terms of the requests moving through)

- good direction in terms of difficulty ramping

### Questions
- How do I make reference to someone elses' chapter? (Just write the authors' name)
- timelines? (Next week-ish)
- diagrams? (SVG and a link tag)
    - IDEA: comparative diagrams of event-driven vs. threaded servers (may not actually show much difference)
