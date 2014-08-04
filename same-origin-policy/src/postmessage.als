/**
  *  postMessage
  *    A model of the HTML5 PostMessage mechanism intended for cross-domain
  *    communication between scripts
  */
module postMessage

open origin
open script
open sop

// Browser API function for cross-document messaging
// used to send a message from one script to another
sig PostMessage extends Call {
  message: Resource,
  targetOrigin: Origin
}{
  from + to in Script
  -- srcOrigin = origin[to.context.src] 
}

pred postMessageRule {
  -- the receiving frame of a PostMessage must belong to the same origin as
  -- targetOrigin (according to the spec messages that don't satisfy this rule
  -- are discarded)
  all m: PostMessage | origin[m.to.context.src] = m.targetOrigin
}

run { some PostMessage }
