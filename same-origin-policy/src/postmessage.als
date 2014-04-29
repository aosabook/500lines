/**
	* postMessage
	* 	A model of the HTML5 PostMessage mechanism
	* 		intended for cross-domain communication between scripts
	*/
module postMessage

open script
open sop

// Browser API function for cross-document messaging
// used to send a message from one script to another
sig PostMessage extends DOMAPICall {
  message : Resource,
  srcOrigin, targetOrigin : URL
}{
  from + to in Script
  args = message
}

pred postMessageRule {
  -- the receiving frame of a PostMessage must belong to the same origin as targetOrigin
  all m : PostMessage | sop/sameOrigin[m.targetOrigin, m.to.context.src]
}

run {} for 3

