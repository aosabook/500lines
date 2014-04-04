/**
	* postMessage
	* 	A model of the HTML5 PostMessage mechanism
	* 		intended for cross-domain communication between scripts
	*/
module postMessage

open http
open browser
open sop

sig PostMessage extends browser/DomAPICall {
	message : Resource,
	origin, targetOrigin : URL
}{
	from + to in browser/Script
	payload = message
}

pred postMessageRule {
  all m : PostMessage | sop/sameOrigin[m.targetOrigin, m.to.context]
}

run {} for 3

