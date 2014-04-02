/**
	* postMessage
	* 	A model of the HTML5 PostMessage mechanism
	* 		intended for cross-domain communication between scripts
	*/
module postMessage

open http
open sop

sig PostMessage extends http/DomAPI {
	data : Resource,
	origin : Origin
}{
	sender + receiver in http/Script
	payloads = data
}

run {} for 3
