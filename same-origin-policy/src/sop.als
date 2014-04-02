/**
	* sop.als
	* 	A model of the same-origin policy
	*/
module sop

open browser
open http
open cors


// True iff the two URLs match in terms of host, protocol, and port
pred sameOrigin[u1, u2 : http/URL] {
	u1.host = u2.host
	u1.protocol = u2.protocol
	u1.port = u2.port
}

fact SameOriginPolicy {
	-- A script can only access the DOM of a frame with the same origin
	all d : browser/DomAPICall | sameOrigin[d.frame.location, d.sender.context]
	-- A script can only make an AJAX call to a server with the same origin
	all x : browser/XMLHTTPReq | x in cors/ReqCORS or sameOrigin[x.url, x.sender.context]
}
