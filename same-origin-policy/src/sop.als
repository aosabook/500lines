/**
	* sop.als
	* 	A model of the same-origin policy
	*/
module sop

open browser
open http


// An origin is defined as a triple (protocol, host, port) where port is optional
sig Origin {
	host : http/Host,
	protocol : http/Protocol,
	-- lone means zero or one. It may help to think of the word as short for
	-- "less than or equal to one"
	port : lone http/Port
}

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
	all x : browser/XMLHTTPReq | sameOrigin[x.url, x.sender.context]
}
