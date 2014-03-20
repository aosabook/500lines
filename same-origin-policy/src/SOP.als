/**
	* SOP.als
	* 	A model of the same-origin policy
	*/
module SOP

--  import the HTTP model
open HTTP


// An origin is defined as a triple (protocol, host, port) where port is optional
sig Origin {
	host : Host,
	protocol : Protocol,
	-- lone means zero or one. It may help to think of the word as short for
	-- "less than or equal to one"
	port : lone Port
}

sig XMLHTTPReq in HTTPReq {
}{
	sender in Script
}

// True iff the two URLs match in terms of host, protocol, and port
pred sameOrigin[u1, u2 : URL] {
	u1.host = u2.host
	u1.protocol = u2.protocol
	u1.port = u2.port
}

fact SameOriginPolicy {
	-- A script can only access the DOM of a frame with the same origin
	all d : DomAPI | sameOrigin[d.frame.location, d.sender.context]
	-- A script can only make an AJAX call to a server with the same origin
	all x : XMLHTTPReq | sameOrigin[x.url, x.sender.context]
}
