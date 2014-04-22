/**
	* sop.als
	* 	A model of the same-origin policy
	*/
module sop

open browser
open http
open cors


// True iff the two URLs match in terms of host, protocol, and port
pred sameOrigin [u1, u2 : http/Url] {
	u1.host = u2.host // and u1.protocol = u2.protocol and u1.port = u2.port
}

pred sameOriginPolicy {
	-- same origin policy actually has multiple parts
	domSop
	xmlHttpReqSop
}

pred domSop {
	-- A script can only access the DOM of a frame with the same origin
	all d : browser/ReadDOM + browser/WriteDOM | sameOrigin[d.frame.location, d.from.context]
}
pred xmlHttpReqSop {
	-- A script can only make an AJAX call to a server with the same origin if
	-- it's not a CORS request.
	all x : browser/XMLHTTPReq | sameOrigin[x.url, x.from.context] or x in cors/ReqCORS
}

