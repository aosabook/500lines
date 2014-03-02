/**
	* CORS.als
	* 	A model of the cross-origin resource sharing (CORS) mechanism
	*/

module CORS

open SOP

sig ReqCORS in HTTPReq {
	-- "origin" header
	origin : Origin
}{
	sender in Script
}

sig RespCORS in HTTPResp {
	-- "access-control-allow-origin" header
	allowedOrigins : set Origin
}{
	receiver in Script
	inResponseTo in ReqCORS
}

// True iff the URL and origin match in terms of host, protocol, and port
pred sameOrigin[u : URL, o : Origin] {
	u.host = o.host
	u.protocol = o.protocol
	u.port = o.port
}

fact CORSRules {
	// A CORS response is accepted iff it is allowed by the server, 
	// as indicated in "access-control-allow-origin" header
	all r : RespCORS | r.inResponseTo.origin in r.allowedOrigins
}
