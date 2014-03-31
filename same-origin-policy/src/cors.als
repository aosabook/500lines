/**
	* cors.als
	* 	A model of the cross-origin resource sharing (CORS) mechanism
	*/
module cors

open http
open sop


sig ReqCORS in http/HTTPReq {
	-- "origin" header
	origin : sop/Origin
}{
	sender in Script
}

sig RespCORS in http/HTTPResp {
	-- "access-control-allow-origin" header
	allowedOrigins : set sop/Origin
}{
	receiver in http/Script
	inResponseTo in ReqCORS
}

// True iff the URL and origin match in terms of host, protocol, and port
pred sameOrigin[u : http/URL, o : sop/Origin] {
	u.host = o.host
	u.protocol = o.protocol
	u.port = o.port
}

fact CORSRules {
	-- A CORS response is accepted iff it is allowed by the server, as indicated
	-- in "access-control-allow-origin" header
	all r : RespCORS | r.inResponseTo.origin in r.allowedOrigins
}
