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

fact CORSRules {
	-- A CORS response is accepted iff it is allowed by the server, as indicated
	-- in "access-control-allow-origin" header
	all r : RespCORS | r.inResponseTo.origin in r.allowedOrigins
}
