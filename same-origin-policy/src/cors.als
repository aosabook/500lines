/**
	* cors.als
	* 	A model of the cross-origin resource sharing (CORS) mechanism
	* 		intended for cross-domain communication from a script and a server
	*/
module cors

open browser
open http

sig ReqCORS in http/HTTPReq {
	-- "origin" header
	origin : http/Origin
}{
	from in browser/Script
}

sig RespCORS in http/HTTPResp {
	-- "access-control-allow-origin" header
	allowedOrigins : set http/Origin
}{
	to in browser/Script
	inResponseTo in ReqCORS
}

pred corsRule {
	-- "origin" header of every CORS req matches the script context 
	all r : ReqCORS | r.origin = url2origin[r.from.context]
	-- A CORS response is accepted iff it is allowed by the server, as indicated
	-- in "access-control-allow-origin" header
	all r : RespCORS | r.inResponseTo.origin in r.allowedOrigins
	all r : HTTPResp | r.inResponseTo in ReqCORS implies r in RespCORS
}
