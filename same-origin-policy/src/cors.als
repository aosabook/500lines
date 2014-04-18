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
	origin : http/Origin,
	-- "access-control-allow-origin" header
	allowedOrigins : set http/Origin
}{
	from in browser/Script
}

pred corsRule {
	-- "origin" header of every CORS req matches the script context 
	all r : ReqCORS | 
		r.origin = url2origin[r.from.context] and
		-- A CORS response is accepted iff it is allowed by the server, as indicated
		-- in "access-control-allow-origin" header
		r.origin in r.allowedOrigins
}
