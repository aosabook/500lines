/**
	* cors.als
	* 	A model of the cross-origin resource sharing (CORS) mechanism
	* 		intended for cross-domain communication from a script and a server
	*/
module cors

open browser
open http
open script
open origin

sig CORSRequest in http/HttpRequest {
  -- "origin" header
  origin : Origin,
  -- "access-control-allow-origin" header
  ret_allowedOrigins : set Origin
}{
  from in Script
}

-- in some cases requests are pre-flighted, but we leave this out of the model

pred corsRule {
  -- "origin" header of every CORS req matches the script context 
  all r : CORSRequest | 
    r.origin = url2origin[r.from.context] and
    -- A CORS response is accepted iff it is allowed by the server, as
   -- indicated in "access-control-allow-origin" header
    r.origin in r.ret_allowedOrigins
}

run {}
