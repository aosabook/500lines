/**
  *  cors.als
  *    A model of the cross-origin resource sharing (CORS) mechanism
  *    intended for cross-domain communication from a script and a server
  */
module cors

open browser
open http
open script
open origin

sig CorsRequest in XmlHttpRequest {
  -- "origin" header
  origin: Origin,
  -- "access-control-allow-origin" header
  allowedOrigins: set Origin
}{
  from in Script
}

fact corsRule {
  all r: CorsRequest |
    -- "origin" header of every CORS request matches the script context and
    r.origin = origin[r.from.context.src] and
    -- a CORS request is accepted iff it is allowed by the server, as indicated
    -- in "access-control-allow-origin" header
    r.origin in r.allowedOrigins
}

run {}
