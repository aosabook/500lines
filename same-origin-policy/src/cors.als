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

sig CorsRequest in http/HttpRequest {
  -- "origin" header
  origin: Origin,
  -- "access-control-allow-origin" header
  allowed_origins: set Origin
}{
  from in Script
}

-- in some cases requests are pre-flighted, but we leave this out of the model

pred corsRule {
  -- "origin" header of every CORS req matches the script context
  all r: CorsRequest |
    r.origin = url2origin[r.from.context.src] and
    -- A CORS response is accepted iff it is allowed by the server, as
   -- indicated in "access-control-allow-origin" header
    r.origin in r.allowed_origins
}

run {}
