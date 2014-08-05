/**
  *  sop.als
  *    A model of the same-origin policy
  */
module sop

open http
open browser
open script
open cors

pred sameOriginPolicy {
  -- same origin policy actually has multiple parts
  domSop
  xmlHttpReqSop
}

pred domSop {
  all c: ReadDom + WriteDom | 
    -- A script can only access the DOM of a document with the same origin or
    origin[c.doc.src] = origin[c.from.context.src] or
    -- (relaxation) script's context and the target document have the same
    -- domain property
    c.doc.domain = c.from.context.domain
}

pred xmlHttpReqSop {
  all x: XmlHttpRequest |
    -- A script can only make an AJAX call to a server with the same origin or
    origin[x.url] = origin[x.from.context.src] or
    -- (relaxation) it's a CORS request
    x in CorsRequest
}
