/**
  *  sop.als
  *    A model of the same-origin policy
  */
module sop

open http
open browser
open script
open setDomain
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
    -- (relaxation) the domain property of both the script's context and the
    -- target document has been set and
    (c.doc + c.from.context in (c.prevs <: SetDomain).doc and
    -- they have the same origin (using the domain property as the host and not
    -- the src host)
    origin[c.doc.src, c.doc.domain.(c.start)] =
    origin[c.from.context.src, c.from.context.domain.(c.start)])
}

pred xmlHttpReqSop {
  all x: XmlHttpRequest |
    -- A script can only make an AJAX call to a server with the same origin or
    origin[x.url] = origin[x.from.context.src] or
    -- (relaxation) it's a CORS request
    x in CorsRequest
}


/* Commands */

// Can a script read or write the DOM of a document with another origin?
check { no c: ReadDom + WriteDom | origin[c.doc.src] != origin[c.from.context.src] }
