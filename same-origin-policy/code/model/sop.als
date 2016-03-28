/**
  *  sop.als
  *    A model of the same-origin policy
  */
module sop
open browser
open cors
open origin
open setDomain

fact sameOriginPolicy {
  -- same origin policy actually has multiple parts
  domSop
  xmlHttpReqSop
}

pred domSop {
  -- For every successful read/write DOM operation,
  all o: ReadDom + WriteDom |  let target = o.doc, caller = o.from.context |
    -- the calling and target documents are from the same origin, or
    origin[target] = origin[caller] or
    -- domain properties of both documents have been modified
    (target + caller in (o.prevs <: SetDomain).doc and
      -- ...and they have matching origin values.
      currOrigin[target, o.start] = currOrigin[caller, o.start])
}

pred xmlHttpReqSop {
  all x: XmlHttpRequest |
    -- A script can only make an AJAX call to a server with the same origin or
    origin[x.url] = origin[x.from.context.src] or
    -- (relaxation) it's a CORS request
    x in CorsRequest
}

// Can a script read or write the DOM of a document with another origin?
run { some c: ReadDom + WriteDom | origin[c.doc.src] != origin[c.from.context.src] } for 4
