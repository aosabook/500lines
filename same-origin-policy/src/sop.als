/**
  *  sop.als
  *    A model of the same-origin policy
  */
module sop

open http
open browser
open script
open cors


// True iff the two URLs match in terms of host, protocol, and port
pred sameOrigin[u1, u2: Url] {
  u1.host = u2.host and u1.protocol = u2.protocol and u1.port = u2.port
}

pred sameOriginPolicy {
	-- same origin policy actually has multiple parts
  domSop
  xmlHttpReqSop
}

pred domSop {
	all c: ReadDom+ WriteDom | 
		-- A script can only access the DOM of a document with the same origin or
		sameOrigin[c.doc.src, c.from.context.src] or
		-- (relaxation) script's context and the target document have the same domain property
		c.doc.domain = c.from.context.domain
}

pred xmlHttpReqSop {
  -- A script can only make an AJAX call to a server with the same origin if
  -- it's not a CORS request (relaxation).
  all x: XmlHttpRequest |
    sameOrigin[x.url, x.from.context.src] or x in CorsRequest
}
