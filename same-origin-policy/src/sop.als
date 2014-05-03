/**
	* sop.als
	* 	A model of the same-origin policy
	*/
module sop

open http
open browser
open script
open cors


// True iff the two URLs match in terms of host, protocol, and port
pred sameOrigin[u1, u2 : URL] {
  u1.host = u2.host and u1.protocol = u2.protocol and u1.port = u2.port
}

pred sameOriginPolicy {
	-- same origin policy actually has multiple parts
  domSOP
  xmlhttpreqSOP
}

pred domSOP {
	-- A script can only access the DOM of a document with the same origin or
	-- the executing context and the target document have the same domain property
	all c : ReadDOM +WriteDOM | 
		sameOrigin[c.doc.src, c.from.context.src] or
		c.doc.domain = c.from.context.domain
}
pred xmlhttpreqSOP {
	-- A script can only make an AJAX call to a server with the same origin if
	-- it's not a CORS request.
	all x : XMLHttpRequest |
			sameOrigin[x.url, x.from.context.src] or x in CORSRequest
}
