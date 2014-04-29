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
  cookieSop
}

pred domSOP {
	-- A script can only access the DOM of a frame with the same origin
	all c : ReadDOM +WriteDOM | sameOrigin[c.target_document.src, c.from.context]
  -- TODO: or they have the same document.domain or they are using post message
}
pred xmlhttpreqSOP {
	-- A script can only make an AJAX call to a server with the same origin if
	-- it's not a CORS request.
	all x : XMLHttpRequest |
			sameOrigin[x.url, x.from.context] or x in cors/CORSRequest
}

pred cookieSop {
  -- TODO: cookie sop
}

run {} for 3
