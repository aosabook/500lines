/**
	* main.als
	* 	The "main" model for a client-server system with the same origin policy
	*
	* Authors: 
	*	Eunsuk Kang (eskang@mit.edu)
	* 	Santiago Perez De Rosso (sperezde@csail.mit.edu)
	* 	Daniel Jackson (dnj@mit.edu)
	*/
module main

-- import other model files
open browser
open cors
open http
open message
open sop


/* Simulation */

// Generates an instance with at least one successful same-origin request
// bound: up to 3 objects of each type, but exactly one server, browser, url,
// dom and origin.
run GenWithSameOriginReq {
	some req :  browser/XMLHTTPReq | sop/sameOrigin[req.url, req.sender.context]
} for 3 but 1 http/Server, 1 browser/Browser, 1 http/URL, 1 browser/DOM,
1 sop/Origin


// Generates an instance with at least one successful same-origin request that
// is not a CORS one
// bound: up to 3 objects of each type, but exactly one server, browser, url,
// dom and origin.
run GenWithSameOriginReqNoCors {
	some req :  browser/XMLHTTPReq | sop/sameOrigin[req.url, req.sender.context]
    no cors/ReqCORS
} for 3 but 1 http/Server, 1 browser/Browser, 1 http/URL, 1 browser/DOM,
1 sop/Origin


// Generates an instance with at least one successful request with different origin.
// bound: up to 3 objects of each type, but only up to 2 servers and 1 browser
// bound: up to 3 objects of each type, but exactly one server, browser, dom,
// and exactly two urls and origins.
run GenWithDifferentOriginReqCors {
	some req :  browser/XMLHTTPReq |
		not sop/sameOrigin[req.url, req.sender.context]
} for 3 but exactly 1 http/Server, exactly 1 browser/Browser,
exactly 2 http/URL, exactly 1 browser/DOM, exactly 2 sop/Origin


/* Property Checking */

-- Designate some subset of resources to be critical, and some of the endpoints
-- to be "malicious"
sig CriticalResource in message/Resource {}
sig MaliciousEndPoint in message/EndPoint {}

// Asserts that no bad endpoint can read a critical resource
assert noResourceLeak {
	no r : CriticalResource, b : MaliciousEndPoint | r in message/accesses[b]
}

// Check whether assertion "noResourceLeak" holds
// bound: up to 5 objects of each type, but only up to 2 servers
-- this generates a counterexample that can be visualized with theme file "SOP.thm"
check noResourceLeak for 5 but 2 http/Server
