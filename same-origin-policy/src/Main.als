/**
	* Main.als
	* 	The "main" model for a client-server system with the same origin policy
	*
	* Authors: 
	*	Eunsuk Kang (eskang@mit.edu)
	* 	Santiago Perez De Rosso (sperezde@csail.mit.edu)
	* 	Daniel Jackson (dnj@mit.edu)
	*/
module Main

// import other model files
open HTTP
open SOP
open CORS


/* Simulation */
run GenRandom {
	// Generate a random scenario involving up to 3 objects of each type
	//  No need to specify any constraint
} for 3

run GenSameOriginReq {
	// Generate a scenario with at least one successful same-origin request
	some req :  XMLHTTPReq | sameOrigin[req.url, req.sender.context]
} for 3 but 2 Server, 1 Browser

run GenCORSReq {
	// Generate a scenario with at least one successful CORS request
	some RespCORS
} for 3 but 2 Server

/* Property checking */
// Designate some subset of resources to be critical, and
// some of the modules to be "malicious"
sig CriticalResource in Resource {}
sig MaliciousModule in Module {}

assert noResourceLeak {
	// No bad module should be able to read a critical resource
	no r : CriticalResource, b : MaliciousModule | r in b.accesses
}

// Check whether assertion "noResourceLeak" holds in all cases
// Generates a counterexample; can be visualized with theme file "SOP.thm"
check noResourceLeak for 5 but 2 Server
