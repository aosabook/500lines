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
open http
open message
open sop
open cors
open postmessage

// Security policies 
// Comment out to see what might happen when one or more of them don't hold
fact Policies {
	domSOP
	xmlhttpreqSOP
	corsRule
	postMessageRule
}

one sig Facebook extends Server {}
one sig MyProfilePage extends Frame {
}{
	location in Facebook.urls
	dom = MyProfile
}
one sig MyProfile extends DOM {}

one sig EvilServer extends Server {}
one sig AdPage extends Frame {}{
	location in EvilServer.urls
	script = EvilScript
}
one sig EvilScript extends Script {}

one sig MyBrowser extends Browser {}{
	frames = MyProfilePage + AdPage
}

fact Assumptions {
	owns.MyProfile in MyBrowser
}

/* Simulation */

// Generates an instance with at least one successful same-origin request
// bound: up to 3 objects of each type, but exactly one server, browser, url,
// dom and origin.
run GenWithSameOriginReq {
	some req :  browser/XMLHTTPReq | sop/sameOrigin[req.url, req.from.context]
} for 4

// Generates an instance with at least one successful same-origin request that
// is not a CORS one
// bound: up to 3 objects of each type, but exactly one server, browser, url,
// dom and origin.
run GenWithSameOriginReqNoCors {
	some req :  browser/XMLHTTPReq | sop/sameOrigin[req.url, req.from.context]
    no cors/ReqCORS
} for 3 

// Generates an instance with at least one successful request with different origin.
// bound: up to 3 objects of each type, but exactly one server, browser, dom,
// and exactly two urls and origins.
run GenWithDifferentOriginReqCors {
	some req :  browser/XMLHTTPReq |
		not sop/sameOrigin[req.url, req.from.context]
} for 3

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
check noResourceLeak for 3 but 2 http/Server

/** for visualization only **/

fact {
	all m1 : Msg - last | 
		let m2 = m1.next | 
			m1 in HTTPReq implies m1 = m2.inResponseTo
}

fun pastEvents : Msg -> Step {
	{m : Msg, s : Step |
		m in (s.prevs + s).evt
		}
}

fun follows : Msg -> Msg {
	{m2, m1 : Msg |
		m2 = browser/message/ord/next[m1]
	}
}

fun sendsTo : EndPoint -> EndPoint -> Step {
	{e1, e2 : EndPoint, s : Step |
		s.evt.from = e1 and s.evt.to = e2
	}
}

fun server : Frame -> Server {
	{e : Frame, s : Server |
		e.location in s.urls
	}	
}

fun from : EndPoint -> Step {
	{e : EndPoint, s : Step |
		e = s.evt.from
	}	
}

fun to : EndPoint -> Step {
	{e : EndPoint, s : Step |
		e = s.evt.to
	}	
}

fun from : Msg -> EndPoint -> Step {
	{m : Msg, e : EndPoint, s : Step |
		m = s.evt and e = m.from
	}	
}

