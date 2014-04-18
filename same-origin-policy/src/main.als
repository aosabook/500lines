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
pred policies {
//	domSOP
	xmlhttpreqSOP
	corsRule
	postMessageRule
}

/* An example web system with
		- two servers (Facebook and Evil Server)
		- the user's browser (MyBrowser)
		- a Facebook page containing the user profile (MyProfile)
		- an ad page from EvilServer with a malicious script (EvilScript)
 */

// Facebook server and its related parts
one sig FBHost in Host {}
one sig Facebook extends http/Server {}{
	urls.host = FBHost
}
one sig MyProfilePage extends browser/Frame {}{
	location in Facebook.urls
	dom = MyProfile
}
sig Profile extends browser/DOM {}
one sig MyProfile in Profile {}

// Malicious server and its related parts
one sig EvilHost in Host {}
one sig EvilServer extends http/Server {}{
	urls.host = EvilHost
}
one sig AdPage extends browser/Frame {}{
	location in EvilServer.urls
	script = EvilScript
	dom in Ad
}
sig Ad extends browser/DOM {}
one sig EvilScript extends browser/Script {}

// User's browser
one sig MyBrowser extends browser/Browser {}{
	frames = MyProfilePage + AdPage
}

fact SystemAssumptions {
	FBHost != EvilHost
	MyProfile not in EvilServer.owns
	all r : RespCORS | r.from = Facebook implies r.allowedOrigins.host = FBHost 
	no r : HTTPReq + HTTPResp |
		r.from = Facebook and 
		r.to = EvilServer and
		some CriticalResource & r.payload
}

/* Checking a Security Property */

// Designate some subset of resources to be critical, and 
// some of the endpoints to be trusted
sig CriticalResource in message/Resource {}
sig Trusted in message/EndPoint {}

fact SecurityBoundary {
	Trusted = Facebook + MyBrowser
	CriticalResource = MyProfile
}

// Asserts that no bad endpoint can read a critical resource
assert noResourceLeak {
	policies implies 
		all r : CriticalResource, e : EndPoint | 
			r in message/accesses[e] implies
				e in Trusted
}

// Check whether assertion "noResourceLeak" holds
// bound: up to 5 objects of each type
// This generates a counterexample that can be visualized with theme file "SOP.thm"
check noResourceLeak for 5

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

fun Untrusted : set EndPoint {
	EndPoint - Trusted
}
