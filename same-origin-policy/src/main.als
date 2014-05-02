/**
   *  main.als
   *    The "main" model for a client-server system with the same origin policy
   *
   *  Authors: 
   *    Eunsuk Kang (eskang@mit.edu)
   *    Santiago Perez De Rosso (sperezde@csail.mit.edu)
   *    Daniel Jackson (dnj@mit.edu)
   */
module main

-- import other model files
open http
open browser
open script
open sop
open cors
open postmessage


// Security policies 
// Comment out to see what might happen when one or more of them don't hold
pred policies {
 	domSOP
	xmlhttpreqSOP
    -- TODO: cookieSop
	corsRule
	postMessageRule
}



/* An example web system with
		- two servers (Facebook and Evil Server)
		- the user's browser (MyBrowser)
		- a Facebook page containing the user profile (MyProfile)
		- an ad page from EvilServer with a malicious script (EvilScript)
 */

// User's browser
one sig MyFBCookie extends browser/Cookie {}
one sig MyBrowser extends browser/Browser {}{
//	cookies in Facebook.paths -> MyFBCookie
}

// Facebook server and its related parts
one sig FBHost in Host {}
one sig Facebook extends http/Server {}{
//	urls.host = FBHost
	all r : HttpRequest |
		(r.to = this and MyProfile in r.returns) implies
			MyFBCookie in r.args
}
one sig MyProfilePage extends Document {}{
	src.host = FBHost
	src.path in Facebook.paths
//	dom = MyProfile
}
sig Profile in Resource {}
one sig MyProfile in Profile {}

// Malicious server and its related parts
one sig EvilHost in Host {}
one sig EvilServer extends http/Server {}{
//	urls.host = EvilHost
}
one sig AdPage extends Document {}{
	src.host = EvilHost
	src.path in EvilServer.paths
//	dom in Ad
}
sig Ad in Resource {}
one sig EvilScript extends Script {}{
	context = AdPage
}

fact SystemAssumptions {
	no FBHost & EvilHost
	DNS.map = FBHost -> Facebook + EvilHost -> EvilServer
	MyProfile not in (EvilServer + EvilScript).owns
	MyFBCookie not in Server.owns
	all r : CORSRequest | r.to = Facebook implies r.allowed_origins.host = FBHost
	no r : HttpRequest |
		(r.from = Facebook and r.to = EvilServer and some CriticalData & r.args) or
		(r.from = MyBrowser and r.to = EvilServer and some CriticalData & r.args)
}

/* Checking a Security Property */

// Designate some subset of resources to be critical, and 
// some of the endpoints to be trusted
sig CriticalData in Resource {}
sig Trusted in Component {}

fact SecurityBoundary {
	Trusted = Facebook + MyBrowser
	CriticalData = MyProfile
}

// Asserts that no bad endpoint can read a critical resource
assert noResourceLeak {
	policies implies 
		all r : CriticalData, m : Component | 
			r in accesses[m] implies
				m in Trusted
}


// Check whether assertion "noResourceLeak" holds
// bound: up to 5 objects of each type
// This generates a counterexample that can be visualized with theme file "SOP.thm"
check noResourceLeak for 5 

run {
	some o : ReadDOM | 
		MyProfile in o.returns
} for 5

/** for visualization only **/
fun Untrusted : set Component {
	Component - Trusted
}
