/**
  *  example.als
  *    A model of an Email application
  */
module example

open analysis

/**
	* Components
	* Instantiations of servers, browser, and script
	*/
// Email application
one sig EmailServer extends Server {}
// Document loaded from the email server
one sig InboxPage extends Document {}{
  domain.init = EmailDomain
  content.init = MyInboxInfo 
}
one sig InboxScript extends Script {} { context = InboxPage }

// Calender application
one sig CalendarServer extends Server {}
one sig CalendarPage extends Document {}{
  domain.init = CalendarDomain
  content.init = MySchedule
}
// Trusted scripts
one sig CalendarScript extends Script {}{ context = CalendarPage }

// Blog application
one sig BlogServer extends Server {}
one sig BlogPage extends Document {}{ domain.init = BlogDomain }

// Concrete instances of domain names
// Conceptually, "email.example.com", "calendar.example.com", "blog.example.com" 
one sig EmailDomain, CalendarDomain, BlogDomain extends Domain {}
one sig ExampleDomain extends Domain {} { 
	// "example.com", which subsumes other subdomains (including itself)
	subsumes = EmailDomain + CalendarDomain + BlogDomain + this 
}

// A malicious server providing an ad service
one sig EvilServer extends Server {}
// Document loaded from the evil ad server
one sig AdBanner extends Document {}{
  domain.init = EvilDomain
  no CriticalData & content.init 
}
// An evil script running inside the ad 
one sig EvilScript extends Script {}
// ""www.evil.com"
one sig EvilDomain extends Domain {}

// The victim user's browser
one sig MyBrowser extends Browser {}
// Resource representing the information in the user's inbox (list of emails, etc)
one sig MyInboxInfo in Resource {}
// Resource representing my schedule on the calendar
one sig MySchedule in Resource {}
// Cookie used to authenticate the user by the email and calendar apps
one sig MyCookie in Cookie {}{ domains = EmailDomain + CalendarDomain }

one sig HTTP extends Protocol {}

/**
	* Requests
	*/
// Request for getting the inbox of the user
sig GetInboxInfo in HttpRequest {}{
  to in EmailServer
  -- cookie must be provided for the request to succeed
  MyCookie in sentCookies
}

// Get the user's schedule 
sig GetSchedule in HttpRequest {}{
  to in CalendarServer
  MyCookie in sentCookies
}

fact SecurityAssumptions {
	-- designate trusted modules
  	EmailServer + MyBrowser + CalendarServer + 
		InboxScript + CalendarScript + BlogServer in TrustedModule
	EvilScript + EvilServer in MaliciousModule
	CriticalData = MyInboxInfo + MyCookie + MySchedule 
    -- assume malicious modules initially don't have access to critical data
	no initData[MaliciousModule] & CriticalData
	-- and trusted modules don't already contain malicious data
	no initData[TrustedModule] & MaliciousData
   -- to disallow EmailDomain or CalenderDomain being mapped to EvilServer, 
   -- in reality, this attack is possible, but will be ruled out here
	Dns.map = EmailDomain -> EmailServer + CalendarDomain -> CalendarServer 
					+ BlogDomain -> BlogServer + EvilDomain -> EvilServer 
	MyInboxInfo != MySchedule
}

run {} for 3

/* Restrictions */

-- you can comment out any of these facts if you prefer

// Restrict calls to be only of one kind
-- leave uncommented the kind of call you want to see only
pred asm1 {
	#Call = 2
	one XmlHttpRequest
	one ReadDom
	no CorsRequest
	MyInboxInfo in EvilServer.accesses.last
	EvilScript.context = AdBanner
}
run asm1 for 3 but 2 Call

pred asm2 {
	Call in XmlHttpRequest
	Call.from = EvilScript
	Call.to = EmailServer
	GetInboxInfo = Call
	some sentCookies
	one Cookie
	MyInboxInfo in EvilScript.accesses.last
	EvilScript.context = AdBanner
}
run asm2 for 2 but 1 Call

one sig Leak extends Callback {}
pred jsonpAttack {
	one GetSchedule
	one ExecCallback
	ExecCallback.to in EvilScript
	MySchedule in EvilScript.accesses.last
   	GetSchedule in JsonpRequest
	one Cookie
	no MyInboxInfo & MySchedule
	EvilScript.context = AdBanner
}
run jsonpAttack for 3 but 2 Call, 3 Resource

pred postmessageAttack {
	one PostMessage
	one ReceiveMessage
	PostMessage.from in EvilScript
	ReceiveMessage.to in InboxScript
	some MaliciousData & InboxScript.accesses.last 
	no MyInboxInfo & MySchedule
	no Port
	EvilScript.context = AdBanner
}
run postmessageAttack for 3 but 3 Time, 2 Call//, 2 Resource

pred corsAttack {
	GetSchedule in CorsRequest
	GetSchedule.from = EvilScript
	MySchedule in EvilScript.accesses.last
	no MyInboxInfo & MySchedule
	no Port
	no body
	one resources
	EvilScript.context = AdBanner
}
run corsAttack for 3 but 2 Time, 1 Call

pred CRASH {
	subsumes = 
		ExampleDomain -> EmailDomain + ExampleDomain -> CalendarDomain  +
		ExampleDomain -> BlogDomain + (Domain <: iden)
	
	some o : SetDomain | o.start = init and o.from = InboxScript and o.newDomain = ExampleDomain
	some o : SetDomain | o.start = init.next and o.from = CalendarScript and o.newDomain = ExampleDomain
	some o : ReadDom {
//		o.from = CalendarScript 
		o.doc = InboxPage
//		o.from = CalendarPage
	}
}
run CRASH for 7 but 5 Domain, 4 Time, 3 Call

pred setdomainNormal {
	subsumes = 
		ExampleDomain -> EmailDomain + ExampleDomain -> CalendarDomain  +
		ExampleDomain -> BlogDomain + (Domain <: iden)
	
	some o : SetDomain | o.start = init and o.from = InboxScript and o.newDomain = ExampleDomain
	some o : SetDomain | o.start = init.next and o.from = CalendarScript and o.newDomain = ExampleDomain
	some o : ReadDom {
		o.from = CalendarScript 
//		o.doc = InboxPage
//		o.from = CalendarPage
	}
}
run setdomainNormal for 7 but 5 Domain, 4 Time, 3 Call

pred setdomainAttack {
	subsumes = 
		ExampleDomain -> EmailDomain + ExampleDomain -> CalendarDomain + 
		ExampleDomain -> BlogDomain + (Domain <: iden)
	EvilScript.context = BlogPage
	
	some o : SetDomain | o.start = init and o.from = InboxScript and o.newDomain = ExampleDomain
	some o : SetDomain | o.start = init.next and o.from = CalendarScript and o.newDomain = ExampleDomain
	some o : ReadDom {
		o.start = init.next.next
		o.from = CalendarScript 
		o.doc = InboxPage
	}
	some o : SetDomain | o.start = init.next.next.next and o.from = EvilScript and o.newDomain = ExampleDomain
	some o : ReadDom {
		o.start = init.next.next.next.next
		o.from = EvilScript
		o.doc = InboxPage
	}
}
run setdomainAttack for 7 but 5 Domain, 6 Time, 5 Call

/* Helper functions for visualization */

fun currentCall: Call -> Time {
  {c: Call, t: Time | c.start = t }
}

fun relevantModules : DataflowModule -> Time {
  {m: DataflowModule, t: Time | m in currentCall.t.(from + to) }
}
