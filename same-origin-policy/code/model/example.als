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

/* Helper functions for visualization */
fun currentCall: Call -> Time {
  {c: Call, t: Time | c.start = t }
}
fun relevantModules: DataflowModule -> Time {
  {m: DataflowModule, t: Time | m in currentCall.t.(from + to) }
}

run {} for 3
