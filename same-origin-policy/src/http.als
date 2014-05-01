/**
   *  http.als
   *    A model of the Hypertext Transfer Protocol.
   */
module http

open call


sig Protocol, Domain, Port, Path {}

sig URL {
  protocol : Protocol,
  host : Domain,
  port : lone Port,
  path : Path
}

/* HTTP Requests */

abstract sig Method {}
one sig Get, Post extends Method {}

abstract sig HttpRequest extends Call {
  -- request
  url : URL,
  method : Method,
  cookies : set Cookie,
  body : lone Resource,
  -- response
  set_cookies : set Cookie,
  resp_body : lone Resource,
}{
  from in Client
  to in DNS.map[url.host]
  all c : set_cookies | url.host in c.domains
  resp_body in to.resources[url.path]
}

/* HTTP Components */
abstract sig Client extends Module {}
abstract sig Server extends Module { resources : Path -> Resource }
abstract sig Resource {}
abstract sig Cookie {
  -- by default all cookies are scoped to the host. The cookie domain and path
  -- field could be used to broaden (thus adding more hosts) or limit the scope
  -- of the cookie.
  domains : set Domain,
}

/* Domain Name Server */
one sig DNS { 
	map : Domain -> Server 
}{
	all s : Server | some d : Domain | d.map = s
}


/* Run commands */

// A simple request
run {}

// Let's force responses to set cookies
run { all r : HttpRequest | some r.set_cookies }

// Can we get a request for a path that's not mapped by the server?
check { all r : HttpRequest | r.url.path in r.to.resources.Resource }

// Can we get the same domain mapping to multiple servers?
check { all d : Domain | no disj s1, s2 : Server | s1 + s2 in DNS.map[d] }
