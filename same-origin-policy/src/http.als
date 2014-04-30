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
  -- port and path are optional
  port : lone Port,
  path : lone Path
}

/* HTTP Requests */

abstract sig Method {}
one sig Get, Post extends Method {}

abstract sig HttpRequest extends Call {
  -- request
  url : URL,
  method : Method,
  req_cookies : set Cookie,
  req_body : lone Resource,
  -- response
  ret_set_cookies : set Cookie,
  ret_body : lone Resource,
}{
  from in Client
  to in Server
  to = dns_resolve[url.host]
  all c : ret_set_cookies | url.host in c.domains
  ret_body in to.responses[url.path]
}

/* HTTP Components */
abstract sig Client extends Module {}
abstract sig Server extends Module { responses : Path -> Resource }
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
fun dns_resolve[d : Domain] : Server { 
	DNS.map[d] 
}


/* Run commands */

// A simple request
run {} for 2 but exactly 2 Path

// Let's force responses to set cookies
run { all r : HttpRequest | some r.ret_set_cookies } for 3

-- TODO: We can get the same host mapping to multiple servers!
// Can we get a request for a path that's not mapped by the server?
// We do, this will generate a counterexample
check { all r : HttpRequest | r.url.path in r.to.responses.Resource } for 3
