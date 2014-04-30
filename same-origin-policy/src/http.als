/**
	* http.als
	*   A model of the Hypertext Transfer Protocol.
	*/
module http

open event

-- TODO: Hos t->Domain
sig Protocol, Host, Port, Path {}

sig URL {
  protocol : Protocol,
  host : Host,
  -- port and path are optional
  port : lone Port,
  path : lone Path
}

/* HTTP Requests */

abstract sig Method {}

one sig Get, Post extends Method {}

abstract sig HttpRequest extends Event {
  -- request
  url : URL,
  method : Method,
  req_cookies : set Cookie,
  req_body : lone Resource,
  -- response
  ret_set_cookies : set Cookie,
  ret_body : lone Resource,
  from : Client,
  to : Server
}{
	to = dns_resolve[url.host]  // C2
	all c : ret_set_cookies | url.host in c.hosts  // C3
}

/* HTTP Components */
abstract sig Client {}

abstract sig Server {
    paths : set Path, -- paths mapped by this server
	responses : paths -> (Resource + Cookie)
}{
	// C1
   all req : HttpRequest & to.this {
	 req.ret_body + req.ret_set_cookies in responses[req.url.path]
    }
}

abstract sig Resource {}
abstract sig Cookie {
  -- by default all cookies are scoped to the host. The cookie domain and path
  -- field could be used to broaden (thus adding more hosts) or limit the scope
  -- of the cookie.
  hosts : some Host,
  path : lone Path
}

/* Domain Name Server */
one sig DNS { 
	map : Host -> one Server 
}{
	all s : Server | some h : Host | h.map = s  // C5
}
fun dns_resolve[h : Host] : Server { 
	DNS.map[h] 
}


/* Run commands */

// To follow the same steps as in the book chapter: 
//   1 - comment all C1-C5
//   2 - remove `one` in `map : Host -> one Server`


// A simple request
// We discover that a server could return an incorrect resource (uncomment C1)
run {}  for 2 but exactly 2 Path

// Let's force to have some DNS map
// We discover that the request is not being routed to the correct server
// (uncomment C2)
run { some map } for 2 but exactly 2 Host

// Let's force responses to set cookies
// We discover that cookies could be scoped to a host that is not the
// corresponding one (uncomment C3)
run { all r : HttpRequest | some r.ret_set_cookies } for 3

// We can get the same host mapping to multiple servers! (change `map` to
// `Host -> one Server`)
// We can get servers that serve no hosts (though true in reality that's not
// very useful because no requests would go to them) (uncomment C5)
run {} for 3

// Can we get a request for a path that's not mapped by the server?
// We do, this will generate a counterexample
check { all r : HttpRequest | r.url.path in r.to.paths } for 3
