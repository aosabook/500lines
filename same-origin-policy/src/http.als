/**
   *  http.als
   *    A model of the Hypertext Transfer Protocol.
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
	to = dns_resolve[url.host]
	all c : ret_set_cookies | url.host in c.hosts
}

/* HTTP Components */
abstract sig Client {}

abstract sig Server {
  -- TODO: remove paths
    paths : set Path, -- paths mapped by this server
    -- TODO: remove Cookie from responses
	responses : paths -> (Resource + Cookie)
}{
  -- TODO: move to request
   all r : HttpRequest & to.this | r.ret_body + r.ret_set_cookies in responses[r.url.path]
}

abstract sig Resource {}
abstract sig Cookie {
  -- by default all cookies are scoped to the host. The cookie domain and path
  -- field could be used to broaden (thus adding more hosts) or limit the scope
  -- of the cookie.
  hosts : some Host,
        -- TODO: remove path
  path : lone Path
}

/* Domain Name Server */
one sig DNS { 
	map : Host -> Server 
}{
	all s : Server | some h : Host | h.map = s
}
fun dns_resolve[h : Host] : Server { 
	DNS.map[h] 
}


/* Run commands */

// A simple request
run {} for 2 but exactly 2 Path

// Let's force responses to set cookies
run { all r : HttpRequest | some r.ret_set_cookies } for 3

-- TODO: We can get the same host mapping to multiple servers!
// Can we get a request for a path that's not mapped by the server?
// We do, this will generate a counterexample
check { all r : HttpRequest | r.url.path in r.to.paths } for 3
