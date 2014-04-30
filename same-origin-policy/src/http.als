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
  -- from, to
  client : Client,
  server : Server
}{
	all c : ret_set_cookies | url.host in c.hosts 	
}

/* HTTP Components */
abstract sig Client {}

abstract sig Server {
    paths : set Path, -- paths mapped by this server
	responses : paths -> (Resource + Cookie)
}{
/*
	owns = paths.resources
	all req : HttpRequest & to.this {
		-- server can't get requests with the wrong path
		req.url.path in paths
		-- returns the corresponding resource
		req.returns = resources[req.url.path]
    }
*/
}

abstract sig Resource {}
abstract sig Cookie {
  -- by default all cookies are scoped to the host. The cookie domain and path
  -- field could be used to broaden (thus adding more hosts) or limit the scope
  -- of the cookie.
  hosts : set Host,
  path : lone Path
}

/* Domain Name Server */
one sig DNS { 
	map : Host -> Server 
}
fun dns_resolve[h : Host] : Server { 
	DNS.map[h] 
}


// A simple "GET" request from client to server
run {
  all r : HttpRequest | r.method in Get
}  for 3 but exactly 1 Client, 1 Server, 1 Host, 2 HttpRequest

run {} for 3
