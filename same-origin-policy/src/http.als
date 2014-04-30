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
	// C2
	//server = dns_resolve[url.host]
	// C3
	//all c : ret_set_cookies | url.host in c.hosts 	
}

/* HTTP Components */
abstract sig Client {}

abstract sig Server {
    paths : set Path, -- paths mapped by this server
	responses : paths -> (Resource + Cookie)
}{

		-- server can't get requests with the wrong path

		-- returns the corresponding resource
	// C1
   /*all req : HttpRequest & server.this {
        //req.url.path in paths
	 req.ret_body + req.ret_set_cookies in responses[req.url.path]
    }*/
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
	map : Host -> Server 
}
fun dns_resolve[h : Host] : Server { 
	DNS.map[h] 
}

// A simple request.
// We discover that a server could return an incorrect resource (uncomment C1)
run {}  for 2 but exactly 2 Path
/*
exactly 1 Client, exactly 1 Server, exactly 1 Host, exactly 2 HttpRequest,
exactly 2 Path*/

// Let's force to have some Dns map
// We discover that the request is not being routed to the correct server
// (uncomment C2)
run { some map } for 2 but exactly 2 Host//but exactly 1 Client, exactly 1 Server, exactly 2 Host, exactly 2 HttpRequest

// Let's force responses to set cookies
// We discover that cookies could be scoped to a host that is not the
// corresponding one (uncomment C3)
run { all r : HttpRequest | some r.ret_set_cookies } for 3 but exactly 1 Client, exactly 1 Server, exactly 2 Host, exactly 2 HttpRequest
