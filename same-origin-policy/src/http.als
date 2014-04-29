/**
	* http.als
	* 	A model of the Hypertext Transfer Protocol.
	*/
module http

open call


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

-- TODO: do we need POST requests?
one sig Get, Post extends Method {}

abstract sig HttpRequest extends Call {
  -- request
  url : URL,
  method : Method,
  cookies : set Cookie,
  -- response
  ret_set_cookies : set Cookie,
  ret_body : lone Resource,
}{
  from in Client
  to in dns_resolve[url.host]	
  -- cookies are always part of the header, never the body
  no ret_body & Cookie
  returns = ret_set_cookies + ret_body
  args = cookies
  all c : ret_set_cookies | url.host in c.hosts 
}

/* HTTP Components */

abstract sig Client extends Component {} {
  no owns
}

abstract sig Server extends Component {
    paths : set Path, -- paths mapped by this server
	resources : paths -> Resource
}{
	owns = paths.resources
	all req : HttpRequest & to.this {
		-- server can't get requests with the wrong path
		req.url.path in paths
		-- returns the corresponding resource
		req.returns = resources[req.url.path]
    }
}

sig Cookie in Resource {
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

run {} for 3
