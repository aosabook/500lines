/**
	* http.als
	* 	A model of the Hypertext Transfer Protocol.
	*/
module http

open call


// A domain name
sig Domain {
	subsumes : set Domain
}{
	this in subsumes
}
sig Path {}
// Host is a domain name with a specific IP attached to it
// http://en.wikipedia.org/wiki/Domain_name#Domain_name_space
sig Host in Domain {} -- Host (e.g. www.example.com)
sig Protocol, Port {}

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
  to in Server
  to = dns_resolve[url.host]	
  -- cookies are always part of the header, never the body
  no ret_body & Cookie
  returns = ret_set_cookies + ret_body
  args = cookies
}

/* HTTP Components */

abstract sig Client extends Component {}

abstract sig Server extends Component {
    paths : set Path, -- paths mapped by this server
	resources : paths -> Resource
}{
	owns = paths.resources
	all req : HttpRequest & receives[this] {
		-- server can't get requests with the wrong path
		req.url.path in paths
		-- returns the corresponding resource
		req.returns = resources[req.url.path]
    }
}

abstract sig Cookie extends Resource {
  -- by default all cookies are scoped to the host. The cookie domain and path
  -- field could be used to broaden or limit the scope of the cookie.
  domain : lone Domain,
  path : lone Path
}

/* Domain Name Server */
one sig DNS { 
	map : Host -> Server 
}
fun dns_resolve[h : Host] : Server { 
	DNS.map[h] 
}

// EK: Commented out; not really necessary?
/*
fact {
  // all cookies are mapped by some server -- i.e., no lingering cookies
  // TODO: all html resources are mapped by some server
   all c : Cookie | some s : Server | c in s.paths.(s.resources)
}
*/

// EK: Commented out for now
/*
// TODO: Maybe we don't need this. We can just have the browser "parse"
// "Resources" into Documents and Scripts instead of HtmlResources

sig HtmlResource extends Resource {}

// An HTTP server that only serves HTML.
// (TODO: read comment above, maybe we don't need this)
sig HtmlServer extends Server {
} { resources in Path -> (HtmlResource + Cookie) }
*/

run {}
