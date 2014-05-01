/**
   *  http.als
   *    A model of the Hypertext Transfer Protocol.
   */
module http

open call[Endpoint]

abstract sig Resource {}

sig Protocol, Domain, Port, Path {}

sig URL {
  protocol: Protocol,
  host: Domain,
  port: lone Port,
  path: Path
}

/* HTTP Requests */

abstract sig HttpRequest extends Call {
  -- request
  url: URL,
  cookies: set Cookie,
  body: lone Resource,
  -- response
  setCookies: set Cookie,
  response: lone Resource,
}{
  from in Client
  to in DNS.map[url.host]
  all c: setCookies | url.host in c.domains
  response in to.resources[url.path]
}

abstract sig GetRequest, PostRequest extends HttpRequest {}

/* HTTP Components */
abstract sig Client extends Endpoint {}
abstract sig Server extends Endpoint { resources: Path -> Resource }

-- must this be abstract??
abstract sig Cookie {
  -- by default all cookies are scoped to the host. The cookie domain and path
  -- field could be used to broaden (thus adding more hosts) or limit the scope
  -- of the cookie.
  domains: set Domain,
}

/* Domain Name Server */
one sig DNS { 
	map: Domain -> Server 
}{
-- drop this? don't think it's needed
	all s: Server | some d: Domain | d.map = s
}


/* Run commands */

// A simple request
run {}

// Let's force responses to set cookies
run { all r: HttpRequest | some r.set_cookies }

// Can we get a request for a path that's not mapped by the server?
check { all r: HttpRequest | r.url.path in r.to.resources.Resource }

// Can we get the same domain mapping to multiple servers?
check { all d: Domain | no disj s1, s2: Server | s1 + s2 in DNS.map[d] }
