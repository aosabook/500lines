/**
  *  http.als
  *    A model of the Hypertext Transfer Protocol.
  */
module http
open call[Endpoint]
open util/relation

abstract sig Resource {}
abstract sig Endpoint {}

abstract sig Protocol, Port, Path {}
sig Domain { subsumes: set Domain }
fact subsumesRule { partialOrder[subsumes, Domain] }

sig Url {
  protocol: Protocol,
  host: Domain,
  port: lone Port,
  path: Path
}

/* HTTP Requests */
abstract sig HttpRequest extends Call {
  -- request
  url: Url,
  sentCookies: set Cookie,
  body: lone Resource,
  -- response
  receivedCookies: set Cookie,
  response: lone Resource,
}{
  from in Client
  to in Dns.map[url.host]
  all c: receivedCookies | url.host in c.domains
  response = to.resources[url.path]
}

/* HTTP Components */
abstract sig Client extends Endpoint {}
abstract sig Server extends Endpoint { resources: Path -> lone Resource }

fact ServerAssumption {
  -- two servers mapped from the common domain on DNS must provide the same resources
  all s1, s2: Server | 
    (some Dns.map.s1 & Dns.map.s2) implies s1.resources = s2.resources
}

-- by default all cookies are scoped to the host. The cookie domain and path
-- field could be used to broaden or limit the scope of the cookie.
sig Cookie {  domains: set Domain }

/* Domain Name Server */
one sig Dns { map: Domain -> Server }

/* Commands */

// A simple request
run {} for 3 

// Let's force responses to set cookies
run { all r: HttpRequest | some r.receivedCookies } for 3 

// Can we get a request for a path that's not mapped by the server?
check { all r: HttpRequest | r.url.path in r.to.resources.Resource } for 3 

// Can we get the same domain mapping to multiple servers?
check { all d: Domain | no disj s1, s2: Server | s1 + s2 in Dns.map[d] } for 3 

// If we do the same request twice, can we get a different response?
check { 
  all r1, r2: HttpRequest | r1.url = r2.url implies r1.response = r2.response
} for 3 
