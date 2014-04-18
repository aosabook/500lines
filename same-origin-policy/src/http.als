/**
	* http.als
	* 	A model of the Hypertext Transfer Protocol.
	*/
module http

open message


sig Protocol {}
sig Host {} -- Hostname (e.g. www.example.com)
sig Port {}
sig Path {}
abstract sig Method {}

-- a more detailed model could include the other request methods (like HEAD,
-- PUT, OPTIONS) but these are not relevant to the analysis.
one sig GET, POST extends Method {}

// Given an example URL "http://www.example.com/dir/page.html",
// "http" is the protocol,
// "www.example.com" is the host,
// "/dir/path.html" is the path, and
// the port is omitted.
sig URL {
	protocol : Protocol,
	host : Host,
	-- port and path are optional
	port : lone Port,
	path : lone Path
}

// An origin is defined as a triple (protocol, host, port) where port is optional
sig Origin {
	protocol : Protocol,
	host : Host,
	port : lone Port
}

fun url2origin[u : URL] : Origin {
	{o : Origin | o.host = u.host and o.protocol = u.protocol and o.port = u.port }
}

abstract sig Server extends message/EndPoint {	
	urls : set URL,
	resMap : urls -> lone message/Resource	-- maps each URL to at most one resource
}{
	owns = resMap[urls]
	
	all req : HTTPReq |
		req.to = this implies
				req.returns = resMap[req.url]
}

/* HTTP requests */
abstract sig HTTPReq extends message/Msg {
	url : URL,
	method : Method
}{
	to in Server
	no args
	one returns
	returns in message/Resource
}
