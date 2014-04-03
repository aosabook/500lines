/**
	* http.als
	* 	A model of the Hypertext Transfer Protocol.
	*/
module http

open message


sig Host {}
sig Port {}
sig Protocol {}
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
	host : Host,
	protocol : Protocol,
	port : lone Port
}

sig Server extends message/EndPoint {	
	resMap : URL -> lone message/Resource	-- maps each URL to at most one resource
}

/* HTTP Messages */
abstract sig HTTPReq extends message/Msg {
	url : URL,
	method : Method
}{
	sender not in Server
	receiver in Server
}

abstract sig HTTPResp extends message/Msg {
	inResponseTo : HTTPReq
}{
	sender in Server
	receiver not in Server
	one payloads
	payload in message/Resource
	inResponseTo in prevs[this]
}

