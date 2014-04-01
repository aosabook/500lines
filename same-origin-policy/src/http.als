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

sig Server extends message/EndPoint {	
	resMap : URL -> lone message/Resource	-- maps each URL to at most one resource
}

abstract sig Client extends message/EndPoint {}



/* HTTP Messages */
abstract sig HTTPReq extends message/Msg {
	url : URL
}{
	sender in Client
	receiver in Server
}
sig GET, POST, OPTIONS extends HTTPReq {}


abstract sig HTTPResp extends message/Msg {
	res : message/Resource,
	inResponseTo : HTTPReq
}{
	sender in Server
	receiver in Client
	payloads = res
}
