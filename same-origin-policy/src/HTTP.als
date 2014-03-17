/**
	* HTTP.als
	* 	A model of the Hypertext Transfer Protocol.
	*/
module HTTP

open Message


/* Server-side components */
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
	// port and path are optional
	port : lone Port,		
	path : lone Path
}

sig Server extends Module {	
	resMap : URL -> lone Resource		// maps each URL to at most one resource
}

/* Client-side components */
sig Browser extends Module {
	frames : set Frame
}
sig Frame {
	location : URL,
	dom : DOM,
	script : lone Script		
}{
	some script implies script.context = location
}
sig Script extends Module {
	context : URL
}

/* HTTP Messages */
abstract sig HTTPReq extends Msg {
	url : URL
}{
	sender in Browser + Script
	receiver in Server
}
sig GET, POST, OPTIONS extends HTTPReq {}

abstract sig HTTPResp extends Msg {
	res : Resource,
	inResponseTo: HTTPReq
}{
	sender in Server
	receiver in Browser + Script
	payloads = res
}

/* Frame interactions */
sig DOM extends Resource {}

abstract sig DomAPI extends Msg {
	frame : Frame									// frame that contains the DOM
}{
	sender in Script
	receiver in Browser
	frame in Browser.frames
}
sig ReadDOM extends DomAPI {
	dom : DOM,
}{
	payloads = dom
}
sig WriteDOM extends DomAPI {
	newDOM : DOM
}{
	payloads = newDOM
}

