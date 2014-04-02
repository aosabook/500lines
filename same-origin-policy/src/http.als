/**
	* http.als
	* 	A model of the Hypertext Transfer Protocol.
	*/
module http

open message


/* Server-side components */
sig Host {}
sig Port {}
sig Protocol {}
sig Path {}
abstract sig Method {}
one sig GET, POST extends Method {}	// can be expanded for other methods

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

/* Client-side components */
sig Browser extends message/EndPoint {
	frames : set Frame
}
sig Frame {
	location : URL,
	dom : DOM,
	tags : set HTMLTag,
	script : lone Script
}{
	some script implies script.context = location
}
sig Script extends message/EndPoint {
	context : URL
}

/* HTTP Messages */
abstract sig HTTPReq extends message/Msg {
	url : URL,
	method : Method
}{
	sender in Browser + Script
	receiver in Server
}

sig XMLHTTPReq in HTTPReq {
}{
	sender in Script
}

abstract sig HTTPResp extends message/Msg {
	res : message/Resource,
	inResponseTo : HTTPReq
}{
	sender in Server
	receiver in Browser + Script
	payloads = res
}

/* Frame interactions */
sig DOM extends message/Resource {}

abstract sig DomAPI extends message/Msg {
	frame : Frame	-- frame that contains the DOM
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

/* HTML Tags */
abstract sig HTMLTag {}
abstract sig SrcTag extends HTMLTag {
	src : URL
}
sig ImgSrcTag, ScriptTag extends SrcTag {}

sig FormTag extends HTMLTag {
	action: URL,
	method : Method
}

fact {
	// Browser makes an HTTP request 
	all b : Browser, f : b.frames, t : f.tags {
		t in SrcTag implies {
			// for every HTML tag that has a "src" attribute
			some r : HTTPReq {
				r.sender = b 
				r.url = t.src 
				r.method = GET
			}
		}
		t in FormTag implies {
			// for every form input tag
			some r : HTTPReq {
				r.sender = b
				r.url = t.action
				r.method = t.method
			}
		}
	}
}

