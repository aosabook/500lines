/**
	* browser.als
	* 	A model of a browser.
	*/
module browser

open http
open message


sig Browser extends message/EndPoint {
	frames : set Frame
}
sig Frame {
	location : http/URL,
	dom : DOM,
	script : lone Script
}{
	some script implies script.context = location
}
sig Script extends message/EndPoint {
	context : http/URL
}

sig XMLHTTPReq in http/HTTPReq {
}{
	sender in Script
}

sig DOM extends message/Resource {}

abstract sig DomAPICall extends message/Msg {
	frame : Frame	-- frame that contains the DOM
}{
	sender in Script
	receiver in Browser
	frame in Browser.frames
}
sig ReadDOM extends DomAPICall {
	dom : DOM,
}{
	payloads = dom
}
sig WriteDOM extends DomAPICall {
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
