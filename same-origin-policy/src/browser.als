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
	tags : set HTMLTag,
	dom : DOM,
	script : lone Script
}{
	some script implies script.context = location
}
sig Script extends message/EndPoint {
	context : http/URL
}{
	some script.this
}

sig XMLHTTPReq in http/HTTPReq {
}{
	from in Script
}

sig DOM extends message/Resource {}

abstract sig DomAPICall extends message/Msg {
	frame : Frame	-- frame that contains the DOM
}{
	from in Script
	to in Browser
	frame in Browser.frames
}
sig ReadDOM extends DomAPICall {
}{
	payload in DOM
	one payload
}
sig WriteDOM extends DomAPICall {
}{
	payload in DOM
	one payload
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
				r.from = b 
				r.url = t.src 
				r.method = GET
			}
		}
		t in FormTag implies {
			// for every form input tag
			some r : HTTPReq {
				r.from = b
				r.url = t.action
				r.method = t.method
			}
		}
	}
}
