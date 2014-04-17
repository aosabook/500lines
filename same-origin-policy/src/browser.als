/**
	* browser.als
	* 	A model of a browser.
	*/
module browser

open http
open message

/* Components */
abstract sig Browser extends message/EndPoint {
	frames : set Frame
}{
	owns = frames.dom
}

abstract sig Frame {
	-- URL from which this frame originated
	location : http/URL,
	-- HTML tags associated with 
	tags : set HTMLTag,
	dom : DOM,
	script : lone Script
}{
	some script implies script.context = location
}

abstract sig Script extends message/EndPoint {
	-- the context in which this script is executing
	context : http/URL
}{
	-- every script must belong to some frame
	some script.this
	no owns
}

abstract sig DOM extends message/Resource {}
abstract sig HTMLTag {}

/* XMLHTTPReq message */
// HTTPReq requests that are made by a script
sig XMLHTTPReq in http/HTTPReq {
}{
	from in Script
}
fact {
	all r : HTTPReq | r.from in Script implies r in XMLHTTPReq
}

/* DOM API messages */
abstract sig DomAPICall extends message/Msg {
	frame : Frame	-- frame that contains the DOM
}{
	from in Script
	to in Browser
	frame in Browser.frames
}
sig ReadDOM extends DomAPICall {
}{
	no payload
	return = frame.dom
}
sig WriteDOM extends DomAPICall {
}{
	payload in DOM
	one payload
	no return
}
