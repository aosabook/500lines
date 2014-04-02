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
