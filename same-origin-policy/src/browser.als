/**
	* browser.als
	* 	A model of a browser.
	*/
module browser

open http
open message

/* Components */
abstract sig Browser extends message/EndPoint {
	frames :  Frame -> Msg
}{
	-- every frame must have been received as a respones of some previous request
	all f : Frame, m : Msg |
 		f -> m in frames implies
			some r : (m + prevs[m]) & HTTPReq | 	
				f.dom in r.returns and
				f.location = r.url 

	-- initially does not own any resource
	no owns
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
	frame in Browser.frames.this
}
sig ReadDOM extends DomAPICall {
}{
	no args
	returns = frame.dom
}
sig WriteDOM extends DomAPICall {
}{
	args in DOM
	one args
	no returns
}
