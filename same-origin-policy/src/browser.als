/**
	* browser.als
	* 	A model of a browser.
	*/
module browser

open http
open call

/* Components */
abstract sig Browser extends Component {
	frames :  Frame -> Time,
	cookies : URL -> Cookie
}{
	-- every frame must have been received as a respones of some previous request
	all f : Frame, t : Time |
 		f -> t in frames iff
			some r : HTTPReq | 	
				r.pre in prevs[t] and
				r.from = this and
				f.dom in r.returns and
				f.location = r.url 

	-- initially does not own any resource
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

abstract sig Script extends Component {
	-- the context in which this script is executing
	context : http/URL
}{
	-- every script must belong to some frame
	some script.this
}

abstract sig Cookie extends Resource {}
abstract sig DOM extends Resource {}
abstract sig HTMLTag {}

fact CookieBehavior {
	all r : HTTPReq, b : Browser |
		let t = r.pre | 
			r.from in b + b.(frames.t).script implies
				r.args & Cookie in b.cookies[r.url] 
}

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
abstract sig DomAPICall extends Call {
	frame : Frame	-- frame that contains the DOM
}{
	from in Script
	to in Browser
	frame in to.frames.pre
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


