/**
	* A very basic model of a browser
	*/
module browser
open http

sig Browser extends http/Client {
	frames: Frame -> Time
	}

sig Frame {
	-- Url from which this frame originated
	location: http/Url,
	-- Dom object associated with this frame at a given time
	dom: Dom -> Time,
	-- script contained in this frame?
	script: lone Script
	} { script.context in location }

sig Script extends http/Client {
	-- the context in which this script is executing (what does this mean??)
	context: http/Url
	}

sig XmlHttpReq in http/HttpReq { }
	{ from in Script }

-- for now, treat DOM objects as resources too (but this is strange and confusing)
sig Dom extends http/Resource {}

abstract sig DomCall extends http/Call {
	frame: Frame	-- frame that contains the DOM (what does this mean?)
	} { from in Script and to in Browser }

-- add frame conditions later for dom calls
sig ReadDom extends DomCall {
	} { no arg and result = frame.dom.before }

sig WriteDom extends DomCall {
	} { arg in Dom and no return and frame.dom.after = arg }

-- now explain effect of http requests?
-- say that any context of a frame must have come from an HTTP request?
fact {
//	all r: GetHttpReq, b: Browser | ... 
	}
