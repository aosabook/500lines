/**
  *  jsonp.als
  *    A model of the JSONP mechanism 
  *		for cross-domain communication from a script and a server
  */
module jsonp

open browser
open http
open script
open origin

sig Callback {}

sig JsonpRequest in browser/BrowserHttpRequest {
  cb : Callback
}

sig JsonpScript in script/Script {
  cb : Callback,
  payload : lone Resource
}{
  some req : JsonpRequest |
    -- its context must be a document that resulted from a JSONP request
    context in req.doc and
    payload in req.response
}

run {
	some JsonpRequest
	some JsonpScript
}

