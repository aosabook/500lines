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

sig CallbackID {}

// Request sent as a result of <script> tag
sig JsonpRequest in browser/BrowserHttpRequest {
  padding: CallbackID
}

// Callback function called when the JSONP request completes
sig JsonpCallback extends script/EventHandler {
  cb: CallbackID,
  payload: lone Resource
}{
  causedBy in JsonpRequest
  cb = causedBy.@padding
  -- the result of the JSONP request is passed on as an argument to the callback
  payload in causedBy.response
}

run { some cb: JsonpCallback | some cb.payload }

