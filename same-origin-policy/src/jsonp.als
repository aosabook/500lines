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

sig CallbackID {}  // identifier of a callback function

// Request sent as a result of <script> tag
sig JsonpRequest in BrowserHttpRequest {
  padding: CallbackID
}{
  response in JsonpResponse
}

sig JsonpResponse in Resource {
  cb : CallbackID,
}

// Callback function called when the JSONP request completes
sig JsonpCallback extends EventHandler {
  cb: CallbackID,
  payload : Resource
}{
  causedBy in JsonpRequest
  let resp = causedBy.response | 
    cb = resp.@cb and
    -- result of JSONP request is passed on as an argument to the callback
    payload = resp
}

run { some cb: JsonpCallback | some cb.payload }

