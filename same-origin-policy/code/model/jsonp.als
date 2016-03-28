/**
  *  jsonp.als
  *    A model of the JSONP mechanism for cross-domain communication from a
  *    script and a server
  */
module jsonp
open script

abstract sig Callback {}  // identifier of a callback function

// Request sent as a result of <script> tag
sig JsonpRequest in BrowserHttpRequest { padding: Callback }{
  response in JsonpResponse
}

// A JsonpResponse is a piece of Javascript 
sig JsonpResponse in Resource {
  cb: Callback,
  payload: Resource
}{
  payload != this
}

fact { all r: JsonpResponse | some req: JsonpRequest | req.response = r }

// Callback function called when the JSONP request completes
sig ExecCallback extends EventHandler {
  cb: Callback,
  payload: Resource
}{
  causedBy in JsonpRequest
  to.context = causedBy.(BrowserHttpRequest <: doc)
  let resp = causedBy.response | 
    cb = resp.@cb and
    -- result of JSONP request is passed on as an argument to the callback
    payload = resp.@payload
}

run { some cb: ExecCallback | some cb.payload }
