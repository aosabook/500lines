/**
  *  postMessage
  *    A model of the HTML5 PostMessage mechanism intended for cross-domain
  *    communication between scripts
  */
module postMessage

open origin
open script
open sop

// Browser API function for cross-document messaging
// used to send a message from one script to another
sig PostMessage extends BrowserOp {
  message: Resource,
  targetOrigin: Origin,
  causes : ReceiveMessage		-- causes browser to invoke ReceiveMessage event handler
}{
  -- "ReceiveMessage" event is sent to the script with the correct context
  targetOrigin = origin[causes.@to.context.src]
  message = causes.@message
  -- the origin of the sender script is provided as "srcOrigin" param 
  causes.srcOrigin = origin[from.context.src]
}

sig ReceiveMessage extends EventHandler {
  message: Resource,
  srcOrigin: Origin,
}

fact PostMessageFact {
  -- there is exactly one PostMessage call for every ReceiveMessage
  causes in PostMessage one -> one ReceiveMessage
}

run { some m: PostMessage | m.targetOrigin != m.causes.srcOrigin }
