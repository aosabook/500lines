/**
  *  postmessage.als
  *    A model of the HTML5 PostMessage mechanism intended for cross-domain
  *    communication between scripts
  */
module postMessage

open origin
open script

// Browser API function for cross-document messaging
// used to send a message from one script to another
sig PostMessage extends BrowserOp {
  message: Resource,
  targetOrigin: Origin
}{
  noDocumentChange[start, end]
}

sig ReceiveMessage extends EventHandler {
  data: Resource,
  srcOrigin: Origin
}{
  causedBy in PostMessage
  -- "ReceiveMessage" event is sent to the script with the correct context
  origin[to.context.src] = causedBy.targetOrigin
  -- messages match
  data = causedBy.@message
  -- the origin of the sender script is provided as "srcOrigin" param 
  srcOrigin = origin[causedBy.@from.context.src]
}

run { some m: ReceiveMessage | m.srcOrigin != m.causedBy.targetOrigin }
