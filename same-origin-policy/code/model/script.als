/** A model of a browser script */
module script

open browser

// A script can issue requests through the XmlHttpRequest object
// In reality though, these requests are issued by the browser on behalf
// of the script but that's fine.
abstract sig Script extends Client { context: Document }

fact Wellformedness {
  -- no two scripts share the same document as their context
  no disj s1, s2: Script | s1.context = s2.context
}

/* Calls initiated by a script */

-- browser that script "s" is running in at time "t"
fun browser[s : Script, t : Time] : Browser { (documents.t).(s.context) }

// HTTP requests sent by a script
sig XmlHttpRequest extends HttpRequest {}{
  from in Script
  -- browser that contains this script
  let b = from.browser[start] | 
    sentCookies in b.cookies.start and
    -- every cookie sent must be scoped to the url of the request
    matchingScope[sentCookies, url]
  noBrowserChange[start, end] and noDocumentChange[start, end] 
}

abstract sig BrowserOp extends Call { doc: Document }{
  from in Script and to in Browser
  doc + from.context in to.documents.start
  -- states of browsers remain the same; only documents themselves change
  noBrowserChange[start, end]
}

// Reads the content of a document
// Represents a set of accessor methods such as "document.documentElement"
sig ReadDom extends BrowserOp { result: Resource }{
  -- return the current content of the target document
  result = doc.content.start
  -- neither content nor domain property of document changes
  noDocumentChange[start, end]
}

// Modify the content of a document
sig WriteDom extends BrowserOp { newDom: Resource }{
  -- the new content of the document is set to input argument
  content.end = content.start ++ doc -> newDom
  -- domain property doesn't change
  domain.end = domain.start
}

// Handlers for browser script events
abstract sig EventHandler extends Call {
  causedBy: Call
}{
  -- this call must happen after the call that caused it
  lt[causedBy.@start, start]
  from in Browser and to in Script
  from in causedBy.@to + causedBy.@from
  to.context in from.documents.start
  noDocumentChange[start, end]
  noBrowserChange[start, end]
}

pred noBrowserChange[start, end: Time] {
  documents.end = documents.start and cookies.end = cookies.start
}

pred noDocumentChange[start, end: Time] {
  content.end = content.start and domain.end = domain.start
}

run {} for 3
