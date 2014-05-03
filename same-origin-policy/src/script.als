/**
  *  script.als
  *    A model of a browser script
  */
module script

open browser


// A script can issue requests through the XmlHttpRequest object
// In reality though, these requests are issued by the browser on behalf
// of the script but that's fine.
sig Script extends Client { context: Document }

/* Calls initiated by scripts*/

// HTTP requests sent by a script
sig XmlHttpRequest extends HttpRequest {}{
  from in Script
  noBrowserChange[before, after] and noDocumentChange[before, after]
}

abstract sig BrowserOp extends Call { doc: Document }{
  from in Script and to in Browser
  doc + from.context in to.documents.before
  -- states of browsers remain the same; only documents themselves change
  noBrowserChange[before, after]
}

// Reads the content of a document
// Represents a set of accessor methods such as "document.documentElement"
sig ReadDom extends BrowserOp { result: Resource }{
  -- return the current content of the target document
  result = doc.content.before
  -- neither content nor domain property of document changes
  noDocumentChange[before, after]
}

// Modify the content of a document
sig WriteDom extends BrowserOp { newDom: Resource }{
  -- the new content of the document is set to input argument
  content.after = content.before ++ doc -> newDom
  -- domain property doesn't change
  domain.after = domain.before
}

// Modify the document.domain property
sig SetDomain extends BrowserOp { newDomain: set Domain }{
  doc = from.context
  domain.after = domain.before ++ doc -> newDomain
  -- no change to the content of the document
  content.after = content.before
}

pred noBrowserChange[before, after: Time] {
  documents.after = documents.before and cookies.after = cookies.before
}

pred noDocumentChange[before, after: Time] {
  content.after = content.before and domain.after = domain.before
}

/* Commands */

// Can a script set the "document.domain" property with a new_domain that doesn't
// match the src?
check { all sd: SetDomain | sd.doc.src.host in sd.newDomain }

run {
  some ReadDom
  some XmlHttpRequest
  all t : Time | #documents.t = 2
  no cookies
  no sentCookies
  no setCookies
  no response
  some body
  ReadDom in XmlHttpRequest.prevs
  XmlHttpRequest.url = Script.context.src
  #Resource > 1
  #Document.src > 1
  all t : Time - last | one before.t
} for 3

run {
	some r : XmlHttpRequest {
		r.from.context.src.host != r.url.host
	}
  all t : Time | #documents.t = 2
  no content
   no cookies
  no sentCookies
  no setCookies
  some response
  no body
  #Document = 2
  #Resource > 1
  all t : Time - last | one before.t
} for 3 but 2 Time
