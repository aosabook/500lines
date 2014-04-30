/**
	* script.als
	* 	A model of a browser script
	*/
module script

open browser


// A script can issue requests through the XmlHttpRequest object
// In reality though, these requests are issued by the browser on behalf
// of the script but that's fine.
abstract sig Script extends Client {
  context : Document
}

/* Calls initiated by scripts*/

// HTTP requests sent by a script
sig XMLHttpRequest extends HttpRequest {}{
  client in Script
}

abstract sig BrowserOp extends Event {
	from : Script,
	to : Browser
}{
  from.context in to.documents.before
}

// API calls for accessing the content of a document (i.e. DOM)
abstract sig DomApi extends BrowserOp {
  target_document : Document		-- document that contains the DOM to be accessed
}{
  -- target document must currently exist in the browser 
  target_document in to.documents.before
}

// Reads the content of a document 
// Represents a set of accessor methods such as "document.documentElement"
sig ReadDOM extends DomApi {
  result : Resource
}{
  -- return the current content of the target document
  result = target_document.content.before
  modified_docs[none, before, after]
}

// Modify the content of a document
sig WriteDOM extends DomApi {
  new_dom : Resource
}{
  -- the new content of the document is set to input argument
  target_document.content.after = new_dom
  modified_docs[target_document, before, after]
}

// Modify the document.domain property
sig SetDomain extends BrowserOp {
  new_domain : set Host
} {
  let doc = from.context |
    doc.domain_prop.after = new_domain and
    -- no change to the content of the document
    doc.content.after = doc.content.before and
    -- does not modify any other document
	modified_docs[doc, before, after]
}

run {} for 3

