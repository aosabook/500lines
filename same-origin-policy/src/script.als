/**
	* browser.als
	* 	A model of a browser with no-SOP.
	*/
module script

open browser


// A script can issue requests through the XmlHttpRequest object
// In reality though, these requests are issued by the browser on behalf
// of the script but that's fine.
abstract sig Script extends Client {
	-- the document under which this script is executing
	doc : Document
}
fun context : Script -> URL {
	(Script <: doc).src
}

/* Calls initiated by scripts*/

// HTTP requests sent by a script
sig XMLHttpRequest extends HttpRequest {}{
	from in Script
}


// API calls for accessing the content of a document (i.e. DOM)
abstract sig DOMAPICall extends Call {
  target_document : Document		-- document that contains the DOM to be accessed
}{
  from in Script
  to in Browser 
  target_document in to.documents.pre
}

// Reads the content of a document 
// Represents a set of accessor methods such as "document.documentElement"
sig ReadDOM extends DOMAPICall {}{
  no args
  returns = target_document.content.pre
}

// Modify the content of a document
sig WriteDOM extends DOMAPICall {}{
  no returns
  target_document.content.post = args
}

// Modify the document.domain property
sig ModifyDomainProperty extends Call {
  new_domain : Domain
} {
  from in Script
  to in Browser
  no args + returns
  src.(from.context).domain_prop.post = new_domain
}

fact FrameConditions {
	all t: Time-last | let t' = t.next {
		all d : Document | 
			-- the content of a document can only change iff 
			d.content.t != d.content.t' implies
				some c : Call & pre.t |
					-- browser sends a new HTTP request or 
					(c in BrowserHttpRequest and c.doc = d) or
					-- a script modifies the document
					(c in WriteDOM and c.target_document = d)

		all b : Browser |
			-- the browser cookies only change when it sends a new HTTP request
			(b.documents.t != b.documents.t' or b.cookies.t != b.cookies.t') implies
				(some c : BrowserHttpRequest & pre.t | c.from = b)
	}
}

run {} for 3
