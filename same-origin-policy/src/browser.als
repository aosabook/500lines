/**
	* browser.als
	* 	A model of a browser with no-SOP.
	*/
module browser

open http
open call

abstract sig HTML extends Resource {}

sig Document {
  url : URL , -- URL from which this document was originated
  dom : DOM -> Time,				-- DOM, the content of the document
  domain_prop : Domain -> Time, 	-- "document.domain" property
}{
	all t : Time - last | let t' = t.next | 
		(some r : BrowserHttpRequest | r.pre = t and r.@url = url and dom.t' = r.ret_body) or
		(some w : WriteDOM | w.pre = t and w.target_document = this and dom.t' = w.args) or	
		dom.t' = dom.t 	 
}

sig BrowserCookie extends Cookie {
  host : http/Host  -- we record the host where we got the cookie from
}

one sig Browser extends http/Client {
  frames : Document -> Time,
  cookies : BrowserCookie -> Time,
}{
	-- Every frame must have been received as a respones of some previous request
	all d : Document, t : Time |
 		d -> t in frames iff
			some r : BrowserHttpRequest & this.sends | 	
				r.pre in prevs[t] and
				d.url = r.url

   -- Every cookie must have been received as a response of some prev request	
	all c : Cookie, t : Time | 
		c -> t in cookies iff 
			(c in owns or
			some r : HttpRequest & this.sends | r.pre in prevs[t] and c in r.ret_set_cookies)
}

fact CookieBehavior {
	-- Every cookie sent from a browser or one of its script must be scoped to a domain
	-- that matches or subsumes the destination host of the request
	all r : HttpRequest, b : Browser |
		let t = r.pre | 
			r.from in b + b.(frames.t).~doc implies
				r.args & Cookie in {c : BrowserCookie | r.url.host in c.domain.subsumes }
}

sig BrowserHttpRequest in HttpRequest {
	doc : Document
}{
	from in Browser
	doc = from.frames.post
	doc.@url = url
	doc.dom.post = ret_body
	doc.domain_prop.post = url.host
}
fact {
	all r : HttpRequest | 
		r.from in Browser implies
			r in BrowserHttpRequest
}


// A script can issue requests through the XmlHttpRequest object
// In reality though, these requests are issued by the browser on behalf
// of the script but that's fine.
abstract sig Script extends http/Client {
	-- the document under which this script is executing
	doc : Document
}
fun context : Script -> URL {
	(Script <: doc).url
}

sig DOM in Resource {}
fact {
	// A DOM is just another representation of an HTML file
	DOM = HTML
}

/* DOM API messages */
abstract sig DOMAPICall extends Call {
  target_document : Document	-- frame that contains the DOM
}{
  from in Script
  to in Browser 
  target_document in to.frames.pre
}

sig ReadDOM extends DOMAPICall {}{
  no args
  returns = target_document.dom.pre
}

sig WriteDOM extends DOMAPICall {}{
  args in DOM
  no returns
  target_document.dom.post = args
}

sig ModifyDomainProperty extends Call {
  new_domain : Domain
} {
  from in Script
  to in Browser
  no args
  no returns
 // from.context.Document.url
  url.(from.context).domain_prop.post = new_domain
}

run {} for 3

/* EK: Commented out; not needed?
// TODO: this can be done above automatically instead of having a Parse signature
one sig Parse {
  doc_map : http/HtmlResource -> Document,
  script_map : http/HtmlResource -> lone Script,
} {
-- TODO: if there's a script there's a Document or that resource
}
// We know how to parse all html
fact { all r : http/Resource | some d : Document | r -> d in Parse.doc_map }
*/

