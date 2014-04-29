/**
	* browser.als
	* 	A model of a browser
	*/
module browser

open http
open call

abstract sig Document {
  src : URL ,										-- URL from which this document was originated
  content : Resource -> Time,			-- the content of the document (i.e., DOM)
  -- "document.domain" property, at any time it could match several hosts (if
  -- for example is set to something like *.foo.com)
  domain_prop : Host -> Time, 	
}

abstract sig Browser extends http/Client {
  documents : Document -> Time,	-- documents that browser displays over time
  cookies : Cookie -> Time,				-- cookies stored by the browser over time
}

/* HTTP request sent from a browser to a server */

sig BrowserHttpRequest extends  HttpRequest {
	doc : Document
}{
	from in Browser
	-- browser creates a new document to display the content of the response
	from.documents.post = from.documents.pre + doc
	doc.content.post = ret_body
	doc.domain_prop.post = url.host
	doc.src = url	
	-- new cookies are stored by the browser
	from.cookies.post = from.cookies.pre + ret_set_cookies
}

fact CookieBehavior {
	-- Every cookie sent from a browser or one of its script must be scoped to a domain
	-- that matches or subsumes the destination host of the request
	all r : HttpRequest, b : Browser |
		let t = r.pre | 
			r.from in b + b.(documents.t).~doc implies
				r.args & Cookie in {c : Cookie | r.url.host in c.hosts }
}

run {} for 3
