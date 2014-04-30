/**
	* browser.als
	* 	A model of a browser
	*/
module browser

open http

abstract sig Document {
  src : URL ,										-- URL from which this document was originated
  content : Resource -> Time,			-- the content of the document (i.e., DOM)
  -- "document.domain" property, at any time it could match several hosts (if
  -- for example is set to something like *.foo.com)
  domain_prop : Host -> Time, 	
}

abstract sig Browser extends Client {
  documents : Document -> Time,	-- documents that browser displays over time
  cookies : Cookie -> Time,				-- cookies stored by the browser over time
}

/* HTTP request sent from a browser to a server */

sig BrowserHttpRequest extends HttpRequest {
  doc : Document
}{
  client in Browser

  -- every cookie sent must be scoped to the url of the request
  req_cookies in matchingCookies[client.cookies.before, url]

  -- browser creates a new document to display the content of the response
  documents.after = documents.before + client -> ret_body
//  doc.content.after = ret_body
  content.after = content.before ++ doc -> ret_body

  doc.domain_prop.after = url.host
  doc.src = url	

  -- new cookies are stored by the browser
  client.cookies.after = client.cookies.before + ret_set_cookies

  -- no other browser changes
  modified_browsers[client]//before, after]
  modified_docs[none, before, after]
}

// return the subset of "cookies" with the scope that matches the url "u"
fun matchingCookies[cookies : set Cookie, u : URL] : set Cookie {
	{ c : cookies | u.host in c.hosts and u.path in c.path }
}

pred modified_docs[docs : set Document, before, after : Time] {
  -- states of all browsers remain the same
  documents.after = documents.before
  cookies.after = cookies.before
  -- states of the document except "docs" remain the same
  let other_docs = Document - docs | 
    other_docs.content.after = other_docs.content.before and
    other_docs.domain_prop.after = other_docs.domain_prop.before
}

pred Event.modified_browsers[browsers : set Browser, before, after : Time] {
  let other_browsers = Browser - browsers |
    other_browsers.documents.after = other_browsers.documents.before and
    other_browsers.cookies.after = other_browsers.cookies.before
}

run {} for 3
