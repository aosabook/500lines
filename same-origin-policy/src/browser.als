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
  domain : Domain -> Time, 	
}

abstract sig Browser extends Client {
  documents : Document -> Time,	-- documents that browser displays over time
  cookies : Cookie -> Time,				-- cookies stored by the browser over time
}

/* HTTP request sent from a browser to a server */

sig BrowserHttpRequest extends HttpRequest {
  doc : Document
}{
  from in Browser

  -- every cookie sent must be scoped to the url of the request
  req_cookies in matchingCookies[from.cookies.before, url]

  -- browser creates a new document to display the content of the response
  documents.after = documents.before + from -> doc
  content.after = content.before ++ doc -> ret_body
  domain.after = domain.before ++ doc -> url.host
  doc.src = url	

  -- new cookies are stored by the browser
  cookies.after = cookies.before + from -> ret_set_cookies
}

// return the subset of "cookies" with the scope that matches the url "u"
fun matchingCookies[cookies : set Cookie, u : URL] : set Cookie {
	{ c : cookies | u.host in c.domains }
}

run {} for 3
