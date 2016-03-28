/**
  *  browser.als
  *    A model of a browser
  */
module browser
open http

abstract sig Document {
  src: Url,  -- URL from which this document was originated
  content: Resource -> Time,  -- the content of the document (i.e., DOM)
  domain: Domain -> Time,  -- "document.domain" property
}

abstract sig Browser extends Client {
  documents: Document -> Time,  -- documents that the browser displays over time
  cookies: Cookie -> Time,  -- cookies stored by the browser over time
}

fact Wellformedness {
  -- no two browsers can share documents
  no disj b1, b2: Browser, t: Time | some b1.documents.t & b2.documents.t
  -- documents already opened are well-formed
  all b: Browser, d: b.documents.init |
    d.src.host = d.domain.init
}

/* HTTP request sent from a browser to a server */

sig BrowserHttpRequest extends HttpRequest { doc: lone Document }{
  -- the request comes from a browser
  from in Browser
  -- the cookies being sent exist in the browser at the time of the request
  sentCookies in from.cookies.start
  -- every cookie sent must be scoped to the url of the request
  matchingScope[sentCookies, url]

  -- if there is no response, then no new document is opened
  some doc iff some response
  -- a new document is created to display the content of the response
  documents.end = documents.start + from -> doc
  -- the new document has the response as its contents
  content.end = content.start ++ doc -> response
  -- the new document has the host of the url as its domain
  domain.end = domain.start ++ doc -> url.host
  -- the document's source field is the url of the request
  some doc implies doc.src = url

  -- new cookies are stored by the browser
  cookies.end = cookies.start + from -> sentCookies
}

pred matchingScope[cookies: set Cookie, url: Url] {
  all c: cookies | url.host in c.domains
}

// Can we have two documents with different src but the same "document.domain"
// property at some point in time?
check {
  no disj d1, d2: Document | some t: Time |
    d1.src not in d2.src and d1.domain.t = d2.domain.t
}
