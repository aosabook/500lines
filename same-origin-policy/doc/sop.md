### Same Origin Policy

Before we can describe the SOP, the first thing we should do is to define what it means for two pages to have the *same* origin. Two URLs are considered to belong to the same origin if and only if they share the same hostname, protocol, and port:
```
pred sameOrigin[u1, u2 : http/URL] {
  u1.host = u2.host and u1.protocol = u2.protocol and u1.port = u2.port
}
```
The policy itself has two parts, constraining the ability of a script to (1) make DOM AP calls and (2) send HTTP requests. More specifically, the first part of the policy states that a script can only read and write to a DOM inside a frame that comes from the same origin as the script:
```
pred domSOP {
  all d : browser/ReadDOM + browser/WriteDOM | sameOrigin[d.frame.location, d.from.context]
}
```
The second part of the policy prevents a script from sending an HTTP request (i.e., XMLHTTPRequest) to a server unless the script belongs to the same origin as the destination URL: 
```
pred xmlhttpreqSOP {
  all x : browser/XMLHTTPReq | sameOrigin[x.url, x.from.context]
}
```
But why exactly are these restrictions necessary? What would be consequences if today's browsers hadn't enforced to the policy? In the next section, we will see how the Alloy Analyer can be used to answer these types of questions.
