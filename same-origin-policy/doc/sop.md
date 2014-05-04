## Same Origin Policy

Before we can state the SOP, the first thing we should do is to define what it means for two pages to have the *same* origin. Two URLs refer to the same origin if and only if they share the same hostname, protocol, and port:
```
pred sameOrigin[u1, u2 : URL] {
  u1.host = u2.host and u1.protocol = u2.protocol and u1.port = u2.port
}
```
The SOP itself has two parts, restricting the ability of a script to (1) make DOM AP calls and (2) send HTTP requests. The first part of the policy states that a script can only read from and write to a document that comes from the same origin as the script:
```
pred domSOP { all c: ReadDOM + WriteDOM | sameOrigin[c.doc.src, c.from.context.src] }
```
A scenario such as ["Script Scenario #1"] is not possible under `domSOP`, since `Script` is not allowed to invoke `ReadDOM` on a document from a different origin.

The second part of the policy says that a script cannot send an HTTP request to a server unless its context has the same origin as the target URL---effectively preventing scenarios such as ["Script Scenario #2"].
```
pred xmlHttpReqSop { all x: XmlHttpRequest | sameOrigin[x.url, x.from.context.src] }

```
As we can see, the SOP is designed to prevent the two types of vulnerabilities that could arise from actions of a malicious script; without it, the web would be a much more dangerous place than it is today.

It turns out, however, that the SOP can be *too* restrictive. For example, sometimes you *do* want to allow communication between two documents of different origins. By the above definition of an origin, a script from `foo.example.com` would not be able to read the content of `bar.example.com`, or send a HTTP request to `www.example.com`, because these are all considered distinct hosts. 

In order to allow some form of cross-origin communication when necessary, browsers implemented a variety of mechanisms for relaxing the SOP. Some of these are more well-thought-out than others, and some have serious flaws that, when badly used, could negate the security benefits of the SOP. In the following sections, we will describe the most common of these mechanisms, and discuss their potential security pitfalls.
