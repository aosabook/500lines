# The Same Origin Policy

## Introduction

The same-origin policy (SOP) is part of the security mechanism of every modern browser. It controls when scripts running in a browser can communicate with one another (roughly, when they originate from the same website). First introduced in Netscape Navigator, the SOP now plays a critical role in the security of web applications; without it, it would be far easier for a malicious hacker to peruse your private photos on Facebook, or empty the balance on your bank account.

But the SOP is far from perfect. At times, it is too restrictive; there are cases (such as mashups) in which scripts from different origins should be able to share a resource but cannot. At other times, it is not restrictive enough, leaving corner cases that can be exploited using common attacks such as cross-site request forgery (CSRF). Furthermore, the design of the SOP has evolved organically over the years and puzzles many developers.

## Modeling with Alloy

This chapter is somewhat different from others in this book. Instead of building a working implementation, our goal is to construct an executable _model_ that serves as a simple yet precise description of the SOP. Like an implementation, the model can be executed to explore dynamic behaviors of the system; but unlike an implementation, the model omits low-level details that may get in the way of understanding the essential concepts.

To construct this model, we use _Alloy_, a language for modeling and analyzing software design. An Alloy model cannot be executed in the traditional sense of program execution. Instead, a model can be (1) _simulated_ to produce an _instance_, which represents a valid configuration or a trace, and (2) _checked_ to see whether the model satisfies a desired _property_.

The approach we take might be called “agile modeling” because of its similarities to agile programming. We work incrementally, assembling the model bit by bit. Our evolving model is at every point something that can be executed. We formulate and run tests as we go, so that by the end we have not only the model itself but also a collection of properties that it satisfies. 

Despite these similarities, agile modeling differs from agile programming in one key respect. Although we'll be running tests, we actually won't be writing any. Alloy's analyzer generates test cases automatically, and all that needs to be provided is the property to be checked. Needless to say, this saves a lot of trouble (and text). The analyzer actually executes all possible test cases up to a certain size (called a _scope_); this typically means generating all starting states with at most some number of objects, and then choosing operations and arguments to apply up to some number of steps. Because so many tests are executed (typically billions), and because all "shapes" that a state can take are covered (albeit within the scope), this analysis tends to expose bugs more effectively than conventional testing. 

## Simplifications

Because the SOP operates in the context of browsers, servers, the HTTP protocol, and so on, a complete description would be overwhelming. So our model (like all models) abstracts away irrelevant aspects, such how network packets are structured and routed. But it also simplifies some relevant aspects, which means that the model cannot fully account for all possible security vulnerabilities.

For example, we treat HTTP requests like remote procedure calls, as if they occur at a single point in time, ignoring the fact that responses to requests might come out of order. We also assume that DNS (the domain name service) is static, so we cannot consider attacks in which a DNS binding changes during an interaction. In principle, though, it would be possible to extend our model to cover all these aspects, although it's in the very nature of security analysis that no model (even it represents the entire codebase) can be guaranteed complete.

## The HTTP Protocol

The first step in building an Alloy model is to declare some sets of objects. Let's start with resources:

```
sig Resource {}
```

The keyword “sig” identifies this as an Alloy signature declaration. This introduces a set of resource objects; think of these, just like the objects of a class with no instance variables, as blobs that have identity but no contents. Resources are named by URLs (uniform record locators):

```
sig URL {
  protocol: Protocol,
  host: Domain,
  port: lone Port,
  path: Path
}
sig Protocol, Domain, Port, Path {}
```
Here we have five signature declarations, introducing sets for URLs and each of the basic types of objects they comprise. Within the URL declaration, we have four fields. Fields are like instance variables in a class; if `u` is a URL, for example, then `u.protocol` would represent the protocol of that URL (just like dot in Java). But in fact, as we'll see later, these fields are relations. You can think of each one as if it were a two column database table. Thus `protocol` is a table with a column containing URLs and a column containing Protocols. And the innocuous looking dot operator is in fact a rather general kind of relational join, so that you could also write `protocol.p` for all the URLs with a protocol `p` -- but more on that later.

Note that domains and paths, unlike URLs, are treated as if they have no structure -- a simplification. The keyword `lone` (which can be read "less than or equal to one") says that each URL has at most one port. The path is the string that follows the host name in the URL, and which (for a simple static server) corresponds to the filepath of the resource; we're assuming that it's always present, but can be an empty path.

Now we need some clients and servers:

```
abstract sig Endpoint {}
abstract sig Client extends Endpoint {}
abstract sig Server extends Endpoint {
  resources: Path -> Resource
  }
```

The `extends` keyword introduces a subset, so the set `Client` of all clients, for example, is a subset of the set `Endpoint` of all endpoints. Extensions are disjoint, so no endpoint is both a client and a server. The `abstract` keyword says that all extensions of a signature exhaust it, so its occurrence in the declaration of `Endpoint`, for example, says that every endpoint must belong to one of the subsets (at this point, `Client` and `Server`). For a server `s`, the expression `s.resources` will denote a map from paths to resources (hence the arrow in the declaration). But as before, remember that each field is actually a relation that includes the owning signature as a first column, so this field represents a three-column relation on `Server`, `Path` and `Resource`.

This is a very simple model of a server: it has a static mapping of paths to resources. In general, the mapping is dynamic, but that won't matter for our analysis.

To map a URL to a server, we'll need to model DNS. So let's introduce a set `DNS` of domain name servers, each with a mapping from domains to servers:

```
one sig DNS { 
	map: Domain -> Server
}
```

The keyword `one` means that (for simplicity) we're going to restrict to exactly one domain name server, so that `DNS.map` will be the mapping used everywhere. Again, as with serving resources, this could be dynamic (and in fact there are known security attacks that rely on changing DNS bindings during an interaction) but we're simplifying.

We'll need cookies, so let's declare them:

```
sig Cookie {
  domains: set Domain,
}
```

Each cookie is scoped with a set of domains; this captures the fact that a cookie can apply to "*.mit.edu", which would include all domains with the suffix "mit.edu".

Finally, we can put this all together and model the requests:

```
abstract sig HttpRequest extends Call {
  url: URL,
  cookies: set Cookie,
  body: lone Resource,
  setCookies: set Cookie,
  response: lone Resource,
}{
  from in Client
  to in DNS.map[url.host]
  all c: setCookies | url.host in c.domains
  response in to.resources[url.path]
}

abstract sig GetRequest, PostRequest extends HttpRequest {}
```

We're modeling an HTTP request and response in a single object; the `url`, `cookies` and `body` are sent by the client, and the `setCookies` and `response` are sent back by the server. Don't be confused by the two meanings of the word "set" in the declaration of `setCookies`. The field name (following W3C terminology) uses "set" to mean that the cookies that are returned are installed in the browser, "setting" their values; on the right hand side, the word "set" means that any number of cookies (including zero) may be returned.

When writing the `HTTPRequest` signature, we found that it contained generic features of calls, namely that they are from and to particular things. So we actually wrote a little Alloy module that declares the `Call` signature, and to use it here we need to import it:

```
open call[Endpoint]
```

It's a polymorphic module, so it's instantiated with `Endpoint`, the set of things calls are from and to.

Following the field declarations in `HTTPRequest` is a collection of constraints. Each of these constraints applies to all members of the set of HTTP requests. The constraints say: that each request comes from a client; that each request is sent to one of the servers specified by the URL host under the DNS mapping; that for all cookies returned, the host name in the server's URL must be included in the cookie's domains; and that the response is obtained by looking up, in the server's resource mapping, the path in the URL of the request.

[what does allowing cookie to include domains from other servers do?]

Now do some runs...
