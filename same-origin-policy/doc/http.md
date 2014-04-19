Web
---

Http
----

Http messages
-------------

The `http` module models the Hypertext Transfer Protocol. We'll build this
module using the messages module explained before. That is, we'll
model http requests and responses as messages, and servers as endpoints.

But let's begin with HTTP messages. A critical piece of the protocol is
the notion of a URL (Uniform Resource Locator) which is composed of a
protocol, host, (an optional) port and (optional) path:

```
sig Protocol {}
sig Host {}
sig Port {}
sig Path {}

sig Url {
	protocol : Protocol,
	host : Host,
	-- port and path are optional
	port : lone Port,
	path : lone Path
}
```

We use the keyword `lone` to constrain `port` and `path` to be an option
(`lone` constraints the multiplicity of the set to at most one element --might
help to think of the word as *l*ess than or equal to *one*). We thus allow
URLs that have no port or path specified.

We can now execute our model to see what a URL would look like by adding a
`run {}` command. Moreover, we can use the Alloy visualizer to iterate through
the multiple instances found by the analyzer. Some of these instances will
have a port or a path, both or none.

TODO: insert figure

Now that we have URLs we are almost ready to model http requests. We are
missing the method of the request (i.e., "GET", "POST" and so on). So, let's
introduce a set of methods, which we declare as abstract since we intend to
classify it into more concrete types (like "GET"):

```
abstract sig Method {}
```

We now classify http request methods into "GET" and "POST":

```
one sig Get, Post extends Method {}
```

Note the use of the `one` keyword in the signature declaration;
`one sig Get, Post extends Method {}` declares `Get` and `Post` to be
signatures whose sets contains exactly one element: The set `Get` will contain
exactly one item and same for `Post`. Might help to think of this as
an application of the "Singleton" pattern; we restrict the amount of `Get` (and
`Post`) objects to exactly one. There's also the use of `extends` which hasn't
appeared before. A signature that extends another signature is said to be a
*subsignature* of the signature it extends, in this case, `Get` and `Post` are
subsignatures of `Method`. Subsignatures of a signature are mutually disjoint;
so `Get` and `Post` are disjoint sets. A subsignature also inherits the fields
of the signature it extends, though in this case `Method` has no fields so
there's nothing for `Get` and `Post` to inherit.

It is worth noting that in reality there are many more HTTP request methods like
"Head", "Put", and so on, but it's best to leave this out of the model since there
are not relevant to our analysis.

TODO: elaborate more on the rationale for this.

Now that we have URLs and HTTP request methods we can go ahead and declare
a set of HTTP requests, which we declare to extend message:

```
abstract sig HTTPReq extends message/Msg {
  url : URL,
  method : Method
}
```

So an `HttpRequest` will inherit `from`, `to` and `payload` from
message. An HTTP response is also a message, and we add a field `inResponseTo`
that connects the response to the request that originated the response:

```
abstract sig HTTPResp extends message/Msg {
  inResponseTo : HTTPReq
}
```

We can execute what we have so far of the http module.

TODO: add appropriate run cmd

Upon closer inspection we can see that something is a bit off. Though the
`inResponseTo` field is connecting a response to a request we never constrained
it to a request that happens **before** the HTTP response was issued. So we add
a constraint to `HttpResp` to
enforce that a `HttpResp` must be in response to a message that happened
before the `HttpResp` was issued. To do this, we extend the signature
declaration with a *signature fact*:

TODO: explain signature fact

```
abstract sig HTTPResp extends message/Msg {
  inResponseTo : HTTPReq
}{
  inResponseTo in prevs[this]
}
```

Finally,  we add a `fact` to avoid generating some spurious instances:

```
fact {
  -- no request goes unanswered and there's only one response per request
  all req : HTTPReq | one resp : HTTPResp | req in resp.inResponseTo
  -- response goes to whoever sent the request
  all resp : HTTPResp | resp.to = resp.inResponseTo.from
}
```

And we now have HTTP messages.

Http Server
-----------


There's one piece of the puzzle that we're missing; the mapping of URLs to
resources. So we declare a `Server` that is an end-point (can receive and send
messages) that maps URLs to resources:

```
sig Server extends message/EndPoint {	
	resMap : URL -> lone message/Resource
}
```

The expression `URL -> lone message/Resource` denotes a binary relation. So
`resMap` can be seen as a set of tuples (url, resource). The use of the `lone`
keyword before `message/Resource` imposes a multiplicity constraint: it says that
`resMap` is required to contain, for each resource `r`, `lone` tuples that
end with `r`. In other words, `resMap` is a partial function: Not every url is
mapped; which is what we want since we don't want **each** server to map
**all** urls.

TODO: explain this better

We still want though **all** urls to be mapped by **some** server (i.e., no lingering
urls). We can add this constraint as a `fact`:


```
fact {
  all u : URL | some s : Server | some s.resMap[u]
}
```

This says that for every URL there is some server that maps
it. `some s.resMap[u]` (which could equivalently been written `some s.resMap.u`)
is taking `s` and doing a relational join with
`resMap[u]` which itself is a relational join between `resMap` and the url `u`.
If the resulting set contains at least one element (`some`) then the fact holds.

We can now run what we have so far with `run {}`. A close inspection at the
instances generated show that two (or more) servers can map the same
url.

TODO: insert figure

Though not (necessarily) problematic, it is certainly non-intuitive to allow
instances where two servers map the same url. We can fix this by extending our
previous `fact` with the following statement:

```
  no disj s1, s2 : Server | some u : URL | some s1.resMap[u] and some s2.resMap[u]
```

Finally, since http is a client/server protocol
we enforce an http request to be sent from a client to a server by extending the
signature declaration with a signature fact:

```
abstract sig HTTPReq extends message/Msg {
	url : URL,
	method : Method
}{
	from not in Server
	to in Server
}
```

and a response to be sent from a server to a client:

```
abstract sig HTTPResp extends message/Msg {
	inResponseTo : HTTPReq
}{
	from in Server
	to not in Server
	inResponseTo in prevs[this]
}
```

We now have HTTP requests, responses and servers that map URLs to resources.