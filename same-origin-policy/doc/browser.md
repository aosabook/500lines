Browser
-------

Building blocks
---------------

The `browser` module models a web browser. We'll build this module using
messages and http.

We begin by creating a `Browser` signature that extends `message/EndPoint`.
This means that the browser will own a set of resources and will also be able
to send or receive messages. It will be able, to, for example, send Http
requests and receive responses.

```
sig Browser extends message/EndPoint {}
```

A lot of things are happening in the browser. There's the set of html tags that
make up the web pages, scripts are running, and there's also the DOM
(Document Object Model) interface that allows scripts to read/write the content
of pages. Let's begin by creating a signature for the html tags:

```
abstract sig HTMLTag {}
```

We make it abstract because we intend to refine it later into relevant tags
... TODO: what happened with srctag and that stuff?

We model a script as an endpoint. That means it'll be able to send messages, in
particular, it will send XmlHttpRequests. This way of modeling migt be strange at
first sight, since a script doesn't really send any requests per-se, it's the
browser that sends it on behalf of the script. But it's best to overlook this
low-level detail since it makes the model simpler and more understandable:
In the end, it's the script that is sending the message, the fact that in
reallity this message is tunnelled through the browser is one of those low-level
details that are not relevant to the analysis and thus best to ignore.

```
abstract sig Script extends message/EndPoint {}
```

Scripts have a context under which they are executing, this is the URL of the
page that's executing the script. Note that it's not the url from where the
script is loaded. If on your personal website "me.com", you include a script tag
with `src` equal to `//someotherwebapp.com/script.js` that script is still
executing in the context of your personal website "me.com", despite the fact
that it's loaded from a different place. So let's add a `context` field to our
`Script` signature:

```
abstract sig Script extends message/EndPoint {
	context : http/URL
}
```

And we are now only missing the DOM, the interface that allows scripts to modify
documents. We'll consider DOM to be a resource.

TODO: explain why

```
abstract sig DOM extends message/Resource {}
```

No model of the same-origin policy would be complete without the XmlHttpRequest
object, which enables scripts to send requests to a web server. We'll model
XmlHttpRequets as a subset of HTTP Requests. We do this by declaring a
*subset signature**, using the keyword `in`:

```
sig XMLHTTPReq in http/HTTPReq {
}{
	from in Script
}
```

Since scripts can only send requests through the XmlHttpRequest object we
add a fact that says that for every request, if the sender is a script then
the request must be an XmlHttpRequest:

```
fact {
  all r : HTTPReq | r.from in Script implies r in XMLHTTPReq
}
```

We can execute our model by adding a `run {}` command.

TODO: insert figure.

We have most the building blocks there, but we still have to connect them
together. We'll do this through the notion of a "Frame."


Connecting the pieces together through frames
---------------------------------------------

A relevant notion in the context of a browser is that of a frame. Frames are
independent between each other, each frame has a location, a set of html tags
that make up the content frame and a DOM (Document Object Model). We could
make a distinction on whether a frame is loaded as an
iframe, a separate window or tab. But this distinction is not relevant to us,
the important thing is that frames are ment to be independent, and that their
interactions are subject to the same origin policy.

We model frames by adding a field to the `Browser` signature. So a browser will
have a set of frames:

```
sig Browser extends message/EndPoint {
	frames : set Frame
}
```

Frames have a location, a set of HTML tags, a dom, and a(n optional) script:

```
sig Frame {
	location : http/URL,
	tags : set HTMLTag,
	dom : DOM,
	script : lone Script
}
```

The script's context is determined by the location of the frame, so let's add
a signature fact:

```
abstract sig Frame {
	location : http/URL,
	tags : set HTMLTag,
	dom : DOM,
	script : lone Script
}{
	some script implies script.context = location
}
```

Now that we have frames, we can talk about DOM calls. A DOM call
is a message from a script to the browser. We'll add a `frame`
field to capture the target frame of the call:

```
abstract sig DomCall extends message/Msg {
	frame : Frame
}{
	from in Script
	to in Browser
}
```

We refine DOM calls into reads and writes:

```
sig ReadDom extends DomCall {
}{
	no args
	returns = frame.dom
}
sig WriteDom extends DomCall {
}{
	args in Dom
	one args
	no returns
}
```

We can now run what we have.
TODO: add some interesting run commands

Making our browser dynamic
--------------------------

As it is now, our browser model is static: it appears with frames loaded. We
want our browser to get frames as a result of sending messages to a webserver,
thus adding some dynanism to our model. So, we change `frames` in
`Browser` to connect a frame with a message:

```
abstract sig Browser extends message/EndPoint {
	frames :  Frame -> Msg
}{
	-- every frame must have been received as a response of some previous
    -- request
	all f : Frame, m : Msg |
 		f -> m in frames implies
			some r : (m + prevs[m]) & HTTPReq | 	
				f.dom in r.returns and
				f.location = r.url 

	-- initially does not own any resource
	no owns
}
```

We are now done with our browser model. We can execute it...
TODO: run cmds
