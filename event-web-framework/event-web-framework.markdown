title: An Event-Driven Web Framework
author: Leo Zovic
<markdown>
_Leo (better known online as inaimathi) is a recovering Graphic Designer who has professionally written Scheme, Common Lisp, Erlang, Javascript, Haskell, Clojure, Go, Python, PHP and C. He currently blogs about programming, plays board games and works at a Ruby-based startup in Toronto, Ontario._
</markdown>

In 2013, I decided to write a [web-based game prototyping tool](https://github.com/Inaimathi/deal) for card and board games called _House_. In these types of games, it is common for one player to wait for another player to make a move; however, when the other player finally does take action, we would like for the waiting player to be notified of the move quickly thereafter.

This is a problem that turns out to be more complicated than it first seems. In this chapter, we'll explore the issues with using HTTP to build this sort of interaction, and then we'll build a _web framework_ in Common Lisp that allows us to solve similar problems in the future.

## The Basics of HTTP Servers

At the simplest level, an HTTP exchange is a single request followed by a single response. A _client_ sends a request, which includes a resource identifier, an HTTP version tag, some headers and some parameters. The _server_ parses that request, figures out what to do about it, and sends a response which includes the same HTTP version tag, a response code, some headers and a response body.

Notice that, in this description, the server responds to a request from a specific client. In our case, we want each player to be updated about _any_ moves as soon as they happen, rather than only getting notifications when their own move is made. This means we need the server to _push_ messages to clients without first receiving a request for the information.[^polling]

[^polling]: One solution to this problem is to force the clients to _poll_ the server. That is, each client would periodically send the server a request asking if anything has changed. This can work for simple applications, but in this chapter we're going to focus on the solutions available to you when this model stops working.

There are several standard approaches to enabling server push over HTTP.

### Comet/Long Poll

The "long poll" technique has the client send the server a new request as soon as it receives a response. Instead of fulfilling that request right away, the server waits on a subsequent event to respond. This is a bit of a semantic distinction, since the client is still taking action on every update.

### Server-Sent Events (SSE)

Server-sent events require that the client initiates a connection and then keeps it open. The server periodically writes new data to the connection without closing it, and the client interprets incoming new messages as they arrive rather than waiting for the response connection to terminate. This is a bit more efficient than the Comet/long poll approach because each message doesn't have to incur the overhead of new HTTP headers.

### WebSockets

WebSockets are a communication protocol built on top of HTTP. The server and client open up an HTTP conversation, then perform a handshake and protocol escalation. The end result is that they're still communicating over TCP/IP, but they're not using HTTP to do it at all. The advantage this has over SSEs is that you can customize the protocol for efficiency.

### Long-Lived Connections

These three approaches are quite different from one another, but they all share an important characteristic: they all depend on long-lived connections. Long polling depends on the server keeping requests around until new data is available, SSEs keep an open stream between client and server to which data is periodically written, and WebSockets change the protocol a particular connection is using, but leave it open.

To see why this might cause problems for your average HTTP server, let's consider how the underlying implementation might work.

### Traditional HTTP Server Architecture
\label{sec.eventsweb.serverarch}

A single HTTP server processes many requests concurrently. Historically, many HTTP servers have used a _thread-per-request_ architecture. That is, for each incoming request, the server creates a thread to do the work necessary to respond.

Since each of these connections is intended to be short-lived, we don't need many threads executing in parallel to handle them all. This model also simplifies the _implementation_ of the server by enabling the server programmer to write code as if there were only one connection being handled at any given time. It also gives us the freedom to clean up failed or "zombie" connections and their associated resources by killing the corresponding thread and letting the garbage collector do its job.

The key observation is that an HTTP server hosting a "traditional" web application that has $N$ concurrent users might only need to handle a very small fraction of $N$ requests _in parallel_ to succeed. For the type of interactive application that we are trying to build, $N$ users will almost certainly require the application to maintain at least $N$ connections in parallel, at once.

The consequence of keeping long-lived connections around is that we'll need either:

- A platform where threads are "cheap" enough that we can use large numbers of them at once.
- A server architecture that can handle many connections with a single thread.

There are programming environments such as [Racket](http://racket-lang.org/), [Erlang](http://www.erlang.org/), and [Haskell](http://hackage.haskell.org/package/base-4.7.0.1/docs/Control-Concurrent.html) that provide thread-like constructs that are "lightweight" enough to consider the first option. This approach requires the programmer to explicitly deal with synchronization issues, which are going to be much more prevalent in a system where connections are open for a long time and likely all competing for similar resources. Specifically, if we have some sort of central data shared by several users simultaneously, we will need to coordinate reads and writes of that data in some way.

If we don't have cheap threads at our disposal or we are unwilling to work with explicit synchronization, we must consider having a single thread handle many connections.[^mn] In this model, our single thread is going to be handling tiny "slices" of many requests all at once, switching between them as efficiently as it possibly can. This system architecture pattern is most commonly referred to as _event-driven_ or _event-based_.[^eventbased]

[^mn]: We could consider a more general system that handles $N$ concurrent users with $M$ threads for some configurable value of $M$; in this model, the $N$ connections are said to be _multiplexed_ across the $M$ threads. In this chapter, we are going to focus on writing a program where $M$ is fixed at 1; however, the lessons learned here should be partially applicable to the more general model.

[^eventbased]: This nomenclature is a bit confusing, and has its origin in early operating-systems research. It refers to how communication is done between multiple concurrent processes. In a thread-based system, communication is done through a synchronized resource such as shared memory. In an event-based system, processes generally communicate through a queue where they post items that describe what they have done or what they want done, which is maintained by our single thread of execution. Since these items generally describe desired or past actions, they are referred to as 'events'.

Since we are only managing a single thread, we don't have to worry as much about protecting shared resources from simultaneous access. However, we do have a unique problem of our own in this model. Since our single thread is working on all in-flight requests at once, we must make sure that it __never blocks__. Blocking on any connection blocks the entire server from making progress on any other request. We have to be able to move on to another client if the current one can't be serviced further, and we need to be able to do so in a manner that doesn't throw out the work done so far.[^crawler]

[^crawler]: See \aosachapref{s:crawler} for another take on this problem. 

While it is uncommon for a programmer to explicitly tell a thread to stop working, many common operations carry a risk of blocking. Because threads are so prevalent, and reasoning about asynchronicity is a heavy burden on the programmer, many languages and their frameworks assume that blocking on I/O is a desirable property. This makes it very easy to block somewhere _by accident_. Luckily, Common Lisp does provide us with a minimal set of asynchronous I/O primitives which we can build on top of.

### Architectural Decisions

Now that we've studied the background of this problem, we've arrived at the point where we need to make informed decisions about _what_ we are building.

At the time I started thinking about this project, Common Lisp didn't have a complete green-thread implementation, and the [standard portable threading library](http://common-lisp.net/project/bordeaux-threads/) doesn't qualify as "really REALLY cheap". The options amounted to either picking a different language, or building an event-driven web server for my purpose. I chose the latter.

In addition to the server architecture, we also need to choose which of the three server-push approaches to use. The use-case we are considering (an interactive multiplayer board game) requires frequent updates to each client, but relatively sparse requests _from_ each client, which fits the SSE approach to pushing updates, so we'll go with this.

Now that we've motivated our architectural decision and decided on a mechanism for simulating bidirectional communication between clients and server, let's get started on building our web framework. We'll start by building a relatively "dumb" server first, and then we'll extend it into a web-application framework that lets us focus on _what_ our heavily-interactive program needs to do, and not _how_ it is doing it.

## Building an Event-Driven Web Server

Most programs that use a single process to manage concurrent streams of work
use a pattern called an _event loop_. Let's look at what an event loop for our
web server might look like.

### The Event Loop

Our event loop needs to:

- listen for incoming connections;
- handle all new handshakes or incoming data on existing connections;
- clean up dangling sockets that are unexpectedly killed (e.g. by an interrupt)

```lisp
(defmethod start ((port integer))
  (let ((server (socket-listen
		 usocket:*wildcard-host* port
		 :reuse-address t
		 :element-type 'octet))
	(conns (make-hash-table)))
    (unwind-protect
	 (loop (loop for ready
		  in (wait-for-input
		      (cons server (alexandria:hash-table-keys conns))
		      :ready-only t)
		  do (process-ready ready conns)))
      (loop for c being the hash-keys of conns
	 do (loop while (socket-close c)))
      (loop while (socket-close server)))))
```

If you haven't written a Common Lisp program before, this code block requires some explanation. What we have written here is a _method definition_. While Lisp is popularly known as a functional language, it also has its own system for object-oriented programming called "The Common Lisp Object System", which is usually abbreviated as "CLOS".[^CLOSpronounce]

[^CLOSpronounce]: Pronounced "kloss", "see-loss" or "see-lows", depending on who you talk to.

### CLOS and Generic Functions

In CLOS, instead of focusing on classes and methods, we write [_generic functions_](http://www.gigamonkeys.com/book/object-reorientation-generic-functions.html) that are implemented as collections of _methods_. In this model, methods don't _belong to_ classes, they _specialize on_ types.[^juliachap] The `start` method we just wrote is a unary method where the argument `port` is _specialized on_ the type `integer`. This means that we could have several implementations of `start` where `port` varies in type, and the runtime will select which implementation to use depending on the type of `port` when `start` is called.

[^juliachap]: The Julia programming language takes a similar approach to object-oriented programming; you can learn more about it in \aosachapref{s:static-analysis}.

More generally, methods can specialize on more than one argument. When a `method` is called, the runtime:

- dispatches on the type of its arguments to figure out which method body should be run, and
- runs the appropriate function.

### Processing Sockets

We'll see another generic function at work in `process-ready`, which was called earlier from our event loop. It processes a ready socket with one of two methods, depending on the type of socket we are handling.

The two types we're concerned with are the `stream-usocket`, which represents a client socket that will make a request and expect to be sent some data back, and the `stream-server-usocket`, which represents our local TCP listener that will have new client connections for us to deal with.

If a `stream-server-socket` is `ready`, that means there's a new client socket waiting to start a conversation. We call `socket-accept` to accept the connection, and then put the result in our connection table so that our event loop can begin processing it with the others.

```lisp
(defmethod process-ready ((ready stream-server-usocket) (conns hash-table))
  (setf (gethash (socket-accept ready :element-type 'octet) conns) nil))
```

When a `stream-usocket` is `ready`, that means that it has some bytes ready for us to read. (It's also possible that the other party has terminated the connection.)

```lisp
(defmethod process-ready ((ready stream-usocket) (conns hash-table))
  (let ((buf (or (gethash ready conns)
		 (setf (gethash ready conns)
		       (make-instance 'buffer :bi-stream (flex-stream ready))))))
    (if (eq :eof (buffer! buf))
	(ignore-errors
	  (remhash ready conns)
	  (socket-close ready))
	(let ((too-big?
	       (> (total-buffered buf)
		  +max-request-size+))
	      (too-old?
	       (> (- (get-universal-time) (started buf))
		  +max-request-age+))
	      (too-needy?
	       (> (tries buf)
		  +max-buffer-tries+)))
	  (cond (too-big?
		 (error! +413+ ready)
		 (remhash ready conns))
		((or too-old? too-needy?)
		 (error! +400+ ready)
		 (remhash ready conns))
		((and (request buf) (zerop (expecting buf)))
		 (remhash ready conns)
		 (when (contents buf)
		   (setf (parameters (request buf))
			 (nconc (parse buf) (parameters (request buf)))))
		 (handler-case
		     (handle-request ready (request buf))
		   (http-assertion-error () (error! +400+ ready))
		   ((and (not warning)
		     (not simple-error)) (e)
		     (error! +500+ ready e))))
		(t
		 (setf (contents buf) nil)))))))
```

This is more involved than the first case. We:

1. Get the buffer associated with this socket, or create it if it doesn't exist yet;
2. Read output into that buffer, which happens in the call to `buffer!`;
3. If that read got us an `:eof`, the other side hung up, so we discard the socket _and_ its buffer;
4. Otherwise, we check if the buffer is one of `complete?`, `too-big?`, `too-old?` or `too-needy?`. If so, we remove it from the connections table and return the appropriate HTTP response.

This is the first time we're seeing I/O in our event loop. In our discussion in \aosasecref{sec.eventsweb.serverarch}, we mentioned that we have to be very careful about I/O in an event-driven system, because we could accidentally block our single thread. So, what do we do here to ensure that this doesn't happen? We have to explore our implementation of `buffer!` to find out exactly how this works.

### Processing Connections Without Blocking

The basis of our approach to processing connections without blocking is the library function [`read-char-no-hang`](http://clhs.lisp.se/Body/f_rd_c_1.htm), which immediately returns `nil` when called on a stream that has no available data. Where there is data to be read, we use a buffer to store intermediate input for this connection.

```lisp
(defmethod buffer! ((buffer buffer))
  (handler-case
      (let ((stream (bi-stream buffer)))
    	(incf (tries buffer))
    	(loop for char = (read-char-no-hang stream) until (null char)
    	   do (push char (contents buffer))
    	   do (incf (total-buffered buffer))
    	   when (request buffer) do (decf (expecting buffer))
    	   when (line-terminated? (contents buffer))
    	   do (multiple-value-bind (parsed expecting) (parse buffer)
    		(setf (request buffer) parsed
    		      (expecting buffer) expecting)
    		(return char))
    	   when (> (total-buffered buffer) +max-request-size+) return char
    	   finally (return char)))
    (error () :eof)))
```

When `buffer!` is called on a `buffer`, it:

- increments the `tries` count, so that we can evict "needy" buffers in `process-ready`;
- loops to read characters from the input stream, and
- returns the last character it read if it has read all of the available input.

It also tracks any `\r\n\r\n` sequences so that we can later detect complete requests. Finally, if any error results, it returns an `:eof` to signal that `process-ready` should discard this connection.

The `buffer` type is a CLOS [_class_](http://www.gigamonkeys.com/book/object-reorientation-classes.html). Classes in CLOS let us define a type with fields called `slots`. We don't see the behaviours associated with `buffer` on the class definition, because (as we've already learned), we do that using generic functions like `buffer!`.

`defclass` does allow us to specify getters/setters (`reader`s/`accessor`s), and slot initializers; `:initform` specifies a default value, while `:initarg` identifies a hook that the caller of \newline `make-instance` can use to provide a default value.

```lisp
(defclass buffer ()
  ((tries :accessor tries :initform 0)
   (contents :accessor contents :initform nil)
   (bi-stream :reader bi-stream :initarg :bi-stream)
   (total-buffered :accessor total-buffered :initform 0)
   (started :reader started :initform (get-universal-time))
   (request :accessor request :initform nil)
   (expecting :accessor expecting :initform 0)))
```

Our `buffer` class has seven slots:

- `tries`, which keeps count of how many times we've tried reading into this buffer
- `contents`, which contains what we've read so far
- `bi-stream`, which a hack around some of those Common Lisp-specific, non-blocking-I/O annoyances I mentioned earlier
- `total-buffered`, which is a count of chars we've read so far
- `started`, which is a timestamp that tells us when we created this buffer
- `request`, which will eventually contain the request we construct from buffered data
- `expecting`, which will signal how many more chars we're expecting (if any) after we buffer the request headers

### Interpreting Requests
\label{sec.eventsweb.handlerfunc}
Now that we've seen how we incrementally assemble full requests from bits of data that are pooled into our buffers, what happens when we have a full request ready for handling? This happens in the method `handle-request`.

```lisp
(defmethod handle-request ((socket usocket) (req request))
  (aif (lookup (resource req) *handlers*)
       (funcall it socket (parameters req))
       (error! +404+ socket)))
```

This method adds another layer of error handling so that if the request is old, big, or needy, we can send a `400` response to indicate that the client provided us with some bad or slow data. However, if any _other_ error happens here, it's because the programer made a mistake defining a _handler_, which should be treated as a `500` error. This will inform the client that something went wrong on the server as a result of their legitimate request.

If the request is well-formed, we do the tiny and obvious job of looking up the requested resource in the `*handlers*` table. If we find one, we `funcall` `it`, passing along the client `socket` as well as the parsed request parameters. If there's no matching handler in the `*handlers*` table, we instead send along a `404` error. The handler system will be part of our full-fledged _web framework_, which we'll discuss in a later section.

We still haven't seen how requests are parsed and interpreted from one of our buffers, though. Let's look at that next:

```lisp
(defmethod parse ((buf buffer))
  (let ((str (coerce (reverse (contents buf)) 'string)))
    (if (request buf)
	    (parse-params str)
	    (parse str))))
```

This high-level method delegates to a specialization of `parse` that works with plain strings, or to `parse-params` that interprets the buffer contents as HTTP parameters. These are called depending on how much of the request we've already processed; the final `parse` happens when we already have a partial `request` saved in the `buffer`, at which point we're only looking to parse the request body.


```lisp
(defmethod parse ((str string))
  (let ((lines (split "\\r?\\n" str)))
    (destructuring-bind (req-type path http-version) (split " " (pop lines))
      (declare (ignore req-type))
      (assert-http (string= http-version "HTTP/1.1"))
      (let* ((path-pieces (split "\\?" path))
	     (resource (first path-pieces))
	     (parameters (second path-pieces))
	     (req (make-instance 'request :resource resource)))
	(loop
	   for header = (pop lines)
	   for (name value) = (split ": " header)
	   until (null name)
	   do (push (cons (->keyword name) value) (headers req)))
	(setf (parameters req) (parse-params parameters))
	req))))

(defmethod parse-params ((params null)) nil)

(defmethod parse-params ((params string))
  (loop for pair in (split "&" params)
     for (name val) = (split "=" pair)
     collect (cons (->keyword name) (or val ""))))
```

In the `parse` method specializing on `string`, we transform the content into usable pieces. We do so on strings instead of working directly with buffers because this makes it easier to test the actual parsing code in an environment like an interpreter or REPL.

The parsing process is:

1. Split on `"\\r?\\n"`.
2. Split the first line of that on `" "` to get the request type (`POST`, `GET`, etc)/URI path/http-version.
3. Assert that we're dealing with an `HTTP/1.1` request.
4. Split the URI path on `"?"`, which gives us plain resource separate from any `GET` parameters.
5. Make a new `request` instance with the resource in place.
6. Populate that `request` instance with each split header line.
7. Set that `request`s parameters to the result of parsing our `GET` parameters.

As you might expect by now, `request` is an instance of a CLOS class:

```lisp
	(defclass request ()
	  ((resource :accessor resource :initarg :resource)
	   (headers :accessor headers :initarg :headers :initform nil)
	   (parameters :accessor parameters :initarg :parameters :initform nil)))
```

We've now seen how our clients can send requests and have them interpreted and handled by our server. The last thing we have to implement as part of our core server interface is the capability to write responses back to the client.

### Rendering Responses

Before we discuss rendering responses, we have to consider that there are two kinds of responses that we may be returning to our clients. The first is a "normal" HTTP response, complete with HTTP headers and body. We represent these kinds of responses with instances of the `response` class:

```lisp
(defclass response ()
  ((content-type
    :accessor content-type :initform "text/html" :initarg :content-type)
   (charset
    :accessor charset :initform "utf-8")
   (response-code
    :accessor response-code :initform "200 OK" :initarg :response-code)
   (keep-alive?
    :accessor keep-alive? :initform nil :initarg :keep-alive?)
   (body
    :accessor body :initform nil :initarg :body)))
```

The second is an [SSE message](http://www.w3.org/TR/eventsource/), which we will use to send an incremental update to our clients.

```lisp
(defclass sse ()
  ((id :reader id :initarg :id :initform nil)
   (event :reader event :initarg :event :initform nil)
   (retry :reader retry :initarg :retry :initform nil)
   (data :reader data :initarg :data)))
```

We'll send an HTTP response whenever we receive a full HTTP request; however, how do we know when and where to send SSE messages without an originating client request?

A simple solution is to register _channels_[^defparameter], to which we'll subscribe `socket`s as necessary.

```lisp
(defparameter *channels* (make-hash-table))

(defmethod subscribe! ((channel symbol) (sock usocket))
  (push sock (gethash channel *channels*))
  nil)
```

[^defparameter]: We're incidentally introducing some new syntax here. This is our way of declaring a mutable variable. It has the form `(defparameter <name> <value> <optional docstring>)`.

We can then `publish!` notifications to said channels as soon as they become available.

```lisp
(defmethod publish! ((channel symbol) (message string))
  (awhen (gethash channel *channels*)
	 (setf (gethash channel *channels*)
	       (loop with msg = (make-instance 'sse :data message)
		  for sock in it
		  when (ignore-errors
			 (write! msg sock)
			 (force-output (socket-stream sock))
			 sock)
		  collect it))))
```

In `publish!`, we call `write!` to actually write an `sse` to a socket. We'll also need a specialization of `write!` on `response`s to write full HTTP responses as well. Let's handle the HTTP case first.

```lisp
(defmethod write! ((res response) (socket usocket))
  (handler-case
      (with-timeout (.2)
	(let ((stream (flex-stream socket)))
	  (flet ((write-ln (&rest sequences)
		   (mapc (lambda (seq) (write-sequence seq stream)) sequences)
		   (crlf stream)))
	    (write-ln "HTTP/1.1 " (response-code res))
	    (write-ln
	     "Content-Type: " (content-type res) "; charset=" (charset res))
	    (write-ln "Cache-Control: no-cache, no-store, must-revalidate")
	    (when (keep-alive? res)
	      (write-ln "Connection: keep-alive")
	      (write-ln "Expires: Thu, 01 Jan 1970 00:00:01 GMT"))
	    (awhen (body res)
	      (write-ln "Content-Length: " (write-to-string (length it)))
	      (crlf stream)
	      (write-ln it))
	    (values))))
    (trivial-timeout:timeout-error ()
      (values))))
```

This version of `write!` takes a `response` and a `usocket` named `sock`, and writes content to a stream provided by `sock`. We locally define the function `write-ln` which takes some number of sequences, and writes them out to the stream followed by a `crlf`. This is for readability; we could instead have called `write-sequence`/`crlf` directly. 

Note that we're doing the "Must not block" thing again. While writes are likely to be buffered and are at lower risk of blocking than reads, we still don't want our server to grind to a halt if something goes wrong here. If the write takes more than 0.2 seconds[^timeout], we just move on (throwing out the current socket) rather than waiting any longer.

[^timeout]: `with-timeout` has different implementations on different Lisps. In some environments, it may create another thread or process to monitor the one that invoked it. While we'd only be creating at most one of these at a time, it is a relatively heavyweight operation to be performing per-write. We might want to consider an alternative approach in those environments.

Writing an `SSE` out is conceptually similar to writing out a `response`:

```lisp
(defmethod write! ((res sse) (socket usocket))
  (let ((stream (flex-stream socket)))
    (handler-case
    (with-timeout (.2)
      (format
       stream "~@[id: ~a~%~]~@[event: ~a~%~]~@[retry: ~a~%~]data: ~a~%~%"
       (id res) (event res) (retry res) (data res)))
      (trivial-timeout:timeout-error ()
        (values)))))
```

This is simpler than working with full HTTP responses since the SSE message standard doesn't specify `CRLF` line-endings, so we can get away with a single `format` call. The `~@[`...`~]` blocks are _conditional directives_, which allow us to gracefully handle `nil` slots. For example, if `(id res)` is non-nil, we'll output `id: <the id here> `, otherwise we will ignore the directive entirely. The payload of our incremental update `data` is the only required slot of `sse`, so we can include it without worrying about it being `nil`. And again, we're not waiting around for _too_ long. After 0.2 seconds, we'll time out and move on to the next thing if the write hasn't completed by then.

### Error Responses

Our treatment of the request/response cycle so far hasn't covered what happens when something goes wrong. Specifically, we used the `error!` function in `handle-request` and `process-ready` without describing what it does.

```lisp
(define-condition http-assertion-error (error)
  ((assertion :initarg :assertion :initform nil :reader assertion))
  (:report (lambda (condition stream)
	     (format stream "Failed assertions '~s'"
		     (assertion condition)))))
```

`define-condition` creates new error classes in Common Lisp. In this case, we are defining an HTTP assertion error, and stating that it will specifically need to know the actual assertion it's acting on, and a way to output itself to a stream. In other languages, you'd call this a method. Here, it's a function that happens to be the slot value of a class.

How do we represent errors to the client? Let's define the `4xx` and `5xx`-class HTTP errors that we'll be using often:

```lisp
(defparameter +404+
  (make-instance
   'response :response-code "404 Not Found"
   :content-type "text/plain"
   :body "Resource not found..."))

(defparameter +400+
  (make-instance
   'response :response-code "400 Bad Request"
   :content-type "text/plain"
   :body "Malformed, or slow HTTP request..."))

(defparameter +413+
  (make-instance
   'response :response-code "413 Request Entity Too Large"
   :content-type "text/plain"
   :body "Your request is too long..."))

(defparameter +500+
  (make-instance
   'response :response-code "500 Internal Server Error"
   :content-type "text/plain"
   :body "Something went wrong on our end..."))
```

Now we can see what `error!` does:

```lisp
(defmethod error! ((err response) (sock usocket) &optional instance)
  (declare (ignorable instance))
  (ignore-errors
    (write! err sock)
    (socket-close sock)))
```

It takes an error response and a socket, writes the response to the socket and closes it (ignoring errors, in case the other end has already disconnected). The `instance` argument here is for logging/debugging purposes.

And with that, we have an event-driven web server that can respond to HTTP requests or send SSE messages, complete with error handling!


## Extending the Server Into a Web Framework

We have now built a reasonably functional web server that will move requests, responses, and messages to and from clients. The actual work of any web application hosted by this server is done by delegating to handler functions, which were introduced in \aosasecref{sec.eventsweb.handlerfunc} but left underspecified.

The interface between our server and the hosted application is an important one, because it dictates how easily application programmers can work with our infrastructure. Ideally, our handler interface would map parameters from a request to a function that does the real work:

```lisp
(define-handler (source :is-stream? nil) (room)
  (subscribe! (intern room :keyword) sock))

(define-handler (send-message) (room name message)
  (publish! (intern room :keyword)
	    (encode-json-to-string
	     `((:name . ,name) (:message . ,message)))))

(define-handler (index) ()
  (with-html-output-to-string (s nil :prologue t :indent t)
    (:html
     (:head (:script
	     :type "text/javascript"
	     :src "/static/js/interface.js"))
     (:body (:div :id "messages")
	    (:textarea :id "input")
	    (:button :id "send" "Send")))))
```

One of the concerns I had in mind when writing House was that, like any application open to the greater internet, it would be processing requests from untrusted clients. It would be nice to be able to say specifically what _type_ of data each request should contain by providing a small _schema_ that describes the data. Our previous list of handlers would then look like this:

```lisp
(defun len-between (min thing max)
  (>= max (length thing) min))

(define-handler (source :is-stream? nil)
    ((room :string (len-between 0 room 16)))
  (subscribe! (intern room :keyword) sock))

(define-handler (send-message)
    ((room :string (len-between 0 room 16))
     (name :string (len-between 1 name 64))
     (message :string (len-between 5 message 256)))
  (publish! (intern room :keyword)
	    (encode-json-to-string
	     `((:name . ,name) (:message . ,message)))))

(define-handler (index) ()
  (with-html-output-to-string (s nil :prologue t :indent t)
    (:html
     (:head (:script
	     :type "text/javascript"
	     :src "/static/js/interface.js"))
     (:body (:div :id "messages")
	    (:textarea :id "input")
	    (:button :id "send" "Send")))))
```

While we are still working with Lisp code, this interface is starting to look almost like a _declarative language_, in which we state _what_ we want our handlers to validate without thinking too much about _how_ they are going to do it. What we are doing is building a _domain-specific language_ (DSL) for handler functions; that is, we are creating a specific convention and syntax that allows us to concisely express exactly what we want our handlers to validate. This approach of building a small language to solve the problem at hand is frequently used by Lisp programmers, and it is a useful technique that can be applied in other programming languages.

### A DSL for Handlers

Now that we have a loose specification for how we want our handler DSL to look, how do we implement it? That is, what specifically do we expect to happen when we call `define-handler`? Let's consider the definition for `send-message` from above:

```lisp
(define-handler (send-message)
    ((room :string (len-between 0 room 16))
     (name :string (len-between 1 name 64))
     (message :string (len-between 5 message 256)))
  (publish! (intern room :keyword)
	    (encode-json-to-string
	     `((:name . ,name) (:message . ,message)))))
```

What we would like `define-handler` to do here is:

1. Bind the action `(publish! ...)` to the URI `/send-message` in the handlers table.
2. When a request to this URI is made: 
    - Ensure that the HTTP parameters `room`, `name` and `message` were
      included.
    - Validate that `room` is a string no longer than 16 characters, `name` is
      a string of between 1 and 64 characters (inclusive) and that `message`
      is a string of between 5 and 256 characters (also inclusive).
3. After the response has been returned, close the channel.

While we could write Lisp functions to do all of these things, and then manually assemble the pieces ourselves, a more common approach is to use a Lisp facility called `macros` to _generate_ the Lisp code for us. This allows us to concisely express what we want our DSL to do, without having to maintain a lot of code to do it. You can think of a macro as an "executable template" that will be expanded into Lisp code at runtime.

Here's our `define-handler` macro[^indentation]:

[^indentation]: I should note, the below code-block is VERY unconventional indentation for Common Lisp. Arglists are typically not broken up over multiple lines, and are usually kept on the same line as the macro/function name. I had to do it to stick to the line-width guidelines for this book, but would otherwise prefer to have longer lines that break naturally at places dictated by the content of the code.

```lisp
(defmacro define-handler
    ((name &key (is-stream? t) (content-type "text/html")) (&rest args)
     &body body)
  (if is-stream?
      `(bind-handler
	,name (make-closing-handler
	       (:content-type ,content-type)
	       ,args ,@body))
      `(bind-handler
	,name (make-stream-handler ,args ,@body))))
```

It delegates to three other macros (`bind-handler`, `make-closing-handler`, \newline `make-stream-handler`) that we will define later. `make-closing-handler` will create a handler for a full HTTP request/response cycle; `make-stream-handler` will instead handle an SSE message. The predicate `is-stream?` distinguishes between these cases for us. The backtick and comma are macro-specific operators that we can use to "cut holes" in our code that will be filled out by values specified in our Lisp code when we actually use `define-handler`.

Notice how closely our macro conforms to our specification of what we wanted `define-handler` to do: If we were to write a series of Lisp functions to do all of these things, the intent of the code would be much more difficult to discern by inspection.

### Expanding a Handler

Let's step through the expansion for the `send-message` handler so that we better understand what is actually going on when Lisp "expands" our macro for us. We'll use the macro expansion feature from the [SLIME](https://common-lisp.net/project/slime/) Emacs mode to do this. Calling `macro-expander` on `define-handler` will expand our macro by one "level", leaving our helper macros in their still-condensed form:

```lisp
(BIND-HANDLER
 SEND-MESSAGE
 (MAKE-CLOSING-HANDLER
  (:CONTENT-TYPE "text/html")
  ((ROOM :STRING (LEN-BETWEEN 0 ROOM 16))
   (NAME :STRING (LEN-BETWEEN 1 NAME 64))
   (MESSAGE :STRING (LEN-BETWEEN 5 MESSAGE 256)))
  (PUBLISH! (INTERN ROOM :KEYWORD)
	    (ENCODE-JSON-TO-STRING
	     `((:NAME ,@NAME) (:MESSAGE ,@MESSAGE))))))
```

Our macro has already saved us a bit of typing by substituting our `send-message` specific code into our handler template. `bind-handler` is another macro which maps a URI to a handler function on our handlers table; since it's now at the root of our expansion, let's see how it is defined before expanding this further.

```lisp
(defmacro bind-handler (name handler)
  (assert (symbolp name) nil "`name` must be a symbol")
  (let ((uri (if (eq name 'root) "/" (format nil "/~(~a~)" name))))
    `(progn
       (when (gethash ,uri *handlers*)
	 (warn ,(format nil "Redefining handler '~a'" uri)))
       (setf (gethash ,uri *handlers*) ,handler))))
```

The binding happens in the last line: `(setf (gethash ,uri *handlers*) ,handler)`, which is what hash-table assignments look like in Common Lisp (modulo the commas, which are part of our macro). Note that the `assert` is outside of the quoted area, which means that it'll be run as soon as the macro is _called_ rather than when its result is evaluated.

When we further expand our expansion of the `send-message` `define-handler` above, we get:

```lisp
(PROGN
  (WHEN (GETHASH "/send-message" *HANDLERS*)
    (WARN "Redefining handler '/send-message'"))
  (SETF (GETHASH "/send-message" *HANDLERS*)
	(MAKE-CLOSING-HANDLER
	 (:CONTENT-TYPE "text/html")
	 ((ROOM :STRING (LEN-BETWEEN 0 ROOM 16))
	  (NAME :STRING (LEN-BETWEEN 1 NAME 64))
	  (MESSAGE :STRING (LEN-BETWEEN 5 MESSAGE 256)))
	 (PUBLISH! (INTERN ROOM :KEYWORD)
		   (ENCODE-JSON-TO-STRING
		    `((:NAME ,@NAME) (:MESSAGE ,@MESSAGE)))))))
```

This is starting to look more like a custom implementation of what we would have written to marshal a request from a URI to a handler function, had we written it all ourselves. But we didn't have to!

We still have `make-closing-handler` left to go in our expansion. Here is its definition:

```lisp
(defmacro make-closing-handler
    ((&key (content-type "text/html")) (&rest args) &body body)
  `(lambda (sock parameters)
     (declare (ignorable parameters))
     ,(arguments
       args
       `(let ((res (make-instance
		    'response
		    :content-type ,content-type
		    :body (progn ,@body))))
	  (write! res sock)
	  (socket-close sock)))))
```

So making a closing-handler involves making a `lambda`, which is just what you call anonymous functions in Common Lisp. We also set up an interior scope that makes a `response` out of the `body` argument we're passing in, performs a `write!` to the requesting socket, then closes it. The remaining question is, what is `arguments`?

```lisp
(defun arguments (args body)
  (loop with res = body
     for arg in args
     do (match arg
	 ((guard arg-sym (symbolp arg-sym))
	  (setf res `(let ((,arg-sym ,(arg-exp arg-sym))) ,res)))
	 ((list* arg-sym type restrictions)
	  (setf res
		(let ((sym (or (type-expression
				(arg-exp arg-sym)
				type restrictions)
			       (arg-exp arg-sym))))
		  `(let ((,arg-sym ,sym))
		     ,@(awhen (type-assertion arg-sym type restrictions)
			 `((assert-http ,it)))
		     ,res)))))
     finally (return res)))
```

Welcome to the hard part. `arguments` turns the validators we registered with our handler into a tree of parse attempts and assertions. `type-expression`, `arg-exp`, and `type-assertion` are used to implement and enforce a "type system" for the kinds of data we're expecting in our responses; we'll discuss them in \aosasecref{sec.eventsweb.types}. Using this together with `make-closing-handler` would implement the validation rules we wrote here:

```lisp
(define-handler (send-message)
    ((room :string (>= 16 (length room)))
     (name :string (>= 64 (length name) 1))
     (message :string (>= 256 (length message) 5)))
  (publish! (intern room :keyword)
	    (encode-json-to-string
	     `((:name . ,name) (:message . ,message)))))
```

...as an "unrolled" sequence of checks needed to validate the request:

```lisp
(LAMBDA (SOCK #:COOKIE?1111 SESSION PARAMETERS)
  (DECLARE (IGNORABLE SESSION PARAMETERS))
  (LET ((ROOM (AIF (CDR (ASSOC :ROOM PARAMETERS))
		   (URI-DECODE IT)
		   (ERROR (MAKE-INSTANCE
			   'HTTP-ASSERTION-ERROR
			   :ASSERTION 'ROOM)))))
    (ASSERT-HTTP (>= 16 (LENGTH ROOM)))
    (LET ((NAME (AIF (CDR (ASSOC :NAME PARAMETERS))
		     (URI-DECODE IT)
		     (ERROR (MAKE-INSTANCE
			     'HTTP-ASSERTION-ERROR
			     :ASSERTION 'NAME)))))
      (ASSERT-HTTP (>= 64 (LENGTH NAME) 1))
      (LET ((MESSAGE (AIF (CDR (ASSOC :MESSAGE PARAMETERS))
			  (URI-DECODE IT)
			  (ERROR (MAKE-INSTANCE
				  'HTTP-ASSERTION-ERROR
				  :ASSERTION 'MESSAGE)))))
	(ASSERT-HTTP (>= 256 (LENGTH MESSAGE) 5))
	(LET ((RES (MAKE-INSTANCE
		    'RESPONSE :CONTENT-TYPE "text/html"
		    :COOKIE (UNLESS #:COOKIE?1111
			      (TOKEN SESSION))
		    :BODY (PROGN
			    (PUBLISH!
			     (INTERN ROOM :KEYWORD)
			     (ENCODE-JSON-TO-STRING
			      `((:NAME ,@NAME)
				(:MESSAGE ,@MESSAGE))))))))
	  (WRITE! RES SOCK)
	  (SOCKET-CLOSE SOCK))))))
```

This gets us the validation we need for full HTTP request/response cycles. What about our SSEs? `make-stream-handler` does the same basic thing as `make-closing-handler`, except that it writes an `SSE` rather than a `RESPONSE`, and it calls `force-output` instead of `socket-close` because we want to flush data over the connection without closing it:

```lisp
(defmacro make-stream-handler ((&rest args) &body body)
  `(lambda (sock parameters)
     (declare (ignorable parameters))
     ,(arguments
       args
       `(let ((res (progn ,@body)))
	  (write! (make-instance
		   'response
		   :keep-alive? t
		   :content-type "text/event-stream")
		  sock)
	  (write!
	   (make-instance 'sse :data (or res "Listening..."))
	   sock)
	  (force-output
	   (socket-stream sock))))))

(defmacro assert-http (assertion)
  `(unless ,assertion
     (error (make-instance
	     'http-assertion-error
	     :assertion ',assertion))))
```

`assert-http` is a macro that creates the boilerplate code we need in error cases. It expands into a check of the given assertion, throws an `http-assertion-error` if it fails, and packs the original assertion along in that event.

```lisp
(defmacro assert-http (assertion)
  `(unless ,assertion
     (error (make-instance
	     'http-assertion-error
	     :assertion ',assertion))))
```

### HTTP "Types"
\label{sec.eventsweb.types}

In the previous section, we briefly touched on three expressions that we're using to implement our HTTP type validation system: `arg-exp`, `type-expression` and `type-assertion`. Once you understand those, there will be no magic left in our framework. We'll start with the easy one first.

#### arg-exp

`arg-exp` takes a symbol and creates an `aif` expression that checks for the presence of a parameter.

```lisp
(defun arg-exp (arg-sym)
  `(aif (cdr (assoc ,(->keyword arg-sym) parameters))
	(uri-decode it)
	(error (make-instance
		'http-assertion-error
		:assertion ',arg-sym))))
```

Evaluating `arg-exp` on a symbol looks like:

```lisp
HOUSE> (arg-exp 'room)
(AIF (CDR (ASSOC :ROOM PARAMETERS))
     (URI-DECODE IT)
     (ERROR (MAKE-INSTANCE
	     'HTTP-ASSERTION-ERROR
	     :ASSERTION 'ROOM)))
HOUSE>
```

We've been using forms like `aif` and `awhen` without understanding how they work, so let's take some time to explore them now.

Recall that Lisp code is itself represented as a tree. That's what the parentheses are for; they show us how leaves and branches fit together. If we step back to what we were doing in the previous section, `make-closing-handler` calls a function called `arguments` to generate part of the Lisp tree it's constructing, which in turn calls some tree-manipulating helper functions, including `arg-exp`, to generate its return value.

That is, we've built a small system that takes a Lisp expression as input, and produces a different Lisp expression as output. Possibly the simplest way of conceptualizing this is as a simple Common–Lisp-to-Common–Lisp compiler that is specialized to the problem at hand.

A widely used classification of such compilers is as _anaphoric macros_. This term comes from the linguistic concept of an _anaphor_, which is the use of one word as a substitute for a group of words that preceded it. `aif` and `awhen` are anaphoric macros, and they're the only ones that I tend to often use. There are many more availabile in the [`anaphora` package](http://www.cliki.net/Anaphora).

As far as I know, anaphoric macros were first defined by Paul Graham in an [OnLisp chapter](http://dunsmor.com/lisp/onlisp/onlisp_18.html). The use case he gives is a situation where you want to do some sort of expensive or semi-expensive check, then do something conditionally on the result. In the above context, we're using `aif` to do a check the result of an `alist` traversal.

```lisp
(aif (cdr (assoc :room parameters))
     (uri-decode it)
     (error (make-instance
	     'http-assertion-error
	     :assertion 'room)))
```

This takes the `cdr` of looking up the symbol `:room` in the association list `parameters`. If that returns a non-nil value, `uri-decode` it, otherwise throw an error of the type `http-assertion-error`.

In other words, the above is equivalent to:

```lisp
(let ((it (cdr (assoc :room parameters))))
  (if it
      (uri-decode it)
      (error (make-instance
	      'http-assertion-error
	      :assertion 'room))))
```

Strongly-typed functional languages like Haskell often use a `Maybe` type in this situation. In Common Lisp, we capture the symbol `it` in the expansion as the name for the result of the check.

Understanding this, we should be able to see that `arg-exp` is generating a specific, repetitive, piece of the code tree that we eventually want to evaluate. In this case, the piece that checks for the presence of the given parameter among the handlers' `parameters`. Now, let's move onto...

#### type-expression

```lisp
(defgeneric type-expression (parameter type)
  (:documentation
   "A type-expression will tell the server
how to convert a parameter from a string to
a particular, necessary type."))
...
(defmethod type-expression (parameter type) nil)
```

This is a generic function that generates new tree structures (coincidentally Lisp code), rather than just a function. The only thing the above tells you is that by default, a `type-expression` is `NIL`. Which is to say, we don't have one. If we encounter a `NIL`, we use the raw output of `arg-exp`, but that doesn't tell us much about the most common case. To see that, let's take a look at a built-in (to `:house`) `define-http-type` expression.

```lisp
(define-http-type (:integer)
    :type-expression `(parse-integer ,parameter :junk-allowed t)
    :type-assertion `(numberp ,parameter))
```

An `:integer` is something we're making from a `parameter` by using `parse-integer`. The `junk-allowed` parameter tells `parse-integer` that we're not confident the data we're giving it is actually parseable, so we need to make sure that the returned result is an integer. If it isn't, we get this behaviour:

```
HOUSE> (type-expression 'blah :integer)
(PARSE-INTEGER BLAH :JUNK-ALLOWED T)
HOUSE>
```

`define-http-handler`[^readable] is one of the exported symbols for our framework. This lets our application programmers define their own types to simplify parsing above the handful of "builtins" that we give them (`:string`, `:integer`, `:keyword`, `:json`, `:list-of-keyword` and `:list-of-integer`).

```lisp
(defmacro define-http-type ((type) &key type-expression type-assertion)
  (with-gensyms (tp)
    `(let ((,tp ,type))
       ,@(when type-expression
	  `((defmethod type-expression (parameter (type (eql ,tp)))
	      ,type-expression)))
       ,@(when type-assertion
	  `((defmethod type-assertion (parameter (type (eql ,tp)))
	      ,type-assertion))))))
```

[^readable]: This macro is difficult to read because it tries hard to make its output human-readable, by expanding `NIL`s away using `,@` where possible.

It works by creating `type-expression` and `type-assertion` method definitions for the type being defined. We could let users of our framework do this manually without much trouble; however, adding this extra level of indirection gives us, the framework programmers, the freedom to change _how_ types are implemented without forcing our users to re-write their specifications. This isn't just an academic consideration; I've personally made radical changes to this part of the system when first building it, and was pleased to find that I had to make very few edits to the applications that depended on it.

Let's take a look at the expansion of that integer definition to see how it works in detail:

```lisp
(LET ((#:TP1288 :INTEGER))
  (DEFMETHOD TYPE-EXPRESSION (PARAMETER (TYPE (EQL #:TP1288)))
    `(PARSE-INTEGER ,PARAMETER :JUNK-ALLOWED T))
  (DEFMETHOD TYPE-ASSERTION (PARAMETER (TYPE (EQL #:TP1288)))
    `(NUMBERP ,PARAMETER)))
```

As we said, it doesn't reduce code size by much, but it does prevent us from needing to care what the specific parameters of those methods are, or even that they're methods at all.

#### type-assertion

Now that we can define types, let's look at how we use `type-assertion` to validate that a parse satisfies our requirements. It, too, takes the form of a complementary `defgeneric`/`defmethod` pair just like `type-expression`:

```lisp
(defgeneric type-assertion (parameter type)
  (:documentation
   "A lookup assertion is run on a parameter
immediately after conversion. Use it to restrict
 the space of a particular parameter."))
...
(defmethod type-assertion (parameter type) nil)
```

Here's what this one outputs:

```lisp
HOUSE> (type-assertion 'blah :integer)
(NUMBERP BLAH)
HOUSE>
```

There are cases where `type-assertion` won't need to do anything. For example, since HTTP parameters are given to us as strings, our `:string` type assertion has nothing to validate:

```lisp
HOUSE> (type-assertion 'blah :string)
NIL
HOUSE>
```

### All Together Now

We did it! We built a web framework on top of an event-driven webserver implementation. Our framework (and handler DSL) defines new applications by:

- Mapping URLs to handlers;
- Defining handlers to enforce the type safety and validation rules on requests;
- Optionally specifying new types for handlers as required.

Now we can describe our application like this:

```lisp
(defun len-between (min thing max)
  (>= max (length thing) min))

(define-handler (source :is-stream? nil)
    ((room :string (len-between 0 room 16)))
  (subscribe! (intern room :keyword) sock))

(define-handler (send-message)
    ((room :string (len-between 0 room 16))
     (name :string (len-between 1 name 64))
     (message :string (len-between 5 message 256)))
  (publish! (intern room :keyword)
	    (encode-json-to-string
	     `((:name . ,name) (:message . ,message)))))

(define-handler (index) ()
  (with-html-output-to-string (s nil :prologue t :indent t)
    (:html
     (:head (:script
	     :type "text/javascript"
	     :src "/static/js/interface.js"))
     (:body (:div :id "messages")
	    (:textarea :id "input")
	    (:button :id "send" "Send")))))

(start 4242)
```

Once we write `interface.js` to provide the client-side interactivity, this will start an HTTP chat server on port `4242` and listen for incoming connections.
