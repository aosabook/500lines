title: An Event-driven Web Framework
author: Leo Zovic

In 2013, I decided to write a [web-based game prototyping tool](https://github.com/Inaimathi/deal) for card and board games. In these types of games, it is common for one player to wait for a while for the other player to make a move; however, when the other player finally does take action, we would like for the waiting player to be notified of the move quickly thereafter.

This is a problem that turns out to be more complicated than it seems at first. In this chapter, we'll explore the issues with using HTTP to build this sort of interaction, and then we'll build a _web framework_ in Common Lisp that allows us to solve similar problems in the future. 

## The Basics of HTTP Servers

At the simplest level, an HTTP exchange is a single request followed by a single response. A _client_ sends a request, which includes a resource identifier, an HTTP version tag, some headers and some parameters. The _server_ parses that request, figures out what to do about it, and sends a response which includes the same HTTP version tag, a response code, some headers and a request body. (For more on this, see the FIXME web server chapter.)

Notice that, in this description, the server responds to a request from a specific client. In our case, we want each player to be updated about _any_ moves as soon as they happen, rather than only getting notifications when their own move is made. This means we need the server to _push_ messages to clients without first receiving a request for the information [^polling]. 

[^polling]: The simplest solution to this problem is to force the clients to _poll_ the server. That is, each client would periodically send the server a request asking if anything has changed. This can work for simple applications, but in this chapter we're going to focus on the solutions available to you when this model stops working.

There are several standard approaches to enabling server push over HTTP. 

### Comet/Longpoll

The "longpoll" technique has the client send the server a new request as soon as it receives a response. Instead of fulfilling that request right away, the server waits on a subsequent event to respond. This is a bit of a semantic distinction, since the client is still taking action on the users' behalf on every update.

### Server-sent Events (SSE)

Server-sent events require that the client initiates a connection and then keeps it open. The server periodically writes new data to the connection without closing it, and the client interprets incoming new messages as they arrive rather than waiting for the response connection to terminate. This is a bit more efficient than the Comet/Longpoll approach because each message doesn't have to incur the overhead of a fresh set of HTTP headers.

### Websockets

"Websockets" are a communication protocol built on top of HTTP. The server and client open up an HTTP conversation, then perform a handshake and protocol escalation. The end result is that they're still communicating over TCP/IP, but they're not using HTTP to do it at all. The advantage this has over SSEs is that you can customize the protocol for efficiency.

### Long-Lived Connections

These three approaches are quite different from one another, but they all share an important characteristic: They all depend on long-lived connections. Longpolling depends on the server keeping requests around until new data is available (thus keeping a connection open until new data arrives, or the client gives up in frustration), SSEs keep an open stream between client and server to which data is periodically written, and Websockets change the protocol a particular connection is speaking, but leave it open. 

To see why this might cause problems for your average HTTP server, let's consider how the underlying implementation might work.

### Traditional HTTP Server Architecture

A single HTTP server processes many requests concurrently. Historically, many HTTP servers have used a _thread-per-request_ architecture. That is, for each incoming request, the server creates a thread to do the work necessary to respond. 

Since each of these connections is very short-lived, we will likely not need many threads executing in parallel to handle all of the incoming requests. This model also simplifies the _implementation_ of the server by enabling the server programmer to write code as if there were only one connection being handled at at any given time. It also gives us the freedom to clean up failed or "zombie" connections and their associated resources by killing the corresponding thread and letting the garbage collector do its job. 

The key observation is that an HTTP server hosting a "traditional" web application that has $N$ concurrent users might only need to handle a very small fraction of $N$ requests _in parallel_ to succeed. For the type of interactive application that we are trying to build in our initial problem statement, $N$ users of our application will almost certainly require the application to maintain at least $N$ connections in parallel at once.

The consequence of keeping long-lived connections around is that we're going to want either:

- A platform where threads are "cheap" enough that we can use large numbers of them at once
- A server architecture that can handle many connections with a single thread

There are platforms such as [Racket](http://racket-lang.org/), [Erlang](http://www.erlang.org/), and [Haskell](http://hackage.haskell.org/package/base-4.7.0.1/docs/Control-Concurrent.html) that do provide thread-like constructs that are "lightweight" enough to consider the first option. This approach requires the programmer to explicitly deal with synchronization issues, which are going to be much more prevalent in a system where connections are open for a long time and likely all competing for similar resources. Specifically, if we have some sort of central data shared by several users simltaneously, we will need to coordinate reads and writes of that data in some way. 

If we don't have cheap threads at our disposal or we are unwilling to work with explicit synchronization, we must consider having a single thread handle many connections[^mn]. In this model, our single thread is going to be handling tiny "slices" of many requests all at once, switching between them as efficiently as it possibly can. This system architecture pattern is most commonly referred to as _event-driven_ or _event-based_[^eventbased]. 

[^mn]: We could consider a more general system that handles $N$ concurrent users with $M$ threads for some configurable value of $M$; in this model, the $N$ connections are said to be _multiplexed_ across the $M$ threads. In this chapter, we are going to focus on writing a program where $M$ is fixed at 1; however, the lessons learned here should be largely applicable to the more general model. 

[^eventbased]: This nomenclature is a bit confusing, and has its origin in early operating-systems research. It refers to how communication is done between multiple concurrent processes. In a thread-based system, communication is done through a synchronized resource such as shared memory. In an event-based system, processes generally communicate through a queue where they post items that describe what they have done or what they want done, which is maintained by our single thread of execution. Since these items generally describe desired or past actions, they are referred to as 'events'. 

Since we are only managing a single thread, we don't have to worry as much about protecting shared resources from simultaneous access. However, we do have a unique problem of our own in this model. Since our single thread is working on all in-flight requests at once, we must make sure that it never gets stuck. That is, we must prevent this thread from _blocking_. Blocking on any connection blocks the entire server from making progress on any other request. We have to be able to move on to another client if the current one can't be serviced further, and we need to be able to do so in a manner that doesn't throw out all of the work done so far[^crawler]. 

[^crawler]: See the FIXME web crawler chapter for another possible solution to this problem. 

While it is uncommon for a programmer to explicitly tell a thread to stop working, many common operations carry a risk of blocking. Because threads are so prevalent and reasoning about asychronousity is a heavy burden on the programmer, many languages and their frameworks assume that blocking on IO is a desirable property. This makes it very easy to block somewhere *by accident*. Luckily, Common Lisp does provide us with a minimal set of asynchronous IO primitives which we can build on top of.

### Architectural decisions

Now that we've studied the background of this problem, we've arrived at the point where we need to make informed decisions about _what_ we are building.

At the time I started thinking about this project, Common Lisp didn't have a complete green-thread implementation, and the [standard portable threading library](http://common-lisp.net/project/bordeaux-threads/) doesn't qualify as "really REALLY cheap". The options amounted to either picking a different language, or building an event-driven web server for my purpose. I chose the latter.

In addition to the server architecture, we also need to choose which of the 3 server-push approaches to use. The use-case we are considering (an interactive multiplayer board game) requires frequent updates to each client, but relatively sparse requests *from* each client, which fits the SSE approach to pushing updates, so we'll go with this.

Now that we've motivated our architectural decision and decided on a mechanism for simulating bidirectional communication between clients and server, let's get started on building our web framework. We'll start by building a relatively "dumb" server first, and then we'll extend it into a web-application framework that provides application programmers with some tools that allow them to focus on _what_ their heavily-interactive program needs to do, and not _how_ it is doing it.

## Building an Event-Driven Web Server

### The Event Loop

The core of every event-driven program is the _event loop_, which looks something like this: 

```lisp
	(defmethod start ((port integer))
	  (let ((server (socket-listen usocket:*wildcard-host* port :reuse-address t :element-type 'octet))
		    (conns (make-hash-table)))
	    (unwind-protect
		    (loop (loop for ready in (wait-for-input (cons server (alexandria:hash-table-keys conns)) :ready-only t)
			            do (process-ready ready conns)))
	     (loop for c being the hash-keys of conns
		     do (loop while (socket-close c)))
	     (loop while (socket-close server)))))
```

In this loop, we have:

- a server socket that listens for incoming connections;
- a structure to store connections/buffers;
- an infinite loop waiting for new handshakes or incoming data on an existing connection;
- cleanup clauses to prevent dangling sockets that are unexpectedly killed (e.g. by an interrupt)

If you haven't written a Common Lisp program before, this code block requires some explanation. What we have written here is a `method` definition (`def`). While Lisp is popularly known as a functional language, it also has its own system for object-oriented programming called "The Common Lisp Object System", which is usually abbreviated as "CLOS"[^CLOSpronounce]. 

[^CLOSpronounce]: Pronounced "KLOS", "see-loss" or "see-lows", depending on who you talk to.

### CLOS and Generic Functions

In CLOS, instead of focusing on classes and methods, we instead write _generic functions_[^juliachap] that are implemented as collections of _methods_. In this model, methods don't _belong_ to classes, they _specialize on_ types. The `start` method we just wrote is a unary method where the argument `port` is _specialized on_ the type `integer`. This means that we could have several implementations of `start` where `port` varies in type, and the runtime will select which implementation to use depending on the type of `port` when `start` is called.

[^juliachap]: The Julia programming language takes a similar approach to object-oriented programming; you can learn more about it in FIXME.

More generally, methods can specialize on more than one argument. When a `method` is called, the runtime:

- dispatches on the type of its arguments to figure out which method body should be run, and
- runs the appropriate function.

You can think about it as a giant table with "Type Name" down the first column and "Operation" across the first row, as in Table FIXME.

```
            | Addition | Subtraction | Multiplication |
    ----------------------------------------------------------
    Integer |          |             |                |
	-----------------------------------------------------
	Real    |          |             |                |
	-----------------------------------------------
	Complex |          |             |
```

NOTE: Is this a misleading analogy? If we consider the specific case of these being binary operators (let's not handle things like (+ x y z w)) isn't it more like an N-d table, or for simplicity:

```

------------------------------------------------------------------------
Operation | Type of x | Type of y | Method to call 
------------------------------------------------------------------------
 +        | Int       | Int       | (defmethod + ((x int) (y int)) ... )
------------------------------------------------------------------------
 +        | Int       | Float     | (defmethod + ((x int) (y float)) ... )
------------------------------------------------------------------------
 *        | Int       | Int       | (defmethod * ((x int) (y int)) ... )
------------------------------------------------------------------------
```

From a theoretical perspective, the class-focused approach and the function-focused approach can be seen as orthogonal approaches to the same problem. Class-based systems tend to organize methods with the class they "belong" to, whereas in a generic-function-based system each method has all of its implementations grouped in one place.

This means that in a class-based system, it's difficult to know what classes implement method `foo`; in a generic-function-based system, it is difficult to know all the methods that specialize on type `bar`. Depending on the nature of your system and your problem, you may want one or the other. (FIXME this probably needs more depth. Or just say how to make this decision is outside the scope of this chapter, we'll just give you a taste of what it's like to work in this type of system here.)

### Processing sockets

We'll see another generic function at work in `process-ready`, which was called earlier from our event loop. It processes a ready socket with one of two methods, depending on the type of socket we are handling.

FIXME: What precisely is the difference between `stream-usocket` and `stream-server-usocket`? What coerces our sockets to one of these two types in the first place?

If a `stream-server-socket` is `ready`, that means there's a new client socket waiting to start a conversation. We call `socket-accept` to accept the connection, and then put the result in our connection table so that our event loop can begin processing it with the others.

```lisp
	(defmethod process-ready ((ready stream-server-usocket) (conns hash-table))
	  (setf (gethash (socket-accept ready :element-type 'octet) conns) nil))
```

When a `stream-usocket` is `ready`, that means that it has some bytes ready for us to read. (It's also possible that the other party has terminated the connection). 

```lisp
	(defmethod process-ready ((ready stream-usocket) (conns hash-table))
	  (let ((buf (or (gethash ready conns)
			 (setf (gethash ready conns) (make-instance 'buffer :bi-stream (flex-stream ready))))))
	    (if (eq :eof (buffer! buf))
		(ignore-errors 
		  (remhash ready conns)
		  (socket-close ready))
		(let ((too-big? (> (total-buffered buf) +max-request-size+))
		      (too-old? (> (- (get-universal-time) (started buf)) +max-request-age+))
		      (too-needy? (> (tries buf) +max-buffer-tries+)))
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
3. If that read got us an `:eof`, it means the other side hung up, so we discard the socket *and* its buffer;
4. Otherwise, we check if the buffer is one of `complete?`, `too-big?`, `too-old?` or `too-needy?`. If it's any of them, we remove it from the connections table and return the appropriate HTTP response.

This is the first time we're seeing I/O in our event loop. In our discussion in FIXME SECTIONREF, we mentioned that we have to be very careful about I/O in an event-driven system, because we could accidentally block our single thread. So, what do we do here to ensure that this doesn't happen? We have to explore our implementation of `buffer!` to find out exactly how this works.

### Processing Connections Without Blocking

The basis of our approach to processing connections without blocking is the library function `read-char-no-hang`](http://clhs.lisp.se/Body/f_rd_c_1.htm), which immediately returns `nil` when called on a stream that has no available data. Where there is data to be read, we use a buffer to store intermediate input for this connection.

[^readcharnohang]: http://clhs.lisp.se/Body/f_rd_c_1.htm

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
- It also tracks any `\r\n\r\n` sequences so that we can later detect complete requests. 
- Finally, any error results it returns an `:eof` to signal that `process-ready` should discard this particular connection.

The `buffer` type is a CLOS _class_. Classes in CLOS let us define a type with fields called `slots`. We don't see the behaviours associated with `buffer` on the class definition, because (as we've already learned), we do that using generic functions like `buffer!`.

`defclass` does allow us to specify getters/setters (`reader`s/`accessor`s), and slot initializers; `:initform` specifies a default value, while `:initarg` identifies a hook that the caller of `make-instance` can use to provide a default value. 

NOTE: Generic functions can specialize on types that aren't classes, right? The example of `port` from the first method definition didn't seem like it was a class.

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

Our `buffer` class has six slots; seen in FIXME TABLE:

TABLE THIS
- `tries`, which keeps count of how many times we've tried reading into this buffer
- `contents`, which contains what we've read so far
- `bi-stream`, which a hack around some of those Common Lisp-specific, non-blocking-IO annoyances I mentioned earlier
- `total-buffered`, which is a count of chars we've read so far
- `started`, which is a timestamp that tells us when we created this buffer
- `request`, which will eventually contain the request we construct from buffered data
- `expecting`, which will signal how many more chars we're expecting (if any) after we buffer the request headers

### Interpreting Requests

Now that we've seen how we incrementally assemble full requests from bits of data that are pooled into our buffers, what happens when we have a full request ready for handling? This happens in the method `handle-request`.

```lisp
	(defmethod handle-request ((socket usocket) (req request))
	  (aif (lookup (resource req) *handlers*)
	       (funcall it socket (parameters req))
	       (error! +404+ socket)))
```

This method adds another layer of error handling so that if the request is old, big, or needy, we can send a `400` response to indicate that the client provided us with some bad or slow data. However, if any _other_ error happens here, it's because the programer made a mistake defining a _handler_, which should be treated as a `500` error. This will inform the client that something went wrong on the server a result of their legitimate request.

If the request is well-formed, we do the tiny and obvious job of looking up the requested resource in the `*handlers*` table. If we find one, we `funcall` `it`, passing along the client `socket` as well as the parsed request parameters. If there's no matching handler in the `*handlers*` table, we instead send along a `404` error. The handler system will be part of our full-fledged _web framework_, which we'll discuss in a subsequent section.

We still haven't seen how requests are parsed and interpreted from one of our buffers, though. Let's look at that next:

```lisp
	(defmethod parse ((buf buffer))
	  (let ((str (coerce (reverse (contents buf)) 'string)))
	    (if (request buf)
		    (parse-params str)
		    (parse str))))
```

This high-level method delegates to a specialization of `parse` that works with plain strings or to `parse-params` that interprets the buffer contents as HTTP parameters. These are called depending on how much of the request we've already processed; the final `parse` happens when we already have a partial `request` saved in the given `buffer`, at which point we're only looking to parse the request body. 


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
		    (loop for header = (pop lines) for (name value) = (split ": " header)
		          until (null name) do (push (cons (->keyword name) value) (headers req)))
		    (setf (parameters req) (parse-params parameters))
		    req))))

	(defmethod parse-params ((params null)) nil)

	(defmethod parse-params ((params string))
	  (loop for pair in (split "&" params)
	        for (name val) = (split "=" pair)
	        collect (cons (->keyword name) (or val ""))))
```

In the `parse` method specializing on `string`, we transform the content into usable pieces. We do this on strings instead of working directly with buffers because this makes it easier to test the actual parsing code in an environment like an interpreter or REPL. 

The parsing process is:

1. split on `"\\r?\\n"`
2. split the first line of that on `" "` to get the request type (`POST`, `GET`, etc)/URI path/http-version
3. assert that we're dealing with an `HTTP/1.1` request
4. split the URI path on `"?"`, which gives us plain resource separate from any potential `GET` parameters
5. make a new `request` instance with the resource in place
6. populate that `request` instance with each split header line
7. set that `request`s parameters to the result of parsing our `GET` parameters

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
	  ((content-type :accessor content-type :initform "text/html" :initarg :content-type)
	   (charset :accessor charset :initform "utf-8")
	   (response-code :accessor response-code :initform "200 OK" :initarg :response-code)
	   (keep-alive? :accessor keep-alive? :initform nil :initarg :keep-alive?)
	   (body :accessor body :initform nil :initarg :body)))
```

The second is an [SSE messages](http://www.w3.org/TR/eventsource/), which we will use to send an incremental update to our clients.

```lisp
	(defclass sse ()
	  ((id :reader id :initarg :id :initform nil)
	   (event :reader event :initarg :event :initform nil)
       (retry :reader retry :initarg :retry :initform nil)
	   (data :reader data :initarg :data)))
```

We'll send an HTTP response whenever we receive a full HTTP request; however, how do we know when and where to send SSE messages in cases where there wasn't an originating client request?

A simple solution is by registered _channels_, to which we'll subscribe `socket`s as necessary.

```lisp
	(defparameter *channels* (make-hash-table))

	(defmethod subscribe! ((channel symbol) (sock usocket))
	  (push sock (gethash channel *channels*))
	  nil)
```

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

In `publish!`, we call `write!` to actually write an `sse` to a socket. We'll also need a specialization of `write!` on `response`s to write full HTTP responses as well. Let's handle the `response` case first.

```lisp
	(defmethod write! ((res response) (sock usocket))
	  (let ((stream (flex-stream sock)))
	    (flet ((write-ln (&rest sequences)
		     (mapc (lambda (seq) (write-sequence seq stream)) sequences)
		     (crlf stream)))
	      (write-ln "HTTP/1.1 " (response-code res))
	      (write-ln "Content-Type: " (content-type res) "; charset=" (charset res))
	      (write-ln "Cache-Control: no-cache, no-store, must-revalidate")
	      (when (keep-alive? res) 
		    (write-ln "Connection: keep-alive")
		    (write-ln "Expires: Thu, 01 Jan 1970 00:00:01 GMT"))
	      (awhen (body res)
		    (write-ln "Content-Length: " (write-to-string (length it)))
		    (crlf stream)
		    (write-ln it))
	      (values))))
```

This version of `write!` takes a `response` and a `usocket` named `sock`, and writes content to a stream provided by `sock`. We locally define the function `write-ln` which takes some number of sequences, and writes them out to the stream followed by a `crlf`. This is for readability; we could instead have called `write-sequence`/`crlf` directly. 

Writing an `SSE` out is conceptually similar to, but mechanically different from writing out a `response`:

```lisp
	(defmethod write! ((res sse) (sock usocket))
	  (let ((stream (flex-stream sock)))
	    (format stream "~@[id: ~a~%~]~@[event: ~a~%~]~@[retry: ~a~%~]data: ~a~%~%"
		    (id res) (event res) (retry res) (data res))))
```

This is simpler than working with full HTTP responses since the SSE message standard doesn't specify `CRLF` line-endings, so we can get away with a single `format` call. The `~@[...~]` blocks are _conditional directives_, which allow us to gracefully handle `nil` slots. For example, if `(id res)` is non-nil, we'll output `id: <the id here> `, otherwise we will ignore the directive entirely. The payload of our incremental update `data` is the only required slot of `sse`, so we can include it without worrying about it being `nil`.

## Extending Our Server Into a Web Framework


=== OUTLINE ===
Framework:
- This isn't a webserver, it's a framework. 
- Looking up _handlers_ for the request
- Many other 'traditional' frameworks will have handlers of a single type: request/response. We're going to have more:
- Introduce the handler types. This can either be from the readme or from the final thing that is currently at line 703.
- An example of using handlers (should probably appear wayyyy earlier)

- The Lisp solution to a problem is often: writing a DSL. You often use a Lisp
  feature called _macros_ to do this.
- We're going to write a handler mechanism that has strongly-type-checked http parameters
- "Macro guts"
    - Example expansion for handler macro
    - Defining handlers, and using "homo-iconic expanders"
    - Binding handlers
    - Expansions for the argument validation DSL
- An explanation of how macros are basically functioning
- Using a generic method (not a macro) to manipulate code for http "type" definition
- Declaring error classes and returning them from the event loop

Stuff to not forget to revisit:
- make-stream-handler creates handlers for SSE targets
- Summary of handler stuff on L709 would make a good intro to the section
- This is used as an abrupt finisher
- webcrawler chapter for discussion of nonblocking IO
- web server chapter for discussion of thread-based servers

======
OLD PASS
Intro
- Make this more generally about long-lived connections over http. Can refer back to Greg's webserver chapter.

Basics of HTTP servers
- Remove the mention of the session thing entirely
- The focus should be on: What happens if you have a problem that doesn't fit nicely into the browserclient-webserver model? We'll explore that problem here.

Current technology for doing this
- After the three points:
    - These three technologies all have different implementations, but they do
      share one requirement: Connections are long-lived. This defies the
      original assumption of the thread-per-request model which said that they
      aren't. So, what do we do?

Solutions tend to fall into one of two extremes:

- Thread-per-request
    - requires cheap threads
    - synchronization
- Events
    - You can't ever block
    - This means you need nonblocking access to resources, such as I/O 

You can see the webcrawler chapter for further discussion of this issue.

** Need to tell a story about why we chose the event-driven model. And why we chose common lisp. Something something mysticism.

Ideas?
- Make it clear that we're implementing SSE (?)
- Maybe show how the browser interacts with this thing?
- Make it clear that the EOF thing is the signal that it's time to purge the client
- But we need to be able to read from socket without blocking. read-char-no-hang is a library function
- Multiple dispatch actually helps us respond to stages in the event lifecycle, because the type of the socket changes
- The idea of how we'll nonblock needs to be sketched out earlier
- Representing a request independently from the sequence of bytes we read (nonblockingly) from a socket
- When do we present handlers? Do we just do request/response (frontendy) handlers first?
- declarative language for "typing" and validation HTTP resources

Two big sections: request/response, and then handlers

Concerns
- CLOS
- multiple dispatch in Julia chapter
