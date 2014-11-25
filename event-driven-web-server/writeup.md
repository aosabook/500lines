# On Interacting Through HTTP in an Event-Driven Manner in the Medium of Common Lisp

Backstory first here. So at some point last year, I got it into my head to put together a quick little [game prototyping tool](https://github.com/Inaimathi/deal). Like, physical card and board games. The use case was doing some light game dev with a friend of mine from university who had moved across the country, so it had to be something that bridged the spatial gap _as well as_ letting us think about mechanics and physical aspects.

Now, the problem with this goal is that it involves keeping long-lived connections between the clients and server, because while playing a game I (usually) want to see an opponents move as soon as it happens rather than only when I move. This wouldn't be a problem if not for the fact that [`hunchentoot`](http://weitz.de/hunchentoot/), the most popular Common Lisp web-server, works on a thread-per-request model. Actually, before we get into the specifics, lets back up for a second.

## The Basics of Event-Driven Servers

At the 10k-foot-level, an HTTP exchange is one request and one response. A client sends a request, which includes a resource identifier, an HTTP version tag, some headers and some parameters. The receiving server parses that request, figures out what to do about it, and sends a response which includes the same HTTP version tag, a response code, some headers and a request body.

And that's it.

Because this is the total of the basic protocol, many minimal servers take the thread-per-request approach. That is, for each incoming request, spin up a thread to do the work of parse/figure-out-what-to-do-about-it/send-response, and spin it down when it's done. The idea is that since each of these connections is very short lived, that won't start too many threads at once, and it'll let you simplify a lot of the implementation. Specifically, it lets you program as though there were only one connection present at any given time, and it lets you do things like kill orphaned connections just by killing the corresponding thread and letting the garbage collector do its job.

There's a couple of things missing in this description though. First, as described, there's no mechanism for a server to send updates to a client without that client specifically requesting them. Second, there's no identity mechanism, which you need in order to confidently assert that a number of requests come from the same client (or, from the client perspective, to make sure you're making a request from the server you think you're talking to). We won't be solving the second problem in the space of this write-up; a full session implementation would nudge us up to ~560 lines of code, and we've got a hard limit of 500. If you'd like to take a look, peek at the full implementation of `:house` [at its github](https://github.com/Inaimathi/house).

The first problem is interesting by itself, though. It's particularly interesting if you've ever wanted to put together multi-user web-applications for whatever reason, and as I noted in the opener, that's exactly what I'm doing. In my case, we've got two or more people all interacting with the same virtual tabletop. And we want each player to know about any other players moves as soon as they happen, rather than only getting notifications when their own move is made. This means we can't naively rely on the request/response structure I outlined earlier; we need the server to push messages down to clients. Here are our options for doing that in `HTTP`-land:

##### Comet/Longpoll

Build the client such that it automatically sends the server a new request as soon as it receives a response. Instead of fulfilling that request right away, the server then sits on it until there's new information to send, like say, a new message from some other user. The end result is that each user gets new updates as soon as they happen, rather than just when he takes action. It's a bit of a semantic distinction though, since the client *is* taking action on the users' behalf on every update.

##### SSE

The client opens up a connection and keeps it open. The server will periodically write new data to the connection without closing it, and the client will interpret incoming new messages as they arrive rather than waiting for the response connection to terminate. This way is a bit more efficient than the Comet/Longpoll approach because each message doesn't have to incur the overhead of a fresh set of HTTP headers.

##### Websockets

The server and client open up an HTTP conversation, then perform a handshake and protocol escalation. The end result is that they're still communicating over TCP/IP, but they're not using HTTP to do it at all. The advantage this has over SSEs is that you can customize your protocol, so it's possible to be more efficient.

That's basically it. I mean there used to be things called "Forever Frames" that have been thoroughly replaced by the SSE approach, and a couple of other tricks you could pull with proprietary or esoteric technologies, but they're not materially different from the above.

These approaches are pretty different from each other under the covers, as you can hopefully see now that you understand them in the abstract, but they have one important point in common. They all depend on long-lived connections. Longpolling depends on the server keeping requests around until new data is available (thus keeping a connection open until new data arrives, or the client gives up in frustration), SSEs keep an open stream between client and server to which data is periodically written, and Websockets change the protocol a particular connection is speaking, but leave it open (and bi-directional, which complicates matters slightly; you basically need to chuck websockets back into the main listen/read loop *and keep them there* until they're closed).

The consequence of keeping long-lived connections around is that you're going to want one of

a) A server that can service many connections with a single thread
b) A thread-per-request server that passes long-lived connections off to a separate subsystem, which must handle those long lived connections using a minimal number of threads
c) A thread-per-request server on top of a platform where threads are cheap enough that you can afford having a few hundred thousand of them around.

I maintain that Option b) is ridiculous. The main reason you want a thread-per-request model is to simplify implementation, but having to implement a threaded core _as well as_ a separate event-driven system just for server pushing would almost by definition be more complicated than using either model in isolation. In the absence of a [really](http://racket-lang.org/) REALLY [cheap](http://www.erlang.org/) thread [system](http://hackage.haskell.org/package/base-4.7.0.1/docs/Control-Concurrent.html), the simplest solution is resorting to Option a). I could have switched languages, or maybe hacked some just-good-enough support into Hunchentoot. What I chose to do was write a Common Lisp server that could service many connections on a single thread.

In other words, `:house` started out as the yak-shaving project from hell, and I'm about to regale you with the tale.

## Server Core

At its' very core, every event-driven server has to look something like

	(defmethod start ((port integer))
	  (let ((server (socket-listen usocket:*wildcard-host* port :reuse-address t :element-type 'octet))
		    (conns (make-hash-table)))
	    (unwind-protect
		    (loop (loop for ready in (wait-for-input (cons server (alexandria:hash-table-keys conns)) :ready-only t)
			            do (process-ready ready conns)))
	     (loop for c being the hash-keys of conns
		     do (loop while (socket-close c)))
	     (loop while (socket-close server)))))

That's the event loop that drives the rest. Its relevant characteristics are

- A server socket that listens for incoming connections,
- some structure in which to store connections/buffers
- an infinite loop waiting for new handshakes or incoming data
- and finally some cleanup clauses to make sure it doesn't leave dangling locks on resources if it's killed by an interrupt or something.

The specifics are going to be revealed in our `process-ready` method.

	(defmethod process-ready ((ready stream-server-usocket) (conns hash-table))
	  (setf (gethash (socket-accept ready :element-type 'octet) conns) nil))

What you're looking at above is the filled in version of

    (defmethod <name> ((<arg-name> <arg-type>)...)
	  <body>)

It looks _almost_ like a `defun`, except that each of the mandatory arguments have an extra symbol packed along with them that specify what types they're expected to be. Or rather, which types this particular method will be called for. This is the Common Lisp multiple dispatch system at work. We're `def`ining a `method` of two arguments, `ready` and `conns`, the first of which will be a `stream-server-socket` and the second will be a `hash-table`. This is what it looks like when we get a new connection coming into the socket we're listening on; the `stream-server-socket` we defined back in `start` signals that it's `ready`, and gets picked up by our central event loop. All we do at this stage is `socket-accept` the incoming connection, and insert it into the connection table. As you saw above, we're waiting on input from all the keys of this table, which means this newly accepted socket will eventually have some data for us. _When_ it does, we'll pick it up as a `ready` socket in our main loop and call `process-ready` on it.

	(defmethod process-ready ((ready stream-usocket) (conns hash-table))
	  (let ((buf (or (gethash ready conns)
			         (setf (gethash ready conns) (make-instance 'buffer :bi-stream (flex-stream ready))))))
	    (if (eq :eof (buffer! buf))
	      (ignore-errors
		     (remhash ready conns)
			 (socket-close ready))
	      (let ((complete? (found-crlf? buf))
		        (too-big? (> (content-size buf) +max-request-size+))
		        (too-old? (> (- (get-universal-time) (started buf)) +max-request-age+))
		        (too-needy? (> (tries buf) +max-buffer-tries+)))
	        (when (or complete? too-big? too-old? too-needy?)
		       (remhash ready conns)
		       (cond (too-big? (error! +413+ ready))
		             ((or too-old? too-needy?)
		              (error! +400+ ready))
		             (t (handler-case
		     	           (handle-request ready (parse buf))
		     	         (http-assertion-error () (error! +400+ ready))
		     	         ((and (not warning)
		     	               (not simple-error)) (e)
		     	          (error! +500+ ready e))))))))))

Which, as you may have suspected, does something different. This is the method that will run when we call `process-ready` with an argument of class `stream-usocket` rather than `stream-server-usocket`. The high level view of what we do here is

1. Get the buffer associated with this socket (creating it if it doesn't exist yet)
2. Reading output into that buffer, which happens in the call to `buffer!`
3. If reading output got us an `:eof`, it means that the other side has closed this socket so we just discard it
4. Otherwise, we check if the buffer is one of `complete?`, `too-big?`, `too-old?` or `too-needy?`. If it's any of them, we remove it from the connections table and send out the appropriate HTTP response.

Now, firstly, you'll have to take my word for it that most of these things happen correctly until we get to their implementations later. But more importantly, it might not be entirely apparent why we're going through this rigmarole. Because we're trying to serve up content with a single thread, we can't afford to block on any particular connection. Because if we _did_, we'd block the entire server, rather than just a single request. As a result, we have to do work to make sure that we never block on an incoming stream _and_ that if a particular stream is dragging its feet, we can move on to the next one without throwing out our work so far. That means a system of buffers to keep track of intermediate results, and a buffering process that stops if it runs out of available input before getting to a complete request. Most importantly, it means non-blocking IO. Here's ours:

	(defmethod buffer! ((buffer buffer))
	  (handler-case
	      (let ((stream (bi-stream buffer))
		        (partial-crlf (list #\return #\linefeed #\return)))
		    (incf (tries buffer))
		    (loop for char = (read-char-no-hang stream nil :eof)
		          do (when (and (eql #\linefeed char)
				                (starts-with-subseq partial-crlf (contents buffer)))
			           (setf (found-crlf? buffer) t))
		          until (or (null char) (eql :eof char))
		          do (push char (contents buffer)) do (incf (content-size buffer))
		          finally (return char)))
	    (error () :eof)))

When you call `buffer!` on a `buffer`, it increments the `tries` count, then loops to read characters from the input stream. If it reaches the end of available input, or an `:eof`, it returns the last character it read. It also keeps track of whether its seen an `\r\n\r\n` go by during the reading (to make it easier to detect complete requests later) and how many times we've tried to read from this buffer (so that we can evict needy buffers further up). Finally, if it encounters any kind of error during the read process, it returns an `:eof` to signal that the caller should just throw out this particular socket.

The procedure `read-char-no-hang` is essential here; that's the thing that allows us to read without blocking, or "`hang`ing", when there are no further chars to read on the incoming stream. Instead of waiting on further input like plain `read-char`, `read-char-no-hang` just returns `nil` immediately. We've also passed two additional arguments that mean it'll return `:eof` rather than erroring at the end-of-file marker. That should explain why we were just throwing away buffers and sockets that came back with an `:eof` result; it's because an `:eof` here means that the client socket has closed its stream, so there's no reason to send a reply back.

The `buffer` class looks like

	(defclass buffer ()
	  ((tries :accessor tries :initform 0)
	   (contents :accessor contents :initform nil)
	   (bi-stream :reader bi-stream :initarg :bi-stream)
	   (found-crlf? :accessor found-crlf? :initform nil)
	   (content-size :accessor content-size :initform 0)
	   (started :reader started :initform (get-universal-time))))

It's just a series of storage slots to track buffering state from the incoming socket; nothing interesting whatsoever, unless you're new to Common Lisp. If you _are_ just joining us from mainstream OO languages, you might notice the odd fact that these CLOS (Common Lisp Object System) `class` declarations only involve slots and related getters/setters, or `reader`s/`accessor`s in CL terms, and initial-value-related options (`:initform` specifies a default value, while `:initarg` specifies a hook for the caller of `make-instance` to provide a default value). This is because the Lisp object system is based on generic functions.

## A Brief Detour through CLOS

Basically, at a very high level, you need to think "Methods specialize on classes" rather than "classes have methods". That should get you most of the way to understanding.

From a theoretical perspective, the class-focused approach and the function-focused approach can be seen as perpendicular approaches to the same problem. Namely

> How do we treat different classes similarly for the purposes of certain operations that they have in common?

The most common concrete example is the various number implementations. No, an integer is not the same as a real number is not the same as a complex number and so forth, *but*, you can add, multiply, divide and so on each of those. And it'd be nice if you could just express the idea of addition without having to name separate operations for different types when each of them amounts to the same conceptual procedure. The class-focused approach says

> You have different classes you need to deal with. Each such class implements the appropriate methods you want supported.

See [Smalltalk](http://pharo.org/) for the prototypical example of this kind of system. The function-focused approach says

> You have a number of generic operations that can deal with multiple types. When you call one, it dispatches on the types of its arguments to see what concrete implementation it should apply.

That's basically what you'll see in action in Common Lisp. You can think about it as a giant table with "Class Name" down the first column and "Operation" across the first row

            | Addition | Subtraction | Multiplication |
    ----------------------------------------------------------...
    Integer |          |             |                |
	-----------------------------------------------------
	Real    |          |             |                |
	-----------------------------------------------
	Complex |          |             |

and each cell representing the implementation of that operation for that type. Class-focused OO says "Focus on the first column; the class is the important part", function-focused OO says "focus on the first row; the operation needs to be central". Consequently, CF-OO systems tend to group all methods related to a class in with that class' data, whereas FF-OO systems tend to isolate the data completely and group all implementations of an operation together. In the first system, it's difficult to ask "what classes implement method `foo`?" which is easy in the second, but the second has similar problems answering "what are all the methods that specialize on class `bar`?". In a way those questions don't make sense from within the systems we're asking them, and understanding why that is will give you some insight into where you want one or the other.

Bringing this back around to our `request`s, the class we defined earlier has three slots

- `resource`, which is the URI the client wants, less `GET` parameters and absolute host
- `headers`, which is the set of HTTP headers we parsed out of the incoming TCP stream
- `parameters`, which is the parsed list of all `GET` and `POST` parameters sent by the client

## End of Detour in 4. 3. 2. 

The next interesting part of `process-ready` comes after the edge-case handling of old/big/needy requests. It's wrapped in another layer of error handling because we might still crap out in different ways here. In particular, if the request is old/big/needy or if an `http-assertion-error` is raised, we want to send a `400` response; the client provided us with some bad or slow data. However, if any _other_ error happens here, it's because someone made a mistake defining a handler, which should be treated as a `500` error. Something went wrong on the server side as a result of a potentially legitimate request.

Lets follow that trail for a while:

	(defmethod handle-request ((socket usocket) (req request))
	  (aif (lookup (resource req) *handlers*)
	       (funcall it socket (parameters req))
	       (error! +404+ socket)))

In `handle-request`, we do the tiny and obvious job of looking up the requested resource in the `*handlers*` table. If we find one, we `funcall` `it`, passing along the client `socket` as well as the parsed request parameters. If there's no matching handler in the `*handlers*` table, we instead send along a `404` error. Following that `lookup` down into the `*handlers*` table is going to drop us down the `macro` rabbit hole though, so lets quickly take a look at the parsing, writing and the key `subscribe`/`publish` system before moving on. If you're looking to learn about macros specifically, and are hardcore enough to jump straight in, skip ahead to the `define-handler` section. Not that the long way will be easy-going, mind you.

Here's how we parse requests

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
		    (setf (parameters req)
		          (append (parse-params parameters)
			              (parse-params (pop lines))))
		    req))))

	(defmethod parse ((buf buffer))
	  (parse (coerce (reverse (contents buf)) 'string)))

	(defmethod parse-params ((params null)) nil)
	(defmethod parse-params ((params string))
	  (loop for pair in (split "&" params)
	        for (name val) = (split "=" pair)
	        collect (cons (->keyword name) (or val ""))))

The top two methods handle parsing buffers or strings, while the bottom two handle parsing HTTP parameters. There are two places where we might expect something with the shape of ampersand-separated k/v pairs, so it seemed like a good idea to pull out the procedure that handles them. The `parse` method specializing on `buffer` just pulls out the `buffer`s contents, and recursively calls `parse` on its reversed, stringified `contents`. In the `parse` method specializing on `string`, we actually take apart the incoming content into usable pieces. The process is

1. split on `"\\r?\\n"`
2. split the first line of that on `" "` to get the request type (`POST`, `GET`, etc)/URI path/http-version
3. assert that we're dealing with an `HTTP/1.1` request
4. split the URI path on `"?"`, which gives us plain resource separate from any potential `GET` parameters
5. make a new `request` instance with the resource in place
6. populate that `request` instance with each split header line
7. set that `request`s parameters to the result of parsing our `GET` and `POST` parameters

The `request` object from step five looks exactly how you'd expect after our CLOS detour.

	(defclass request ()
	  ((resource :accessor resource :initarg :resource)
	   (headers :accessor headers :initarg :headers :initform nil)
	   (parameters :accessor parameters :initarg :parameters :initform nil)))

The only place this particular `class` gets specialized on is in `handle-request`, which you saw earlier. Now, before we take a look at how we get handlers into our `*handlers*` table, lets skip ahead a bit and see something that every handler is going to have to do; write a response.

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

You can see that this operation takes a `response` and a `usocket`, grabbing a stream from the `usocket` and writing a bunch of lines to it. We locally define the function `write-ln` which takes some number of sequences, and writes them out to the stream followed by a `crlf`. That's just for readability; we could easily have done manual `write-sequence`/`crlf` calls. A `response` is another class, like `request`.

	(defclass response ()
	  ((content-type :accessor content-type :initform "text/html" :initarg :content-type)
	   (charset :accessor charset :initform "utf-8")
	   (response-code :accessor response-code :initform "200 OK" :initarg :response-code)
	   (keep-alive? :accessor keep-alive? :initform nil :initarg :keep-alive?)
	   (body :accessor body :initform nil :initarg :body)))

Which contains all the information we needed to write out a TCP response for our client back in `write!`.

-`content-type` is a mime-type for the thing this handler will be returning, It'll most commonly be `text/html`, which is why that's the default. Other common values include `application/json` and `text/plain`.
-`charset` is the character encoding the page uses, which'll always be `utf-8` as far as I know.
-`response-code` is an [HTTP response code](https://en.wikipedia.org/wiki/List_of_HTTP_status_codes). A successful result is `200 OK`. We'll see the common errors covered later.
-`keep-alive?` is a flag that tells us whether to keep the connection active or not. In the context of `:house`, it's only used on stream handlers.
-`body` is hopefully self explanatory.

Now, because we want to be able to send updates to clients between requests, we want to specialize `write!` on another class

	(defmethod write! ((res sse) (sock usocket))
	  (let ((stream (flex-stream sock)))
	    (format stream "~@[id: ~a~%~]~@[event: ~a~%~]~@[retry: ~a~%~]data: ~a~%~%"
		    (id res) (event res) (retry res) (data res))))

which will need to hold all the information we need to send an incremental update to our clients using [SSE messages](http://www.w3.org/TR/eventsource/)

	(defclass sse ()
	  ((id :reader id :initarg :id :initform nil)
	   (event :reader event :initarg :event :initform nil)
       (retry :reader retry :initarg :retry :initform nil)
	   (data :reader data :initarg :data)))

Writing an `SSE` out is conceptually similar to, but mechanically different from writing out a `response`. It's much simpler, because an `SSE` only has four slots, one of which is mandatory. Also, the SSE message standard doesn't specify `CRLF` line-endings, so we can get away with a single `format` call instead of the fairly involved process of `write!`ing a full HTTP request. The `~@[...~]` blocks are conditional directives. Which is to say, if `(id res)` is non-nil, we'll output `id: <the id here> `, and ditto for `event` and `retry`. `data`, the payload of our incremental update, is the only slot that's always present.

While we're on the subject of sending `SSE` updates, we also need a way to keep track of who to send them to. This is because the purpose of such updates in our case is to notify watchers of the actions of other players. The simplest way of doing this is keeping track of channels, to which we'll subscribe `socket`s as necessary.

	(defparameter *channels* (make-hash-table))

	(defmethod subscribe! ((channel symbol) (sock usocket))
	  (push sock (gethash channel *channels*))
	  nil)

We can then `publish!` notifications to said channels as soon as they become available.

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

The `publish!` method will be called with a channel symbol and a message whenever we have something new to publish to a particular channel. It looks up all the listeners of that channel, iterates over each of them trying to write the update out, and collects each socket that is successfully written to (sockets that _weren't_ successfully written to are no longer listening, so we don't have to care). Now that we know about the event-loop core, the parsing step, the writing step, and the idea behind subscribing/publishing notifications, it's time to pull it all together.

## Defining Handlers

We'll want to be able to write things like

    (define-handler (source :close-socket? nil) (room)
       (subscribe! (intern room :keyword) sock))

    (define-handler (send-message) (room name message)
       (publish! (intern room :keyword) (encode-json-to-string `((:name . ,name) (:message . ,message)))))

    (define-handler (index) ()
       (with-html-output-to-string (s nil :prologue t :indent t)
         (:html
           (:head (:script :type "text/javascript" :src "/static/js/interface.js"))
           (:body (:div :id "messages")
	              (:textarea :id "input")
	              (:button :id "send" "Send")))))


and have them mean the obvious. Actually, because we'd ostensibly be processing requests from untrusted sources, we'd want to write something more like

    (defun len-between (min thing max)
	  (>= max (length thing) min))

    (define-handler (source :close-socket? nil) ((room :string (len-between 0 room 16)))
       (subscribe! (intern room :keyword) sock))

    (define-handler (send-message)
	    ((room :string (len-between 0 room 16))
	     (name :string (len-between 1 name 64))
	     (message :string (len-between 5 message 256)))
       (publish! (intern room :keyword) (encode-json-to-string `((:name . ,name) (:message . ,message)))))

    (define-handler (index) ()
       (with-html-output-to-string (s nil :prologue t :indent t)
         (:html
           (:head (:script :type "text/javascript" :src "/static/js/interface.js"))
           (:body (:div :id "messages")
	              (:textarea :id "input")
	              (:button :id "send" "Send")))))

and get the validation out of the way in the same stroke. It's not *quite* strongly typed HTTP parameters, because I'm interested in enforcing more than type, but that's a good first approximation. You can imagine more or less this same thing being implemented in a mainstream class-based OO language using a class hierarchy. That is, you'd define a `handler` class, then subclass that for each handler you have, giving each `get`, `post`, `parse` and `validate` methods as needed. If you imagine this well enough, you'll also see the small but non-trivial pieces of boilerplate that the approach would get you, both in terms of setting up classes and methods themselves and in terms of doing the parsing/validation of your parameters. The Common Lisp approach, and I'd argue the right approach, is to write a DSL to handle the problem. In this case, it takes the form of a new piece of syntax that lets you declare certain properties of your handlers, and expands into the code you would have written by hand. Writing this way, your code ends up amounting to a set of instructions which a Lisp implementation can unfold into the much more verbose and extensive code that you want to run. The benefit here is that you don't have to maintain the intermediate code, as you would if you were using IDE/editor-provided code generation facilities, you have the comparably easy and straight-forward task of maintaining the unfolding instructions.

Lets step through the expansion for `send-message`, just so you understand what's going on. What I'm about to show you is the output of the SLIME macro-expander, which does a one-level expansion on the macro call you give it.

    (define-handler (send-message)
	    ((room :string (len-between 0 room 16))
	     (name :string (len-between 1 name 64))
	     (message :string (len-between 5 message 256)))
       (publish! (intern room :keyword) (encode-json-to-string `((:name . ,name) (:message . ,message)))))

No big deal; that's just what I want to write. What I want this to mean is

> "Bind the action `(publish! ...)` to the URI `/send-message` in the handlers table. Before you run that action, make sure the client has passed us parameters named `room`, `name` and `message`, ensure that `room` is a string no longer than 16 characters, `name` is a string of between 1 and 64 characters (inclusive) and finally that `message` is a string of between 5 and 256 characters (also inclusive). After you've sent the response back, close the channel.".

Expanding it will get us

    (BIND-HANDLER SEND-MESSAGE
       (MAKE-CLOSING-HANDLER (:CONTENT-TYPE "text/html")
           ((ROOM :STRING (LEN-BETWEEN 0 ROOM 16))
            (NAME :STRING (LEN-BETWEEN 1 NAME 64))
            (MESSAGE :STRING (LEN-BETWEEN 5 MESSAGE 256)))
         (PUBLISH! (INTERN ROOM :KEYWORD)
                   (ENCODE-JSON-TO-STRING `((:NAME ,@NAME) (:MESSAGE ,@MESSAGE))))))

We're binding the result of `make-closing-handler` to the (for now) symbol `send-message`. Expanding `bind-handler` gets us

    (PROGN
     (WHEN (GETHASH "/send-message" *HANDLERS*)
       (WARN "Redefining handler '/send-message'"))
     (SETF (GETHASH "/send-message" *HANDLERS*)
             (MAKE-CLOSING-HANDLER (:CONTENT-TYPE "text/html")
                 ((ROOM :STRING (LEN-BETWEEN 0 ROOM 16))
                  (NAME :STRING (LEN-BETWEEN 1 NAME 64))
                  (MESSAGE :STRING (LEN-BETWEEN 5 MESSAGE 256)))
               (PUBLISH! (INTERN ROOM :KEYWORD)
                         (ENCODE-JSON-TO-STRING
                          `((:NAME ,@NAME) (:MESSAGE ,@MESSAGE)))))))

Which is to say, we'd like to associate the handler we're making with the URI `/send-message` in the handler table `*HANDLERS*`. We'd additionally like a warning to be issued if that binding already exists, but will re-bind it regardless. None of that is particularly interesting. Lets take a look at the expansion of `make-closing-handler` specifically:

    (LAMBDA (SOCK #:COOKIE?1110 SESSION PARAMETERS)
      (DECLARE (IGNORABLE SESSION PARAMETERS))
      (LET ((ROOM
    	 (AIF (CDR (ASSOC :ROOM PARAMETERS)) (URI-DECODE IT)
    	      (ERROR (MAKE-INSTANCE 'HTTP-ASSERTION-ERROR :ASSERTION 'ROOM)))))
        (ASSERT-HTTP (LEN-BETWEEN 0 ROOM 16))
        (LET ((NAME
    	   (AIF (CDR (ASSOC :NAME PARAMETERS)) (URI-DECODE IT)
    		(ERROR
    		 (MAKE-INSTANCE 'HTTP-ASSERTION-ERROR :ASSERTION 'NAME)))))
          (ASSERT-HTTP (LEN-BETWEEN 1 NAME 64))
          (LET ((MESSAGE
    	     (AIF (CDR (ASSOC :MESSAGE PARAMETERS)) (URI-DECODE IT)
    		  (ERROR
    		   (MAKE-INSTANCE 'HTTP-ASSERTION-ERROR :ASSERTION 'MESSAGE)))))
    	(ASSERT-HTTP (LEN-BETWEEN 5 MESSAGE 256))
    	(LET ((RES
    	       (MAKE-INSTANCE 'RESPONSE :CONTENT-TYPE "text/html" :COOKIE
    			      (UNLESS #:COOKIE?1110 (TOKEN SESSION)) :BODY
    			      (PROGN
    				(PUBLISH! (INTERN ROOM :KEYWORD)
    					  (ENCODE-JSON-TO-STRING
    					   `((:NAME ,@NAME)
    					     (:MESSAGE ,@MESSAGE))))))))
    	  (WRITE! RES SOCK)
    	  (SOCKET-CLOSE SOCK))))))

This is the big one. It looks mean, but it really amounts to an unrolled loop over the arguments. You can see that for every parameter, we're grabbing its value in the `parameters` association list, ensuring it exists, `uri-decode`ing `it` if it does, and asserting the appropriate properties we want to enforce. At any given point, if an assertion is violated, we're done and we return an error (handling said error not pictured here, but the error handlers surrounding an HTTP handler call will ensure that these errors get translated to `HTTP 400` or `500` errors over the wire). If we get through all of our arguments without running into an error, we're going to evaluate the handler body, write the result out to the requester and close the socket.

## Understanding the Expanders

The top-level form we'll want to write is defined as

    (defmacro define-handler ((name &key (close-socket? t) (content-type "text/html")) (&rest args) &body body)
      (if close-socket?
          `(bind-handler ,name (make-closing-handler (:content-type ,content-type) ,args ,@body))
          `(bind-handler ,name (make-stream-handler ,args ,@body))))

It just almost-straight-forwardly expands into a `bind-handler` and a `make-closing-handler` or `make-stream-handler` as appropriate. You can see that, because we're using homo-iconic code, we can use the backtick and comma operators to basically cut holes in an expression we'd like to evaluate. Calling the resulting macros will slot values into said holes and evaluate the result.

Next up, `bind-handler`

	(defmacro bind-handler (name handler)
	  (assert (symbolp name) nil "`name` must be a symbol")
	  (let ((uri (if (eq name 'root) "/" (format nil "/~(~a~)" name))))
	    `(progn
	       (when (gethash ,uri *handlers*)
		 (warn ,(format nil "Redefining handler '~a'" uri)))
	       (setf (gethash ,uri *handlers*) ,handler))))

takes a symbol and a handler, and binds the handler to the URI it creates by prepending "/" to the lower-cased symbol-name of that symbol (that's what the `format` call does). The binding happens in the last line; `(setf (gethash ,uri *handlers*) ,handler)`, which is what hash-table assignments look like in Common Lisp (modulo the commas, of course). This is another level that you can fairly straight-forwardly map to its expansion above. Note that the first assertion here is outside of the quoted area, which means that it'll be run as soon as the macro is called rather than when its result is evaluated.

Next up, lets take a look at `make-closing-handler`. We'll take a look at `make-stream-handler` too, but I want to start with the one whose expansion you've already seen.

	(defmacro make-closing-handler ((&key (content-type "text/html")) (&rest args) &body body)
	  `(lambda (sock parameters)
	     (declare (ignorable parameters))
	     ,(arguments args
			 `(let ((res (make-instance 
				      'response 
				      :content-type ,content-type 
				      :body (progn ,@body))))
			    (write! res sock)
			    (socket-close sock)))))

So making a closing-handler involves making a `lambda`, which is just what you call anonymous functions in Common Lisp. We also set up an interior scope that makes a `response` out of the `body` argument we're passing in, `write!`s that to the requesting socket, then closes it. The remaining question is, what is `arguments`?

	(defun arguments (args body)
	  (loop with res = body
	     for arg in args
	     do (match arg
		  ((guard arg-sym (symbolp arg-sym))
		   (setf res `(let ((,arg-sym ,(arg-exp arg-sym)))
				,res)))
		  ((list* arg-sym type restrictions)
		   (setf res
			 `(let ((,arg-sym ,(or (type-expression (arg-exp arg-sym) type restrictions) (arg-exp arg-sym))))
			    ,@(awhen (type-assertion arg-sym type restrictions) `((assert-http ,it)))
			    ,res))))
	     finally (return res)))

Welcome to the hard part. `arguments` takes the handlers' arguments, and generates that tree of parse attempts and assertions you saw in the full macro-expansion of `send-message`. In other words, it takes

	(define-closing-handler (send-message)
	    ((room :string (>= 16 (length room)))           ;; < the arguments
	     (name :string (>= 64 (length name) 1))         ;; <
	     (message :string (>= 256 (length message) 5))) ;; <
	  (publish! (intern room :keyword) (encode-json-to-string `((:name . ,name) (:message . ,message)))))
    ;;^^^^^^^^^^^^^^^^^^^^^ and the body ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
	
and wraps `the body` in

	(LAMBDA (SOCK #:COOKIE?1111 SESSION PARAMETERS)
	           (DECLARE (IGNORABLE SESSION PARAMETERS))
	           (LET ((ROOM                                                                   ;; < these conversions/assertions
	                  (AIF (CDR (ASSOC :ROOM PARAMETERS)) (URI-DECODE IT)                    ;; <
	                       (ERROR (MAKE-INSTANCE 'HTTP-ASSERTION-ERROR :ASSERTION 'ROOM))))) ;; <
	             (ASSERT-HTTP (>= 16 (LENGTH ROOM)))                                         ;; <
	             (LET ((NAME                                                                 ;; <
	                    (AIF (CDR (ASSOC :NAME PARAMETERS)) (URI-DECODE IT)                  ;; <
	                         (ERROR                                                          ;; <
	                          (MAKE-INSTANCE 'HTTP-ASSERTION-ERROR :ASSERTION 'NAME)))))     ;; <
	               (ASSERT-HTTP (>= 64 (LENGTH NAME) 1))                                     ;; <
	               (LET ((MESSAGE                                                            ;; <
	                      (AIF (CDR (ASSOC :MESSAGE PARAMETERS)) (URI-DECODE IT)             ;; <
	                           (ERROR                                                        ;; <
	                            (MAKE-INSTANCE 'HTTP-ASSERTION-ERROR :ASSERTION 'MESSAGE)))));; <
	                 (ASSERT-HTTP (>= 256 (LENGTH MESSAGE) 5))                               ;; <
	                 (LET ((RES
	                        (MAKE-INSTANCE 'RESPONSE :CONTENT-TYPE "text/html" :COOKIE
	                                       (UNLESS #:COOKIE?1111 (TOKEN SESSION)) :BODY
	                                       (PROGN
	                                        (PUBLISH! (INTERN ROOM :KEYWORD)
	                                                  (ENCODE-JSON-TO-STRING
	                                                   `((:NAME ,@NAME)
	                                                     (:MESSAGE ,@MESSAGE))))))))
	                   (WRITE! RES SOCK)
	                   (SOCKET-CLOSE SOCK))))))

that. Here's an evaluation from a REPL:

	HOUSE> (arguments '((room :string (>= 16 (length room))) (name :string (>= 64 (length name) 1)) (message :string (>= 256 (length message) 5))) :body-placeholder)
	(LET ((ROOM
	       (AIF (CDR (ASSOC :ROOM PARAMETERS)) (URI-DECODE IT)
	            (ERROR (MAKE-INSTANCE 'HTTP-ASSERTION-ERROR :ASSERTION 'ROOM)))))
	  (ASSERT-HTTP (>= 16 (LENGTH ROOM)))
	  (LET ((NAME
	         (AIF (CDR (ASSOC :NAME PARAMETERS)) (URI-DECODE IT)
	              (ERROR (MAKE-INSTANCE 'HTTP-ASSERTION-ERROR :ASSERTION 'NAME)))))
	    (ASSERT-HTTP (>= 64 (LENGTH NAME) 1))
	    (LET ((MESSAGE
	           (AIF (CDR (ASSOC :MESSAGE PARAMETERS)) (URI-DECODE IT)
	                (ERROR
	                 (MAKE-INSTANCE 'HTTP-ASSERTION-ERROR :ASSERTION 'MESSAGE)))))
	      (ASSERT-HTTP (>= 256 (LENGTH MESSAGE) 5))
	      :BODY-PLACEHOLDER)))
	HOUSE>

The `match` clause inside `arguments` distinguishes between symbol arguments and list arguments, which lets you have untyped arguments in handlers. For instance, if you knew you could trust your users not to pick gigantic names, you could do this:

    (define-closing-handler (send-message) ((room :string (>= 16 (length room))) name (message :string (>= 256 (length message) 5)))
       (publish! (intern room :keyword) (encode-json-to-string `((:name . ,name) (:message . ,message)))))

The appropriate `arguments` call would then just check for the *presence* of a `name` parameter rather than asserting anything about its contents.

	HOUSE> (arguments '((room :string (>= 16 (length room))) name (message :string (>= 256 (length message) 5))) :body-placeholder)
	(LET ((ROOM
	       (AIF (CDR (ASSOC :ROOM PARAMETERS)) (URI-DECODE IT)
	            (ERROR (MAKE-INSTANCE 'HTTP-ASSERTION-ERROR :ASSERTION 'ROOM)))))
	  (ASSERT-HTTP (>= 16 (LENGTH ROOM)))
	  (LET ((NAME
	         (AIF (CDR (ASSOC :NAME PARAMETERS)) (URI-DECODE IT)
	              (ERROR (MAKE-INSTANCE 'HTTP-ASSERTION-ERROR :ASSERTION 'NAME)))))
	    (LET ((MESSAGE
	           (AIF (CDR (ASSOC :MESSAGE PARAMETERS)) (URI-DECODE IT)
	                (ERROR
	                 (MAKE-INSTANCE 'HTTP-ASSERTION-ERROR :ASSERTION 'MESSAGE)))))
	      (ASSERT-HTTP (>= 256 (LENGTH MESSAGE) 5))
	      :BODY-PLACEHOLDER)))
	HOUSE> 

You should be able to map the result onto the expression with minimal effort at this point, but the specifics of how it converts a particular type might be eluding you if you haven't read ahead, or read the code yet. So lets dive into that before we move back to the other handler type. Three expressions matter here: `arg-exp`, `type-expression` and `type-assertion`. Once you understand those, there will be no magic left. Easy first.

`arg-exp` takes an argument symbol and returns that `aif` expression we use to check for the presence of a parameter. Just the symbol, not the restrictions.

	(defun arg-exp (arg-sym)
	  `(aif (cdr (assoc ,(->keyword arg-sym) parameters))
		(uri-decode it)
		(error (make-instance 'http-assertion-error :assertion ',arg-sym))))

the evaluation looks like

	HOUSE> (arg-exp 'room)
	(AIF (CDR (ASSOC :ROOM PARAMETERS)) (URI-DECODE IT)
	     (ERROR (MAKE-INSTANCE 'HTTP-ASSERTION-ERROR :ASSERTION 'ROOM)))
	HOUSE> 

## A Short Break -- Briefly Meditating on Macros

Lets take a short break here. At this point we're two levels deep into tree processing. And what we're doing will only make sense to you if you remember that Lisp code is itself represented as a tree. That's what the parentheses are for; they show you how leaves and branches fit together. If you step back, you'll realize we've got a macro definition, `make-closing-handler`, which calls a function, `arguments`, to generate part of the tree its constructing, which in turn calls some tree-manipulating helper functions, including `arg-exp`, to generate its return value. The tree that these functions have as input *happen* to represent Lisp code, and because there's no difference between Lisp code and a tree, you have a transparent syntax definition system. The input is a Lisp expression, and the output is a lisp expression that will be evaluated in its place. Possibly the simplest way of conceptualizing this is as a very simple and minimal Common Lisp to Common Lisp compiler.

### Another Short Break -- Briefly Meditating on Anaphoric Macros

A particularly widely used, and particularly simple group of such compilers are called *anaphoric macros*. You've seen `aif` already, and will later see `awhen` later on. Personally, I only tend to use those two with any frequency, but there's a fairly wide variety of them available in the [`anaphora` package](http://www.cliki.net/Anaphora). As far as I know, they were first defined by Paul Graham in an [OnLisp chapter](http://dunsmor.com/lisp/onlisp/onlisp_18.html). The use case he gives is a situation where you want to do some sort of expensive or semi-expensive check, then do something conditionally on the result. In the above context, we're using `aif` to do a check on the result of an `alist` traversal.

	(aif (cdr (assoc :room parameters))
	     (uri-decode it)
	     (error (make-instance 'http-assertion-error :assertion 'room)))

What this means is

> Take the `cdr` of looking up the symbol `:room` in the association list `parameters`. If that returns a non-nil value `uri-decode` it, otherwise throw an error of the type `http-assertion-error`.

In other words, the above is equivalent to

	(let ((it (cdr (assoc :room parameters))))
	  (if it
	      (uri-decode it)
	      (error (make-instance 'http-assertion-error :assertion 'room))))

In Haskell, you'd use `Maybe` in this situation. In Common Lisp, you take advantage of the lack of hygienic macros as above to trivially capture the symbol `it` in the expansion as the name for the result of the check. The reason I bring any of this up is that I lied to you recently.

> If you step back, you'll realize we've got a macro definition, `make-closing-handler`, which calls a function, `arguments`, to generate part of the tree its constructing, which in turn calls some tree-manipulating helper functions, including `arg-exp`, to generate its return value. *-Me*

The tree hasn't bottomed out yet. In fact, by the time you get to `arg-exp`, you've still got at least two levels to go; `assert-http` and `make-instance` both expand into more primitive forms before getting evaluated. We'll be taking a look at `assert-http` later on, but I won't be expanding and explaining `make-instance`. If you're interested, you can get `SLIME` running and keep macro-expanding 'till you hit bottom. It may take a while.

Now lets get back to the point; expanding type annotations for HTTP handlers. And in order to plumb the depths of that mystery, we'll need to take a look at how we intend to *define* HTTP types.

## Defining HTTP Types

With the above macro-related tidbits, you should be able to see that `arg-exp` is actually doing the job of generating a specific, repetitive, piece of the code tree that we eventually want to evaluate. In this case, the piece that checks for the presence of the given parameter among the handlers' `parameters`. And that's all you need to understand about it, so lets move on to...

	(defgeneric type-expression (parameter type)
	  (:documentation
	   "A type-expression will tell the server how to convert a parameter from a string to a particular, necessary type."))
    ...
	(defmethod type-expression (parameter type) nil)

This is a *method* that generates new tree structures (coincidentally Lisp code), rather than just a function. And yes, you can do that just fine. The only thing the above tells you is that by default, a `type-expression` is `NIL`. Which is to say, we don't have one. If we encounter a `NIL`, we just use the output of `arg-exp` raw, but that doesn't tell us much about the usual case. To see that, lets take a look at a built-in (to `:house`) `define-http-type` expression.

	(define-http-type (:integer)
	    :type-expression `(parse-integer ,parameter :junk-allowed t)
		:type-assertion `(numberp ,parameter))

So an `:integer` is a thing that we're going to get out of a raw `parameter` by using `(parse-integer parameter :junk-allowed t)`, and we want to check whether the result is actually an integer (The Haskellers reading along will probably chuckle at this, but the best way of thinking about most lisp functions is as returning a `Maybe` because many of them signal failure by returning `NIL` rather than whatever they were going to return. `parse-integer` with `:junk-allowed` is one of these, so we need to check that its result is *actually* an integer before proceeding (This gives you some fun edge cases in places where `NIL` is part of the set of legitimately possible return values of a particular procedure. Examples are `gethash` and `getf`. I'm not going to get into that here, other than mentioning that you typically handle it by using multiple return values)). Here's the demonstration of the first part (we'll get to `type-assertion`s in a moment):

	HOUSE> (type-expression 'blah :integer)
	(PARSE-INTEGER BLAH :JUNK-ALLOWED T)
	HOUSE> 

Now, I mentioned that some types are built-in to `:house`, but they're not being defined using Lisp primitives. In particular `define-http-type` is not a built-in.

	(defmacro define-http-type ((type) &key type-expression type-assertion)
	  (with-gensyms (tp)
	    `(let ((,tp ,type))
	       ,@(when type-expression
		       `((defmethod type-expression (parameter (type (eql ,tp)))
			   ,type-expression)))
	       ,@(when type-assertion
		       `((defmethod type-assertion (parameter (type (eql ,tp)))
			   ,type-assertion))))))

Incidentally, this is one fugly looking macro primarily because it aims to have readable output. Which means getting rid of potential `NIL`s by expanding them away using `,@` where possible. Double incidentally, this macro *is* one of the exported symbols for `house`; the point is that a `house` user could define their own to simplify parsing more than `:string`, `:integer`, `:keyword`, `:json`, `:list-of-keyword` and `:list-of-integer`. All it does is expand into the appropriate `type-expression` and `type-assertion` method definitions for the type you're looking to define. You could, in fact, do this manually if you liked, but that would mean directly interacting with the method definitions. Adding this extra level of indirection lets you potentially change the representation away from its current form without forcing any users to re-write their specifications. This isn't an academic consideration either; I've changed the implementation three times in fairly radical ways over the course of the `:house` project and had to make very few edits to applications that depend use it as a direct result of that extra macro layer. Lets take a look at the expansion of that integer definition, just to drive the point home.

	(LET ((#:TP1288 :INTEGER))
	  (DEFMETHOD TYPE-EXPRESSION (PARAMETER (TYPE (EQL #:TP1288)))
	    `(PARSE-INTEGER ,PARAMETER :JUNK-ALLOWED T))
	  (DEFMETHOD TYPE-ASSERTION (PARAMETER (TYPE (EQL #:TP1288)))
	    `(NUMBERP ,PARAMETER)))

Like I said, it doesn't actually save you much typing, but does prevent you from needing to care what the specific parameters of those methods are, or even that they're methods at all.

Anyhow, having gone through all that, the purpose of `type-assertion` should be fairly obvious. It's the *other* half of input sanitation, namely ensuring that the result of a parse satisfies some basic requirements. And it takes the form of a complementary `defgeneric`/`defmethod` pair to `type-expression`

	(defgeneric type-assertion (parameter type)
	  (:documentation
	   "A lookup assertion is run on a parameter immediately after conversion. Use it to restrict the space of a particular parameter."))
	...
	(defmethod type-assertion (parameter type) nil)

Here's what this one outputs

	HOUSE> (type-assertion 'blah :integer)
	(NUMBERP BLAH)
	HOUSE> 

Sometimes, `type-assertion` wouldn't bother asserting anything. In particular, since the incoming parameters are going to be strings (if they're passed in at all), by default we don't check anything for `:string` parameters other than their presence.

	HOUSE> (type-assertion 'blah :string)
	NIL
	HOUSE> 

You should now understand exactly why `arguments` works the way it does. Just to reiterate:

	HOUSE> (arguments '((blah :integer (>= 12 blah 4))) :body-placeholder)
	(LET ((BLAH
	       (PARSE-INTEGER
	        (AIF (CDR (ASSOC :BLAH PARAMETERS)) (URI-DECODE IT)
	             (ERROR (MAKE-INSTANCE 'HTTP-ASSERTION-ERROR :ASSERTION 'BLAH)))
	        :JUNK-ALLOWED T)))
	  (ASSERT-HTTP (NUMBERP BLAH))
	  (ASSERT-HTTP (>= 12 BLAH 4))
	  :BODY-PLACEHOLDER)
	HOUSE> 

Last part before we conclude this section, `make-stream-handler` does the same basic thing as `make-closing-handler`. Except it'll write an `SSE` rather than a `RESPONSE`, and it calls `force-output` instead of `socket-close` because we want to push bytes down the pipe, but don't want to close it out entirely in this context.

	(defmacro make-stream-handler ((&rest args) &body body)
	  `(lambda (sock parameters)
	     (declare (ignorable parameters))
	     ,(arguments args
			 `(let ((res (progn ,@body)))
			    (write! (make-instance 'response
						   :keep-alive? t :content-type "text/event-stream")
				    sock)
			    (write! (make-instance 'sse :data (or res "Listening...")) sock)
			    (force-output (socket-stream sock))))))

That's the entirety of the handler subsystem for this project. What we've got is

-a table of handlers indexed by URI internally
-a user-facing DSL for easily creating type and restriction-annotated handlers
-a user-facing micro-DSL for easily defining new types to annotate handlers with

There's just one more chunk to put together.

## Error Handling

	(define-condition http-assertion-error (error)
	  ((assertion :initarg :assertion :initform nil :reader assertion))
	  (:report (lambda (condition stream)
		     (format stream "Failed assertions '~s'"
			     (assertion condition)))))
	
	(defmacro assert-http (assertion)
	  `(unless ,assertion
	     (error (make-instance 'http-assertion-error :assertion ',assertion))))

This is how you define a new error class in Common Lisp. You call `define-condition` (which you can think of as a variant of `defclass`), inherit from `error`, and hand it some options specific to your `error`. In this case, I'm defining an HTTP assertion error, and the only specific things it'll need to know are the actual assertion it's acting on, and a specific way to output itself to a stream. In other languages, you'd call this a method. Here, it's just a function that happens to be the slot value of a class.

The accompanying `assert-http` macro lets you avoid the minor boilerplate associated with asserting for this specific type of error. It just expands into a check of the given assertion, throws an `http-assertion-error` if it fails, and packs the original assertion along in that event. The other chunklet of our error model has to do with how we represent errors to the client. And that's at the bottom of the same file.

	(defparameter +404+
	  (make-instance 'response :response-code "404 Not Found"
			 :content-type "text/plain" :body "Resource not found..."))
	
	(defparameter +400+
	  (make-instance 'response :response-code "400 Bad Request"
			 :content-type "text/plain" :body "Malformed, or slow HTTP request..."))
	
	(defparameter +413+
	  (make-instance 'response :response-code "413 Request Entity Too Large"
			 :content-type "text/plain" :body "Your request is too long..."))
	
	(defparameter +500+
	  (make-instance 'response :response-code "500 Internal Server Error"
			 :content-type "text/plain" :body "Something went wrong on our end..."))

These are the relevant `4xx` and `5xx`-class HTTP errors that we'll be sending around commonly enough that we just want them globally declared. You can see the `+400+` response that we fed through `error!` up at the top there. It's just an HTTP `response` with a particular `response-code`. I probably could have written a macro to abstract the common parts away, but didn't feel the need for it at the time. You can treat is as an exercise, if you like. Just to confirm that what you thought was happening is actually happening, lets take a look at one more relevant method from the core.

	(defmethod error! ((err response) (sock usocket) &optional instance)
	  (declare (ignorable instance))
	  (ignore-errors 
	    (write! err sock)
	    (socket-close sock)))

It takes an error response and a socket, writes the response to the socket and closes it (ignoring errors, in case the other end has already disconnected). The `instance` argument here is purely for logging/debugging purposes. We'll get into that later.

## All Together Now

That did it. We can now finally write

    (defun len-between (min thing max)
	  (>= max (length thing) min))

    (define-handler (source :close-socket? nil) ((room :string (len-between 0 room 16)))
       (subscribe! (intern room :keyword) sock))

    (define-handler (send-message)
	    ((room :string (len-between 0 room 16))
	     (name :string (len-between 1 name 64))
	     (message :string (len-between 5 message 256)))
       (publish! (intern room :keyword) (encode-json-to-string `((:name . ,name) (:message . ,message)))))

    (define-handler (index) ()
       (with-html-output-to-string (s nil :prologue t :indent t)
         (:html
           (:head (:script :type "text/javascript" :src "/static/js/interface.js"))
           (:body (:div :id "messages")
	              (:textarea :id "input")
	              (:button :id "send" "Send")))))

    (start 4242)

Once you fill in the `interface.js` piece, this will in fact start an HTTP chat server on port `4242` and listen for incoming connections, handling them all appropriately.

And you know exactly how it's happening, down to the sockets.

[[TODO: Take a crack at putting together a light JS UI so that users can actually run this.]]

## Bonus Stage

I mentioned we'd get to the debug component, and figured I'd go over it in the epilogue. This article is probably going to be heavy going for people not already familiar with Lisp, so I didn't want to weigh it down further. Over the course of writing this server, I periodically had to diagnose various low-level problems, but definitely didn't want to have those debug statements make it out into a deployment. Because I wrote most of the functionality as methods, I was able to take advantage of a particular minor feature of CLOS to cluster all of my debugging-related `printf`s into the `debug!` procedure in `util.lisp`.

	(defun debug! (&optional (stream *standard-output*))
	  (flet ((dbg (label &rest msg) (format stream ">>>> ~a~%~{~s~%----------~%~}~%" label msg)))
	    (defmethod process-ready :before ((sock stream-usocket) conns buffers)
		       (dbg "Preparing to buffer..." sock
			    "CONNECTIONS: " (alexandria:hash-table-keys conns)
			    "BUFFERS: " (alexandria:hash-table-keys buffers)))
	    (defmethod handle-request :before (sock req) 
		       (dbg "Handling request..." sock req (resource req) (headers req) (parameters req)))
	    (defmethod handle-request :after (sock req) 
		       (dbg "Completed request..."))
	    (defmethod buffer! :before (buf)
		       (dbg "Buffering..." buf (tries buf)))
	    (defmethod buffer! :after (buf)
		       (when (> (tries buf) +max-buffer-tries+)
			 (dbg "Needy buffer..." buf (tries buf) (coerce (reverse (contents buf)) 'string))))
	    (defmethod write! :before ((res response) sock) 
		       (dbg "Writing response..."))
	    (defmethod error! :before (res sock &optional instance) 
		       (dbg "Sending error response..."
			    instance sock res (response-code res)))
	    (defmethod subscribe! :before (chan sock) 
		       (dbg "New subscriber" chan))
	    (defmethod publish! :before (chan msg) 
		       (dbg "Publishing to channel" chan msg))
	    nil))

The feature is `:before`/`:after` hooks. And I guess the built-in language feature of a `defmethod` that can be run somewhere other than the top-level, but I sort of take that for granted these days. Defining a `:before` hook on a particular method lets you specify a bit of code that'll be executed before every call to that method. `:after` is similar, except the stuff you specify happens after the main method is called. You can specialize these `:before`/`:after` hooks as arbitrarily as the main methods, and only the relevant one will actually run. One possible use for this is the above. If you take a closer look at the `error!` `:before` method, you'll see where we're using the `instance` optional argument. This is the only place, and its only use is making `:house` debug logs more easily readable.

As I mentioned, most of the `:house` server is written using `defmethod`, which means I have plenty of places to hook up debugging/logging statements so I can see what's going on. Clustering all such print statements inside of the `debug!` procedures means that the rest of my code gets to stay unchanged when I need to add a `printf` somewhere, and it means that the `printf`s don't get run unless I specifically ask for them by calling `(debug!)` at some point in my session.
