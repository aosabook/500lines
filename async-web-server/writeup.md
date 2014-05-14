##### Draft

Ok, this was going to start off with the basics of threaded vs asynchronous servers, and a quick rundown of why I think the latter are better, but its been brought to my attention that the whole Common Lisp thing might be intimidating to people. I debated both internally and externally on how to actually begin, and decided that the best way might actually be from the end (Just to be clear, yes, this is a toy example. If you'd like to see a non-toy example using the same server, take a look at [cl-notebook](https://github.com/Inaimathi/cl-notebook) or [deal](https://github.com/Inaimathi/deal). And on a related note This writeup also features a stripped-down version of `house`. [The real version](https://github.com/Inaimathi/house) also does some light static file serving and deals with sessions. That's it though.). So to *that* end, here's what I eventually want to be able to do:

    (define-stream-handler (source) (room)
       (subscribe! (intern room :keyword) sock))

    (define-closing-handler (index) ()
       <insert some javascript UI here>)

    (define-closing-handler (send-message) (room name message)
       (publish! (intern room :keyword) (encode-json-to-string `((:name . ,name) (:message . ,message)))))

    (start 4242)

And having done that, I should be able to browse over to `localhost:4242/index` (actual front-end structure left as an exercise for the reader) and see a little chat room using which I could subscribe and post to various message channels. Because I don't hate myself, and I'd like this to actually work, I need to both do some sanitation on inputs and make sure that it needn't be done manually. So really, I want to be able to say something like

    (define-stream-handler (source) ((room :string :max 16))
       (subscribe! (intern room :keyword) sock))

    (define-closing-handler (index) ()
       <insert some javascript UI here>)

    (define-closing-handler (send-message) ((room :string :max 16) (name :string :min 1 :max 64) (message :string :min 5 :max 256))
       (publish! (intern room :keyword) (encode-json-to-string `((:name . ,name) (:message . ,message)))))

    (start 4242)

It's not *quite* strongly typed HTTP parameters, because I'm interested in enforcing more than the type. You can imagine more or less this same thing being done in a mainstream class-based OO language using a class hierarchy. That is, you'd define a `handler` class, then subclass that for each handler you have, giving each `get`, `post`, `parse` and `validate` methods as needed. If you imagine this well enough, you'll also see the small but non-trivial pieces of boilerplate that the approach would get you, both in terms of setting up classes and methods themselves and in terms of doing the parsing/validation of your parameters. The Common Lisp approach is to write a DSL to handle the situation. In this case, it takes the form of a new piece of syntax that lets you declare certain properties of your handlers, and expands into the code you would have written by hand.

Lets step through the expansion for `send-message`, just so you understand what's going on at each step.

    (define-closing-handler (send-message) ((room :string :max 16) (name :string :min 1 :max 64) (message :string :min 5 :max 256))
       (publish! (intern room :keyword) (encode-json-to-string `((:name . ,name) (:message . ,message)))))

No big deal; that's just what I want to write. Expanding it gets us

    (BIND-HANDLER SEND-MESSAGE
              (MAKE-CLOSING-HANDLER (:CONTENT-TYPE "text/html")
                  ((ROOM :STRING :MAX 16) (NAME :STRING :MIN 1 :MAX 64)
                   (MESSAGE :STRING :MIN 5 :MAX 256))
                (PUBLISH! (INTERN ROOM :KEYWORD)
                          (ENCODE-JSON-TO-STRING
                           `((:NAME ,@NAME) (:MESSAGE ,@MESSAGE))))))

We're binding the result of `make-closing-handler` to the (for now) symbol `send-message`. Expanding `bind-handler` gets us

	(PROGN
	 (WHEN (GETHASH "/send-message" *HANDLERS*)
	   (WARN "Redefining handler '/send-message'"))
	 (SETF (GETHASH "/send-message" *HANDLERS*)
	         (MAKE-CLOSING-HANDLER (:CONTENT-TYPE "text/html")
	             ((ROOM :STRING :MAX 16) (NAME :STRING :MIN 1 :MAX 64)
	              (MESSAGE :STRING :MIN 5 :MAX 256))
	           (PUBLISH! (INTERN ROOM :KEYWORD)
	                     (ENCODE-JSON-TO-STRING
	                      `((:NAME ,@NAME) (:MESSAGE ,@MESSAGE)))))))

Which is to say, we'd like to associate the handler we're making with the uri `/send-message` in the handler table `*HANDLERS*`, and issue a warning if that isn't a fresh association. None of that is particularly interesting. Lets take a look at the expansion of `make-closing-handler` specifically:

	(LAMBDA (SOCK PARAMETERS)
                (DECLARE (IGNORABLE PARAMETERS))
                (LET ((ROOM
                       (AIF (CDR (ASSOC :ROOM PARAMETERS)) (URI-DECODE IT)
                            (ERROR (MAKE-INSTANCE 'HTTP-ASSERTION-ERROR :ASSERTION 'ROOM)))))
                  (ASSERT-HTTP (>= 16 (LENGTH ROOM)))
                  (LET ((NAME
                         (AIF (CDR (ASSOC :NAME PARAMETERS)) (URI-DECODE IT)
                              (ERROR
                               (MAKE-INSTANCE 'HTTP-ASSERTION-ERROR :ASSERTION 'NAME)))))
                    (ASSERT-HTTP (>= 64 (LENGTH NAME) 1))
                    (LET ((MESSAGE
                           (AIF (CDR (ASSOC :MESSAGE PARAMETERS)) (URI-DECODE IT)
                                (ERROR
                                 (MAKE-INSTANCE 'HTTP-ASSERTION-ERROR :ASSERTION 'MESSAGE)))))
                      (ASSERT-HTTP (>= 256 (LENGTH MESSAGE) 5))
                      (LET ((RES
                             (MAKE-INSTANCE 'RESPONSE :CONTENT-TYPE "text/html" :BODY
                                            (PROGN
                                             (PUBLISH! (INTERN ROOM :KEYWORD)
                                                       (ENCODE-JSON-TO-STRING
                                                        `((:NAME ,@NAME)
                                                          (:MESSAGE ,@MESSAGE))))))))
                        (WRITE! RES SOCK)
                        (SOCKET-CLOSE SOCK))))))

This is the big one. It looks mean, but it really amounts to an unrolled loop. You can see that for every parameter, we're grabbing its value in the `parameters` association list, ensuring it exists, `uri-decode`ing it if it does, and asserting the appropriate properties we want to enforce. At any given point, if an assertion is violated, we're done and we return an error (not pictured here, but the error handlers surrounding an HTTP handler call will ensure it). If we get through all of our arguments without running into an error, we're going to evaluate the handler body, write the result out to the requester and close the socket.

There's a couple of other Lisp-specific pieces of functionality I'll point out as we go, but that's the meat of the approach. We'll be taking a look at the code that performs this sequence of transformations later in the chapter, but I wanted you to see the practical payoff right away.

Now that you've seen the basics of macroexpansion, lets start with the basics of asynchronous servers and work our way back through the pattern.

### The Basics

At the 10k-foot-level, an HTTP exchange is one request and one response. A client sends a request, which includes a resource identifier, an HTTP version tag, some headers and some parameters. The receiving server parses that request, figures out what to do about it, and sends a response which includes the same HTTP version tag, a response code, some headers and a request body.

And that's it.

Because this is the total of the basic protocol, many minimal servers take the thread-per-request approach. That is, for each incoming request, spin up a thread to do the work of parse/figure-out-what-to-do-about-it/send-response, then spin it down. The idea is that since each of these connections is very short lived, that won't start up too many threads at once, and it'll let you simplify a lot of the implementation. Specifically, it lets you program as though there were only one connection present at any given time, and it lets you do things like kill orphaned connections just by killing their thread and letting the garbage collector do its job.

There's a couple of things missing in this system though. First, as described, there's no mechanism for a server to send updates to a client without that client specifically requesting them. Second, there's no identity mechanism, which you need in order to confidently assert that a number of requests come from the same client (or, from the client perspective, to make sure you're making a request from the server you think you're talking to). We won't be solving the second problem here; a full sesison implementation would nudge us up to ~560 lines of code, and we've got a hard limit of 500.

The second problem is interesting though. It's interesting if you've ever wanted to put together multi-user web-applications for whatever reason. The simplest base-case is the anonymous chat room. Consider the situation where you've got two people entering text into a text-box with the intent that both of them should see each message. When Adrian types in a message, you can send him the updated chat room immediately. But if Beatrice were to type a message, according to the system we've described above, there's no built-in way to update Adrian's view of the world with that new message. What you want to be able to do is to push messages from the server at Adrian without him having to take any deliberate action. 

#### Server Push

Here are our options:

##### Comet/Longpoll

Build the client such that it automatically sends the server a new request as soon as it receives a response. Instead of fulfilling that request right away, the server then hangs on to said request until it has new information to send, like say, a new message from Beatrice. The end result is that Adrian gets new updates as soon as they happen, rather than just when he takes action. It's a bit of a semantic distinction though, since the client is taking action on his behalf on every update.

##### SSE

The client opens up a connection and keeping it open. The server will periodically write new data to the connection without closing it, and the client will interpret incoming new messages as they arrive rather than waiting for the response connection to terminate. This way is a bit more efficient than the Comet/Longpoll approach because each message doesn't have to incur the overhead of a fresh set of HTTP headers.

##### Websockets

The server and client open up an HTTP conversation, then perform a handshake and protocol escalation. The end result is that they're still communicating over TCP/IP, but they're not using HTTP to do it at all. The advantage this has over SSEs is that you can customize your protocol, so it's possible to be more efficient.

That's basically it. I mean there used to be things called "Forever Frames" that have been thoroughly replaced by the SSE approach, and a couple of other tricks you could pull with proprietary or esoteric technologies, but they're not materially different from the above.

These approaches are pretty different from each other under the covers, as you can hopefully see now that you understand them, but they have one important point in common. They all depend on long-lived connections. Longpolling depends on the server keeping requests around until new data is available (thus keeping a connection open until new data arrives, or the client gives up in frustration), SSEs keep an open stream between client and server to which data is periodically written, and Websockets change the protocol a particular connection is speaking, but leave it open (and bi-directional, which complicates matters slightly; you basically need to chuck websockets back into the main listen/read loop *and keep them there* until they're closed).

The consequence of keeping long-lived connections around is that you're going to want one of

a) A server that can service many connections with a single thread
b) A thread-per-request server that passes long-lived connections off to a separate subsystem, which must handle those long lived connections using a minimal number of threads
c) A thread-per-request server on top of a platform where threads are cheap enough that you can afford having a few hundred thousand of them around.

For an example of option `c`, have a look at Yaws (the [web server](http://hyber.org/), not the [tropical infection](http://en.wikipedia.org/wiki/Yaws)). `b` strikes me as ridiculous. The reason for using a thread-per-request model is that it mechanically simplifies server implementation, but adding the requirement of a separate long-lived connection subsystem seems like it would result in a net complexity *increase*. So, if we want server pushing in the absence of really, *really*, **really** cheap threads, we're dealing with an asynchronous server. Which means dealing with non-blocking IO (we'll see why that is later on), and potentially dealing with a single thread.

### High Level

We're not concerned with implementing a client, though that's a very interesting problem in its own right, we're dealing with the server-side of the HTTP equation. If you're a server, what you want to be doing is:

1. Listening for connections on a TCP port
2. When a connection comes in, read from it until you get a complete HTTP request
3. Parse the request
4. Route the parsed request to the appropriate handler
5. Call that handler to generate a response
6. Send the response out to the requester

That's the base case, of course. If you *also* want to be pushing data at your clients, there's also two special cases in steps `4` and `5`. Instead of fulfilling a standard request, you might have a situation where

- The requester wants to subscribe to an update feed you have running
- The requester is making a request that requires an update to be published to one or more existing feeds

[[Note to editor: What's the rule on pics? This section seems like it could benefit massively from a diagram or two.]]

### Implementation

Breaking that down into a more detailed implementation, we'll want to think of this server as 4 main subsystems; one to deal with listening/reading, one to deal with request parsing, one to deal with routing/handling, and one last one to deal with our subscriptions/broadcasts. Lets start from the simplest of these, and work our way back through the pattern.

#### Subscriptions and Broadcasts

There's basically two operations we need here, and they're both very simple. We want to be able to subscribe a particular connection to a channel, and we want to be able to send a message out to a channel, if it exists. We'll also need to keep track of existing channels and subscribers.

	(defparameter *channels* (make-hash-table))

	(defmethod subscribe! ((channel symbol) (sock usocket))
	  (push sock (gethash channel *channels*))
	  nil)

	(defmethod publish! ((channel symbol) (message string))
	  (awhen (gethash channel *channels*)
	    (loop with msg = (make-instance 'sse :data message)
	       for sock in it (progn (write! msg sock)
				     (force-output (socket-stream sock))))))

There. `*channels*` is a hash table of channel names to subscriber lists. The `subscribe!` method takes a channel and a socket, and adds the socket to the particular channels' subscription list. Finally, `publish!` takes a channel and a message, and publishes that message (in `SSE` format) to each listener on that particular channel. The first two will actually, factually work in a real system, but that `publish!` method is simplified; it leaves out error handling code, and doesn't clean up connections that have stopped listening. All it does is get a channel, then iterate over all of `it`s sockets, calling `write!` on the message we want to publish (For those of you new to lisp, `awhen` is a simple macro form called an "anaphoric macro". This particular one binds the symbol `it` to the result of its test. The end result being that if `(gethash channel *channels*)` returns false, nothing will happen, but if it returns a channel, we will be able to refer to said channel by using `it` rather than calling `(gethash channel *channels*)` a third time).

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

Instead of being so simple-minded about it, this `publish!` iterates over the subscriber list, collects the sockets that were written to successfully, and assigns the subscriber list to that bag of active listeners. Any sockets that errored out during the write are dropped, since they're no longer listening. This will now *also* really, truly work properly.

#### Request Routing, Handling and Writing

	(defmethod handle-request ((sock usocket) (req request))
	  (aif (gethash (resource req) *handlers*)
	       (funcall it sock (parameters req))
	       (error! +404+ sock)))

	(defun crlf (&optional (stream *standard-output*))
	  (write-char #\return stream)
	  (write-char #\linefeed stream)
	  (values))

	(defmethod write! ((res response) (sock usocket))
	  (let ((stream (socket-stream sock)))
	    (flet ((write-ln (&rest sequences)
			     (mapc (lambda (seq) (write-sequence seq stream)) sequences)
			     (crlf stream)))
	      (write-ln "HTTP/1.1 " (response-code res))  
	      (write-ln "Content-Type: " (content-type res) "; charset=" (charset res))
	      (write-ln "Cache-Control: no-cache, no-store, must-revalidate")
	      (awhen (cookie res)
		     (write-ln "Set-Cookie: " it))
	      (awhen (location res)
		     (write-ln "Location: " it))
	      (when (keep-alive? res) 
		(write-ln "Connection: keep-alive")
		(write-ln "Expires: Thu, 01 Jan 1970 00:00:01 GMT"))
	      (awhen (body res)
		     (write-ln "Content-Length: " (write-to-string (length it)))
		     (crlf stream)
		     (write-ln it))
	      (values))))

	(defmethod write! ((res sse) (sock usocket))
	  (format (socket-stream sock)
		  "~@[id: ~a~%~]~@[event: ~a~%~]~@[retry: ~a~%~]data: ~a~%~%"
		  (id res) (event res) (retry res) (data res)))

	(defmethod error! ((err response) (sock usocket))
	  (ignore-errors 
	    (write! err sock)
	    (socket-close sock)))

This section should also include an explanation of all of `define-handler`, and associated type declarations.

#### Request Parsing

	(defmethod parse-params ((params null)) nil)
	(defmethod parse-params ((params string))
	  (loop for pair in (split "&" params)
	     for (name val) = (split "=" pair)
	     collect (cons (->keyword name) (or val ""))))
	
	(defmethod parse ((str string))
	  (let ((lines (split "\\r?\\n" str)))
	    (destructuring-bind (req-type path http-version) (split " " (pop lines))
	      (declare (ignore req-type))
	      (assert-http (string= http-version "HTTP/1.1"))
	      (let* ((path-pieces (split "\\?" path))
		     (resource (first path-pieces))
		     (parameters (second path-pieces))
		     (req (make-instance 'request :resource resource :parameters parameters)))
		(loop for header = (pop lines) for (name value) = (split ": " header)
		   until (null name)
		   for n = (->keyword name)
		   do (push (cons n value) (headers req)))
		(setf (parameters req)
		      (append (parse-params (parameters req))
			      (parse-params (pop lines))))
		req))))

#### Listening and Reading

	(defmethod start ((port integer))
	  (let ((server (socket-listen usocket:*wildcard-host* port :reuse-address t))
		(conns (make-hash-table))
	        (buffers (make-hash-table)))
	    (unwind-protect
		 (loop (loop for ready in (wait-for-input (cons server (alexandria:hash-table-keys conns)) :ready-only t)
			  do (if (typep ready 'stream-server-usocket)
				 (setf (gethash (socket-accept ready) conns) :on)
				 (let ((buf (gethash ready buffers (make-instance 'buffer))))
				   (when (eq :eof (buffer! buf ready))
				     (remhash ready conns)
				     (remhash ready buffers))
				   (let ((complete? (complete? buf))
					 (big? (too-big? buf))
					 (old? (too-old? buf)))
				     (when (or complete? big? old?)
				       (remhash ready conns)
				       (remhash ready buffers)
				       (cond (big? 
					      (error! +413+ ready))
					     (old? 
					      (error! +400+ ready))
					     (t (handler-case
						    (handle-request ready (parse buf))
						  ((not simple-error) ()
						    (error! +400+ ready)))))))))))
	      (loop for c being the hash-keys of conns
		 do (loop while (socket-close c)))
	      (loop while (socket-close server)))))

	(defmethod complete? ((buffer buffer)) (found-crlf? buffer))

	(defmethod too-big? ((buffer buffer))
	  (> (content-size buffer) +max-request-size+))

	(defmethod too-old? ((buffer buffer))
	  (> (- (get-universal-time) (started buffer)) +max-request-size+))

	(defmethod buffer! ((buffer buffer) (sock usocket))
	  (unwind-protect
	       (let ((stream (socket-stream sock))
		     (partial-crlf (list #\return #\newline #\return)))
		 (loop for char = (read-char-no-hang stream nil :eof)
		    do (when (and (eql #\newline char)
				  (starts-with-subseq partial-crlf (contents buffer)))
			 (setf (found-crlf? buffer) t))
		    until (or (null char) (eql :eof char))
		    do (push char (contents buffer)) do (incf (content-size buffer))
		    finally (return char)))
	    :eof))

#### Using It

Now that we've got the server written, lets take a look at how we might implement that theoretical anonymous chatroom in it. I'll leave the client an exercise for the reader; if you've read this far and understood the core concepts, it shouldn't take you too much JavaScript hackery to come up with something workable.

Instead, lets do our usual and focus on the server side.

We'll need all of two handlers (not counting the one that'll serve up the front-end, so three, I guess):

1. The handler that serves up the stream of the specified room
2. The handler that sends a message to a specified room




- Quick how-to showing how to put together an anonymous chat using the simplified server.

##### Notes To Self

- Compare/contrast with the standard OO way of doing things
	- talk about how you would have gone about extensibility in the sense of adding handlers and additional argument types
