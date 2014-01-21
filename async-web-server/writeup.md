##### Draft

So lets start with the basics.

### The Basics

At the 10k-foot-level, an HTTP exchange is one request and one response. A client sends a request, which includes a resource identifier, an HTTP version tag, some headers and some parameters. The receiving server parses that request, figures out what to do about it, and sends a response which includes the same HTTP version tag, a response code, some headers and a request body.

There's a couple of things missing in this system. First, as described, there's no mechanism for a server to send updates to a client without that client specifically requesting them. Second, there's no identity mechanism, which you need in order to confidently assert that a number of requests come from the same client (or, from the client perspective, to make sure you're making a request from the server you think you're talking to). We won't be solving the second problem here; a full sesison implementation would nudge us up to ~560 lines of code, and we've got a hard limit of 500. The solution to the first one's fairly easy to implement though.

#### Server Push

Quick rundown of the options first [brief explanation of all of the above]

##### Comet/Longpoll
##### SSEs
##### Websockets

They're pretty different under the covers, as you can hopefully see now that you understand them, but they have one important point in common: they all depend on long-lived connections. Longpolling depends on the server keeping requests around until new data is available (thus keeping a connection open until new data arrives, or the client gives up in frustration), SSEs keep an open stream between client and server to which data is periodically written, and Websockets basically change the protocol a particular connection is speaking, but leave it open (and bi-directional, which complicates matters slightly; you basically need to chuck websockets back into the main listen/read loop *and keep them there* until they're closed).

The consequence of keeping long-lived connections around is that you're either going to want

a) A server that can service many connections with a single thread
b) A thread-per-request server that passes long-lived connections off to a separate subsystem, which must handle those long lived connections using minimal threads
c) A thread-per-request server on top of a platform where threads are cheap enough that you can afford having a few hundred thousand of them around.

For an example of option `c`, have a look at Yaws (the [web server](http://hyber.org/), not the [tropical infection](http://en.wikipedia.org/wiki/Yaws)). `b` strikes me as ridiculous. The usual argument given for using a thread-per-request model is that it mechanically simplifies server implementation, but it feels like adding the requirement of a separate long-lived connection subsystem would result in a net complexity *increase* with this approach. So, if we want server pushing in the absence of really, *really*, **really** cheap threads, we're dealing with an asynchronous server. Which means dealing with non-blocking IO (we'll see why that is later on), and potentially dealing with a single thread.

### High Level

We're not concerned with implementing a client, though that's a very interesting problem in its own right, we're dealing with the server-side of the HTTP equation. If you're a server, what you want to be doing is:

1. Listening for connections on a TCP port
2. When a connection comes in, read from it until you get a complete HTTP request
3. Parse the request
4. Route the parsed request to the appropriate handler
5. Call that handler to generate a request
6. Send the response out to the requester

That's the base case, of course. If you *also* want to be pushing data at your clients, there's also two special cases in steps `4` and `5`. Instead of fulfilling a standard request, you might have a situation where

- The requester wants to subscribe to an update feed you have running
- The requester is making a request that requires an update to be published to one or more existing feeds

[[Note to editor: What's the rule on pics? This section seems like it could benefit massively from a diagram or two.]]

### Implementation

Breaking that down into a more detailed implementation, we'll want to think of this server as [] main subsystems; one to deal with listening/reading, one to deal with request parsing, one to deal with routing/handling, and one last one to deal with our subscriptions/broadcasts. Lets start from the simplest of these, and work our way back through the pattern.

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

Instead of being so simple-minded about it, this `publish!` iterates over the subscriber list, collects the sockets that were written to successfully, and assigns the subscriber list to that bag of active listeners. This will now *also* really, truly work properly.

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

Quick how-to showing how to put together an anonymous chat using the simplified server.

##### Notes To Self

- Compare/contrast with the standard OO way of doing things
	- talk about how you would have gone about extensibility in the sense of adding handlers and additional argument types
