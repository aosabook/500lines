# On Interacting Through HTTP in an Asynchronous Manner in the Medium of Common Lisp

Ok, this was going to start off with the basics of threaded vs asynchronous servers, and a quick rundown of the use cases for each, but its been brought to my attention that the whole Common Lisp thing might be intimidating to people. Given that tidbit, I debated both internally and externally on how to begin, and decided that the best way might be from the end (Just to be clear, yes, this is a toy example. If you'd like to see a non-toy example using the same server, take a look at [cl-notebook](https://github.com/Inaimathi/cl-notebook) or [deal](https://github.com/Inaimathi/deal). And on a related note This writeup also features a stripped-down version of the `house` server. [The real version](https://github.com/Inaimathi/house) also does some light static file serving, deals with sessions in a currently semi-satisfactory way, imposes a priority system on HTTP types, has a few clearly labeled cross-plaform hacks, and (by the time this writeup is done) will also probably be doing more detailed front-end error reporting. That's it though. The main handler definition system as well as the *actual server* is exactly the same.). So to *that* end, here's what I want to be able to do:

    (define-stream-handler (source) (room)
       (subscribe! (intern room :keyword) sock))

    (define-closing-handler (index) ()
       <insert some javascript UI here>)

    (define-closing-handler (send-message) (room name message)
       (publish! (intern room :keyword) (encode-json-to-string `((:name . ,name) (:message . ,message)))))

    (start 4242)

And having done that, I should be able to browse over to `localhost:4242/index` (actual front-end structure left as an exercise for the reader) and see a little chat room using which I could subscribe and post to various message channels. Because I don't hate myself, and I'd like this to actually work in some sort of production environment, I need to both do some sanitation on inputs and make sure that this sanitation needn't be done manually. So really, I want to be able to say something like

    (define-stream-handler (source) ((room :string (>= 16 (length room))))
       (subscribe! (intern room :keyword) sock))

    (define-closing-handler (index) ()
       <insert some javascript UI here>)

	(define-closing-handler (send-message) 
	    ((room :string (>= 16 (length room)))
	     (name :string (>= 64 (length name) 1))
	     (message :string (>= 256 (length message) 5)))
	  (publish! (intern room :keyword) (encode-json-to-string `((:name . ,name) (:message . ,message)))))

    (start 4242)

It's not *quite* strongly typed HTTP parameters, because I'm interested in enforcing more than the type, but that's a good first approximation. You can imagine more or less this same thing being implemented in a mainstream class-based OO language using a class hierarchy. That is, you'd define a `handler` class, then subclass that for each handler you have, giving each `get`, `post`, `parse` and `validate` methods as needed. If you imagine this well enough, you'll also see the small but non-trivial pieces of boilerplate that the approach would get you, both in terms of setting up classes and methods themselves and in terms of doing the parsing/validation of your parameters. The Common Lisp approach, and I'd argue the right approach, is to write a DSL to handle the situation. In this case, it takes the form of a new piece of syntax that lets you declare certain properties of your handlers, and expands into the code you would have written by hand. Writing this way, your code ends up amounting to a set of instructions by which a Lisp implementation can unfold your code into the much more verbose and extensive code that you want to run. The benefit here is that you don't have to maintain the intermediate code, as you would if you were using IDE/editor-provided code generation facilities, you have the comparably easy and straight-forward task of maintaining the unfolding specification.

### Stepping Through Expansions

Lets step through the expansion for `send-message`, just so you understand what's going on at each step. What I'm about to show you is the output of the SLIME macroexpander, which does a one-level expansion on the macro call you give it.

	(define-closing-handler (send-message) 
	    ((room :string (>= 16 (length room)))
	     (name :string (>= 64 (length name) 1))
	     (message :string (>= 256 (length message) 5)))
	  (publish! (intern room :keyword) (encode-json-to-string `((:name . ,name) (:message . ,message)))))

No big deal; that's just what I want to write. What I want this to mean is

> "Bind the action `(publish! ...)` to the URI `/send-message` in the handlers table. Before you run that action, make sure the client has passed us parameters named `room`, `name` and `message`, ensure that `room` is a string no longer than 16 characters, `name` is a string of between 1 and 64 characters (inclusive) and finally that `message` is a string of between 5 and 256 characters (also inclusive). After you've sent the response back, close the channel.".

Expanding it will get us

	(BIND-HANDLER SEND-MESSAGE
	              (MAKE-CLOSING-HANDLER (:CONTENT-TYPE "text/html")
	                  ((ROOM :STRING (>= 16 (LENGTH ROOM)))
	                   (NAME :STRING (>= 64 (LENGTH NAME) 1))
	                   (MESSAGE :STRING (>= 256 (LENGTH MESSAGE) 5)))
	                (PUBLISH! (INTERN ROOM :KEYWORD)
	                          (ENCODE-JSON-TO-STRING
	                           `((:NAME ,@NAME) (:MESSAGE ,@MESSAGE))))))

We're binding the result of `make-closing-handler` to the (for now) symbol `send-message`. Expanding `bind-handler` gets us

	(PROGN
	 (WHEN (GETHASH "/send-message" *HANDLERS*)
	   (WARN "Redefining handler '/send-message'"))
	 (SETF (GETHASH "/send-message" *HANDLERS*)
	         (MAKE-CLOSING-HANDLER (:CONTENT-TYPE "text/html")
	             ((ROOM :STRING (>= 16 (LENGTH ROOM)))
	              (NAME :STRING (>= 64 (LENGTH NAME) 1))
	              (MESSAGE :STRING (>= 256 (LENGTH MESSAGE) 5)))
	           (PUBLISH! (INTERN ROOM :KEYWORD)
	                     (ENCODE-JSON-TO-STRING
	                      `((:NAME ,@NAME) (:MESSAGE ,@MESSAGE)))))))

Which is to say, we'd like to associate the handler we're making with the uri `/send-message` in the handler table `*HANDLERS*`. We'd additionally like a warning to be issued if that binding already exists, but will re-bind it regardless. None of that is particularly interesting. Lets take a look at the expansion of `make-closing-handler` specifically:

	(LAMBDA (SOCK #:COOKIE?1111 SESSION PARAMETERS)
	           (DECLARE (IGNORABLE SESSION PARAMETERS))
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
	                        (MAKE-INSTANCE 'RESPONSE :CONTENT-TYPE "text/html" :COOKIE
	                                       (UNLESS #:COOKIE?1111 (TOKEN SESSION)) :BODY
	                                       (PROGN
	                                        (PUBLISH! (INTERN ROOM :KEYWORD)
	                                                  (ENCODE-JSON-TO-STRING
	                                                   `((:NAME ,@NAME)
	                                                     (:MESSAGE ,@MESSAGE))))))))
	                   (WRITE! RES SOCK)
	                   (SOCKET-CLOSE SOCK))))))

This is the big one. It looks mean, but it really amounts to an unrolled loop over the arguments. You can see that for every parameter, we're grabbing its value in the `parameters` association list, ensuring it exists, `uri-decode`ing it if it does, and asserting the appropriate properties we want to enforce. At any given point, if an assertion is violated, we're done and we return an error (handling said error not pictured here, but the error handlers surrounding an HTTP handler call will ensure that these errors get translated to `HTTP 400` or `500` errors being sent over the wire). If we get through all of our arguments without running into an error, we're going to evaluate the handler body, write the result out to the requester and close the socket.

### Understanding the Expanders

If you're more interested in the server proper, skip the next few sections. The first thing we're going to do is dissect the code that generates the above expansions. We'll start from the end again. In this case, the end of `define-handler.lisp`

	(defmacro define-closing-handler ((name &key (content-type "text/html")) (&rest args) &body body)
	  `(bind-handler ,name (make-closing-handler (:content-type ,content-type) ,args ,@body)))

	(defmacro define-stream-handler ((name) (&rest args) &body body)
	  `(bind-handler ,name (make-stream-handler ,args ,@body)))

The `define-(*)-handler` macros just straight-forwardly exand into calls to `bind-handler` and `make-\1-handler`. You can see that, because we're using homoiconic code, we can use the backtick and comma operators to basically cut holes in an expression we'd like to evaluate. Calling the resulting macros will slot values into said holes and evaluate the result. We *could* have defined one macro here, with perhaps an extra key argument in the first cluster that let you specify whether it is meant to close out the connection or not. That would have resulted in one slightly complicated `defmacro` rather than two dead-simple ones, so I decided against it, but reserve the right to change my mind later.

Next up, `bind-handler`

	(defmacro bind-handler (name handler)
	  (assert (symbolp name) nil "`name` must be a symbol")
	  (let ((uri (if (eq name 'root) "/" (format nil "/~(~a~)" name))))
	    `(progn
	       (when (gethash ,uri *handlers*)
		 (warn ,(format nil "Redefining handler '~a'" uri)))
	       (setf (gethash ,uri *handlers*) ,handler))))

takes a symbol and a handler, and binds the handler to the uri it creates by prepending "/" to the lower-cased symbol-name of that symbol (that's what the `format` call does). The binding happens in the last line; `(setf (gethash ,uri *handlers*) ,handler)`, which is what hash-table assignments look like in Common Lisp (modulo the commas, of course). This is another level that you can fairly straight-forwardly map to its expansion above. Note that the first assertion here is outside of the quoted area, which means that it'll be run as soon as the macro is called rather than when its result is evaluated.

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

So making a closing-handler involves making a `lambda`, which is just what you call anonymous functions in Common Lisp and Scheme. We also set up an interior scope that makes a `response` out of the `body` argument we're passing in, `write!`s that to the requesting socket, then closes it. The remaining question is, what is `arguments`?

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

Welcome to the hard part. `arguments` takes the handlers' arguments, and generates that tree of parse attempts and assertions you saw in the full macroexpansion of `send-message`. In other words, it takes

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

### A Short Break -- Briefly Meditating on Macros

Lets take a short break here. At this point we're two levels deep into tree processing. And what we're doing will only make sense to you if you remember that Lisp code is itself represented as a tree. That's what the parentheses are for; they show you how leaves and branches fit together. If you step back, you'll realize we've got a macro definition, `make-closing-handler`, which calls a function, `arguments`, to generate part of the tree its constructing, which in turn calls some tree-manipulating helper functions, including `arg-exp`, to generate its return value. The tree that these functions have as input *happen* to reprent Lisp code, and because there's no difference between Lisp code and a tree, you have a transparent sytax definition system. The input is a Lisp expression, and the output is a lisp expression that will be evaluated in its place. Possibly the simplest way of conceptualizing this is as a very simple and minimal Common Lisp to Common Lisp compiler.

### Another Short Break -- Briefly Meditating on Anaphoric Macros

A particularly widely used, and particularly simple group of such compilers are called *anaphoric macros*. You've seen `aif` already, and will later see `awhen` later on. Personally, I only tend to use those two with any frequency, but there's a fairly wide variety of them available in the [`anaphora` package](http://www.cliki.net/Anaphora). As far as I know, they were first defined by Paul Graham in an [OnLisp chapter](http://dunsmor.com/lisp/onlisp/onlisp_18.html). The use case he gives is a situation where you want to do some sort of expensive or semi-expensive check, then do something conditionally on the result. In the above context, we're using `aif` to do a check on the result of an `alist` traversal.

	(aif (cdr (assoc :room parameters))
	     (uri-decode it)
	     (error (make-instance 'http-assertion-error :assertion 'room)))

What this means is "Take the `cdr` of looking up the symbol `:room` in the association list `parameters`. If that returns a non-nil value `uri-decode` it, otherwise throw an error of the type `http-assertion-error`." In other words, the above is equivalent to

	(let ((it (cdr (assoc :room parameters))))
	  (if it
	      (uri-decode it)
	      (error (make-instance 'http-assertion-error :assertion 'room))))

In Haskell, you'd use `Maybe` in this situation. In Common Lisp, you'd take advantage of the lack of hygienic macros as above to trivially capture the symbol `it` in the expansion as the name for the result of the check. The reason I bring any of this up is that I lied to you recently.

> If you step back, you'll realize we've got a macro definition, `make-closing-handler`, which calls a function, `arguments`, to generate part of the tree its constructing, which in turn calls some tree-manipulating helper functions, including `arg-exp`, to generate its return value.

The tree hasn't bottomed out yet. In fact, by the time you get to `arg-exp`, you've still got at least two levels to go; `assert-http` and `make-instance` both expand into more primitive forms before getting evaluated. We'll be taking a look at `assert-http` later on, but I won't be expanding and explaining `make-instance`. If you're interested, you can get `SLIME` running and keep macroexpanding 'till you hit bottom. It may take a while.

Now lets get back to the point; expanding type annotations for HTTP handlers. And in order to plumb the depths of that mystery, we'll need to take a look at how we intend to *define* HTTP types.

### Defining HTTP Types

With the above macro-related tidbits, you should be able to see that `arg-exp` is actually doing the job of generating a specific, otherwise repetitive, piece of the code tree that we eventually want to evaluate. In this case, the piece that checks for the presence of the given parameter among the handlers' `parameters`. And that's all you need to understand about it, so lets move on to...

	(defgeneric type-expression (parameter type)
	  (:documentation
	   "A type-expression will tell the server how to convert a parameter from a string to a particular, necessary type."))
    ...
	(defmethod type-expression (parameter type) nil)

[[Note to Editor: Should I have a bit of exposition here about generic functions and how they work?]]
[[Note to Self: See if you can do this a bit later. Let them digest macroexpansion first, then hop into generics and multiple dispatch when you get to the model]]

This is a *method* that generates new tree structures (coincidentally Lisp code), rather than just a function. And yes, you can do that just fine. The only thing the above tells you is that by default, a `type-expression` is `NIL`. Which is to say, we don't have one. If we encounter a `NIL`, we just use the output of `arg-exp` raw, but that doesn't tell us much about the usual case. To see that, lets take a look at a built-in (to `:house`) `define-http-type` expression.

	(define-http-type (:integer)
	    :type-expression `(parse-integer ,parameter :junk-allowed t)
		:type-assertion `(numberp ,parameter))

So an `:integer` is a thing that we're going to get out of a raw `parameter` by using `(parse-integer parameter :junk-allowed t)`, and we want to check whether the result is actually an integer (The Haskellers reading along will probably chuckle at this, but the best way of thinking about most lisp functions is as returning a `Maybe` because many of them signal failure by returning `NIL` rather than whatever they were going to return. `parse-integer` is one of these, so we need to check that its result is *actually* an integer before proceeding (This gives you some fun edge cases in places where `NIL` is part of the set of legitimately possible return values of a particular procedure. Examples are `gethash` and `getf`. I'm not going to get into that here, other than mentioning that you typically handle it by using multiple return values)). Here's the demonstration of the first part (we'll get to `type-assertion`s in a moment):

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

After unrolling the above macro tower and understanding how it fits together, the rest of this is going to be a pretty straight-forward asynchronous server implementation. Since we just talked about the different things `make-stream-handler` and `make-closing-handler` generate to write to the client sockets' stream, this is a nice segue into the `:house` model.

### Object Oriented Programming in Common Lisp

[[TODO: Write up the model summary here. If you can, work in the points about generics and multiple dispatch here. If not, you can add some exposition to the previous section when you touch on `define-http-type`]]




### The Basics of Asynchronous Servers

At the 10k-foot-level, an HTTP exchange is one request and one response. A client sends a request, which includes a resource identifier, an HTTP version tag, some headers and some parameters. The receiving server parses that request, figures out what to do about it, and sends a response which includes the same HTTP version tag, a response code, some headers and a request body.

And that's it.

Because this is the total of the basic protocol, many minimal servers take the thread-per-request approach. That is, for each incoming request, spin up a thread to do the work of parse/figure-out-what-to-do-about-it/send-response, then spin it down. The idea is that since each of these connections is very short lived, that won't start up too many threads at once, and it'll let you simplify a lot of the implementation. Specifically, it lets you program as though there were only one connection present at any given time, and it lets you do things like kill orphaned connections just by killing their thread and letting the garbage collector do its job.

There's a couple of things missing in this system though. First, as described, there's no mechanism for a server to send updates to a client without that client specifically requesting them. Second, there's no identity mechanism, which you need in order to confidently assert that a number of requests come from the same client (or, from the client perspective, to make sure you're making a request from the server you think you're talking to). We won't be solving the second problem here; a full session implementation would nudge us up to ~560 lines of code, and we've got a hard limit of 500.

The second problem is interesting though. It's interesting if you've ever wanted to put together multi-user web-applications for whatever reason. The simplest base-case is the anonymous chat room. Consider the situation where you've got two people entering text into a text-box with the intent that both of them should see each message. When Adrian types in a message, you can send him the updated chat room immediately. But if Beatrice were to type a message, according to the system we've described above, there's no built-in way to update Adrian's view of the world with that new message until he makes another request. What you want to be able to do is to push messages from the server at Adrian without him having to take any deliberate action. 

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

##### Notes To Self

- Compare/contrast with the standard OO way of doing things
	- talk about how you would have gone about extensibility in the sense of adding handlers and additional argument types
