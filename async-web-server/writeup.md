# On Interacting Through HTTP in an Event-Driven Manner in the Medium of Common Lisp

Ok, this was going to start off with the basics of threaded vs event-driven servers, and a quick rundown of the use cases for each, but its been brought to my attention that the whole Common Lisp thing might be intimidating to people. Given that tidbit, I debated both internally and externally on how to begin, and decided that the best way might be from the end (Just to be clear, yes, this is a toy example. If you'd like to see a non-toy example using the same server, take a look at [cl-notebook](https://github.com/Inaimathi/cl-notebook), [deal](https://github.com/Inaimathi/deal), and possibly [langnostic](https://github.com/Inaimathi/langnostic). And on a related note This write-up also features a stripped-down version of the `house` server. [The real version](https://github.com/Inaimathi/house) also does some light static file serving, deals with sessions properly, imposes a priority system on HTTP types, has a few clearly labeled cross-platform hacks, and (by the time this article is done) will also probably be doing more detailed back-end error reporting. That's it though. The main handler definition system, HTTP types implementation as well as the *actual server* is exactly the same.). So to *that* end, here's what I want to be able to do:

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

As a side-note, since all those validators all check `length`, we could define a separate function to simplify the input code for us.

    (defun length-between (min thing max)
	  (>= max (length thing) min))

    ...

    (define-stream-handler (source) ((room :string (length-between 0 room 16))
       (subscribe! (intern room :keyword) sock))

    ...

	(define-closing-handler (send-message) 
	    ((room :string (length-between 0 room 16))
	     (name :string (length-between 1 name 64)
	     (message :string (length-between 5 message 256)))
	  (publish! (intern room :keyword) (encode-json-to-string `((:name . ,name) (:message . ,message)))))

    ...

In this particular case, it doesn't gain us much, and since I'm a bit more concerned about the simplicity of *intervening* code rather than *input* code for this exercise, I'll be sticking to the primitives. You'll see what I mean by that "intervening" comment in a moment; if you're not already a Lisper it may get a bit weird.

It's not *quite* strongly typed HTTP parameters, because I'm interested in enforcing more than type, but that's a good first approximation. You can imagine more or less this same thing being implemented in a mainstream class-based OO language using a class hierarchy. That is, you'd define a `handler` class, then subclass that for each handler you have, giving each `get`, `post`, `parse` and `validate` methods as needed. If you imagine this well enough, you'll also see the small but non-trivial pieces of boilerplate that the approach would get you, both in terms of setting up classes and methods themselves and in terms of doing the parsing/validation of your parameters. The Common Lisp approach, and I'd argue the right approach, is to write a DSL to handle the problem. In this case, it takes the form of a new piece of syntax that lets you declare certain properties of your handlers, and expands into the code you would have written by hand. Writing this way, your code ends up amounting to a set of instructions which a Lisp implementation can unfold into the much more verbose and extensive code that you want to run. The benefit here is that you don't have to maintain the intermediate code, as you would if you were using IDE/editor-provided code generation facilities, you have the comparably easy and straight-forward task of maintaining the unfolding instructions.

### Stepping Through Expansions

Lets step through the expansion for `send-message`, just so you understand what's going on. What I'm about to show you is the output of the SLIME macro-expander, which does a one-level expansion on the macro call you give it.

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

Which is to say, we'd like to associate the handler we're making with the URI `/send-message` in the handler table `*HANDLERS*`. We'd additionally like a warning to be issued if that binding already exists, but will re-bind it regardless. None of that is particularly interesting. Lets take a look at the expansion of `make-closing-handler` specifically:

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

This is the big one. It looks mean, but it really amounts to an unrolled loop over the arguments. You can see that for every parameter, we're grabbing its value in the `parameters` association list, ensuring it exists, `uri-decode`ing `it` if it does, and asserting the appropriate properties we want to enforce. At any given point, if an assertion is violated, we're done and we return an error (handling said error not pictured here, but the error handlers surrounding an HTTP handler call will ensure that these errors get translated to `HTTP 400` or `500` errors over the wire). If we get through all of our arguments without running into an error, we're going to evaluate the handler body, write the result out to the requester and close the socket.

### Understanding the Expanders

If you're more interested in the server proper, skip the next few sections. The first thing we're going to do is dissect the code that generates the above expansions, then dive into the internal representation of a bunch of different HTTP-related constructs. We'll start from the end again. In this case, the end of `define-handler.lisp`

	(defmacro define-closing-handler ((name &key (content-type "text/html")) (&rest args) &body body)
	  `(bind-handler ,name (make-closing-handler (:content-type ,content-type) ,args ,@body)))

	(defmacro define-stream-handler ((name) (&rest args) &body body)
	  `(bind-handler ,name (make-stream-handler ,args ,@body)))

The `define-(*)-handler` macros just straight-forwardly expand into calls to `bind-handler` and `make-\1-handler`. You can see that, because we're using homo-iconic code, we can use the backtick and comma operators to basically cut holes in an expression we'd like to evaluate. Calling the resulting macros will slot values into said holes and evaluate the result. We *could* have defined one macro here, with perhaps an extra key argument in the first cluster that let you specify whether it is meant to close out the connection or not. That would have resulted in one slightly complicated `defmacro` rather than two dead-simple ones, so I decided against it, but reserve the right to change my mind later.

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

#### A Short Break -- Briefly Meditating on Macros

Lets take a short break here. At this point we're two levels deep into tree processing. And what we're doing will only make sense to you if you remember that Lisp code is itself represented as a tree. That's what the parentheses are for; they show you how leaves and branches fit together. If you step back, you'll realize we've got a macro definition, `make-closing-handler`, which calls a function, `arguments`, to generate part of the tree its constructing, which in turn calls some tree-manipulating helper functions, including `arg-exp`, to generate its return value. The tree that these functions have as input *happen* to represent Lisp code, and because there's no difference between Lisp code and a tree, you have a transparent syntax definition system. The input is a Lisp expression, and the output is a lisp expression that will be evaluated in its place. Possibly the simplest way of conceptualizing this is as a very simple and minimal Common Lisp to Common Lisp compiler.

#### Another Short Break -- Briefly Meditating on Anaphoric Macros

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

### Defining HTTP Types

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

After unrolling the above macro tower and understanding how it fits together, the rest of this is going to be a pretty straight-forward event-driven server implementation. Since we just talked about the different things `make-stream-handler` and `make-closing-handler` generate to write to the client sockets' stream, this is a nice segue into the `:house` model.

### Object Oriented Programming in Common Lisp

The `:house` model is heavily object based. We've got six core classes, one of which is a new `error`, one utility macro and some default instances to go through.

Lets start with `response` and `sse`, since we just looked at the places they get generated and used.

	(defclass response ()
	  ((content-type :accessor content-type :initform "text/html" :initarg :content-type)
	   (charset :accessor charset :initform "utf-8")
	   (response-code :accessor response-code :initform "200 OK" :initarg :response-code)
	   (keep-alive? :accessor keep-alive? :initform nil :initarg :keep-alive?)
	   (body :accessor body :initform nil :initarg :body)))
	
	(defclass sse ()
	  ((id :reader id :initarg :id :initform nil)
	   (event :reader event :initarg :event :initform nil)
	   (retry :reader retry :initarg :retry :initform nil)
	   (data :reader data :initarg :data)))

If you're joining us from mainstream class/prototype-based languages, you might notice the odd fact that these CLOS (Common Lisp Object System) `class` declarations only involve slots and related getters/setters, or `reader`s/`accessor`s in CL terms. This is because the Lisp object system is based on generic functions. Basically, at a very high level, you need to think "Methods specialize on classes" rather than "classes have methods". That should get you most of the way to understanding.

From a theoretical perspective, the class-focused approach and the function-focused approach can be seen as perpendicular approaches to the same problem. Namely

> How do we treat different classes similarly for the purposes of certain operations that they have in common?

The most common concrete example is the various number implementations. No, an integer is not the same as a real number is not the same as a complex number and so forth, *but*, you can add, multiply, divide and so on each of those. And it'd be nice if you could just express the idea of addition without having to name separate operations for different types when each of them amounts to the same conceptual procedure. The class-focused approach says

> You have different classes you need to deal with. Each such class implements the appropriate methods you want supported.

See [Smalltalk](TODO: link to Pharo here) for the prototypical example of this kind of system. The function-focused approach says

> You have a number of generic operations that can deal with multiple types. When you call one, it dispatches on its arguments to see what concrete implementation it should apply.

That's basically what you'll see in action in Common Lisp. You can think about it as a giant table with "Class Name" down the first column and "Operation" across the first row

            | Addition | Subtraction | Multiplication |
    ----------------------------------------------------------...
    Integer |          |             |                |
	-----------------------------------------------------
	Real    |          |             |                |
	-----------------------------------------------
	Complex |          |             |

and each cell representing the implementation of that operation for that type. Class-focused OO says "Focus on the first column; the class is the important part", function-focused OO says "focus on the first row; the operation needs to be central". Consequently, CF-OO systems tend to group all methods related to a class in with that class' data, whereas FF-OO systems tend to isolate the data completely and group all implementations of an operation together. In the first system, it's difficult to ask "what classes implement method `foo`?" which is easy in the second, but the second has similar problems answering "what are all the methods that specialize on class `bar`?". In a way those questions don't make sense from within the systems we're asking them, and understanding why that is will give you some insight into where you want one or the other.

Anyway, the `response` and `sse` classes above should be fairly self-explanatory if you know approximately how HTTP works. A `response` needs a `content-type`, a `charset`, a `response-code`, a `keep-alive?` flag, and a `body` to be written properly.

-`content-type` is a mime-type for the thing this handler will be returning, It'll most commonly be `text/html`, which is why that's the default. Other common values include `application/json` and `text/plain`.
-`charset` is the character encoding the page uses, which'll always be `utf-8` as far as I know.
-`response-code` is an [HTTP response code](https://en.wikipedia.org/wiki/List_of_HTTP_status_codes). A successful result is `200 OK`. We'll see the common errors covered later.
-`keep-alive?` is a flag that tells us whether to keep the connection active or not. In the context of `:house`, it's only used on stream handlers.
-`body` is hopefully self explanatory.

An `sse` needs a comparably minimal `id`, `event`, `retry` and `data` slots, and those map directly onto the corresponding fields of an SSE message as defined in [the specification](http://dev.w3.org/html5/eventsource/#event-stream-interpretation).

Now, because we're in a function-focused OO system, those slots don't really give you all the information you need. You'll also need to know about the operations we plan to perform on them.

#### Brief cut-over to the core

The core server file, `house.lisp`, defines a `write!` method for two different class combinations; `response/usocket` and `sse/usocket`. Because the system we're in uses multiple dispatch, we also could have defined `response/stream` and `sse/stream` operations just to generalize the method a bit, but I didn't see a real win with that approach. Lets take a look at the implementations for the two classes we're looking at. `response` first.

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

You can see that this operation takes a `response` and a `usocket` (an implementation of sockets for Common Lisp), grabbing a stream from the `usocket` and writing a bunch of lines to it. We locally define the function `write-ln` which takes some number of sequences, and writes them out to the stream followed by a `crlf`. That's just for readability; we could easily have done manual `write-sequence`/`crlf` calls. This is also the example use of `awhen` I promised you. Without that, we'd need to call `(body res)` twice or make other arrangements. That's not expensive in the performance sense, since it's just a value lookup in a `response` instance, but when getting around it is as cheap as adding `a` to the beginning of the containing conditional, you may as well.

	(defmethod write! ((res sse) (sock usocket))
	  (let ((stream (flex-stream sock)))
	    (format stream "~@[id: ~a~%~]~@[event: ~a~%~]~@[retry: ~a~%~]data: ~a~%~%"
		    (id res) (event res) (retry res) (data res))))

Writing an `SSE` out is conceptually similar to, but mechanically different from writing out a `response`. It's much simpler, because an `SSE` only has four slots, one of which is mandatory, and the SSE message standard doesn't specify `CRLF` line-endings, so we can get away with a single `format` call. The `~@[...~]` blocks are conditional directives. Which is to say, if `(id res)` is non-nil, we'll output `id: <the id here> `, and ditto for `event` and `retry`. `data`, the payload of our incremental update, is the only slot that's always present.

### Back to the Model...

That more or less gives you a good idea about the `response` and `SSE` classes; they're the things we serialize back down the socket connection to the client. At the opposite end is the thing we're going to be pumping client input into.

	(defclass request ()
	  ((resource :accessor resource :initarg :resource)
	   (headers :accessor headers :initarg :headers :initform nil)
	   (parameters :accessor parameters :initarg :parameters :initform nil)))

A raw HTTP request is going to be finely sliced and slotted into the above. The `resource` designates which page they want. It'll look something like `/home` or `/foo/bar/baz.css`. `headers` is an association list of incoming HTTP headers (though oddly, we don't need to use any of them for our purposes with House wihch means they'll be parsed out of incoming requests then left to gather dust). Finally, `parameters` is going to hold the association list of all incoming GET and POST parameters. Again though, because we're in an FFOO system, we'll want to take a look at the operations on this class as well as its structure to understand the implications. There are actually two this time; `parse`, which creates `request`s from raw streams, and `handle-request`, which takes a `socket` and a `request` and figures out what to do about them.

### Second brief look at the core

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

	(defmethod parse-params ((params null)) nil)
	(defmethod parse-params ((params string))
	  (loop for pair in (split "&" params)
	     for (name val) = (split "=" pair)
	     collect (cons (->keyword name) (or val ""))))

That set of three methods is sufficient for picking apart a complete, raw request into a `request` instance. You can see that it splits its targets on cross-platform newlines to start with. The first line is going to look something like `GET /uri/the/user/wants.html HTTP/1.x`. Because we're making a minimal, special-purpose server, we can actually do with ignoring the first bit, and all we do with the last is `assert` that it's `HTTP/1.1`. The `path` component of that first line might be a bit more complicated though. Specifically, it might also look like `GET /uri/the/user/wants.html?foo=1&bar=2 HTTP/1.x`, in which case we need to parse the things to the right of the `?` as parameters rather than as part of the URI. That's why we split the path on `?`, and treat it as separate `resource` and `parameters` pieces from then on (incidentally, in languages like Haskell, or Clojure, you'd be able to destructure that `split` call implicitly, thereby avoiding the need for an intermediate `path-pieces` altogether).

The next piece of work we do is creating the `request` instance, initializing it with the appropriate `resource`, then parsing out the headers one by one and finally, optionally parse the GET and POST parameters into the `parameters` slot. There are two flavors of `parameters` there; you can get them from the suffix of the URI, and/or from the request body. Regardless of where they are, they're parsed in the same way, as you can see by the `parse-params` method. That method is specialized on both `string` and `null` because either or both kinds of parameters might be absent from a particular request, and we want to gracefully handle that situation when parsing from the raw results.

The only other thing we need to take a look at here is `handle-request`, which you should already sort of suspect the structure of given how our `define-handler` system is put together.

	(defmethod handle-request ((sock usocket) (req request))
	  (aif (lookup (resource req) *handlers*)
	       (funcall it sock (parameters req))
	       (error! +404+ sock)))

That is, look up the `resource` of that parsed request in the `*handlers*` table. If you find something, call it with the socket and the request `parameters`. If you *don't* find anything, send a `404` error down to the client. And this is the perfect segue into our error model.

### Back to the Model 2

	(define-condition http-assertion-error (error)
	  ((assertion :initarg :assertion :initform nil :reader assertion))
	  (:report (lambda (condition stream)
		     (format stream "Failed assertions '~s'"
			     (assertion condition)))))
	
	(defmacro assert-http (assertion)
	  `(unless ,assertion
	     (error (make-instance 'http-assertion-error :assertion ',assertion))))

This is how you define a new error class in Common Lisp. You `define-condition`, which you can think of as a variant of `defclass` and inherit from `error`, and hand it some options specific to your `error`. In this case, I'm defining an HTTP assertion error, and the only specific things it'll need to know are the actual assertion it's acting on, and a specific way to output itself to a stream. In other languages, you'd call this a method. Here, it's just a function that happens to be the slot value of a class.

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

These are the relevant `4xx` and `5xx`-class HTTP errors that we'll be sending around commonly enough that we just want them globally declared. You can see the `+400+` response that we fed through `error!` up at the top there. It's just an HTTP `response` with a particular `response-code`. I probably could have written a macro to abstract the common parts away, but didn't feel the need for it at the time. You can treat is as an exercise, if you like. Just to confirm that what you thought was happening is actually happening, lets take a look at one more method from the core.

	(defmethod error! ((err response) (sock usocket) &optional instance)
	  (declare (ignorable instance))
	  (ignore-errors 
	    (write! err sock)
	    (socket-close sock)))

It takes an error response and a socket, writes the response to the socket and closes it (ignoring errors, in case the other end has already disconnected). The `instance` argument here is purely for logging/debugging purposes. We'll get into that later.

### Event-Driven Specifics

That more or less concludes the parts of this system that are HTTP-specific. That is, you'd want them all whether you're writing a thread-per-request, or an event-driven server, and they would look the same in either case. The pieces I mentioned removing to streamline this implementation of `:house` for didactic purposes all fall into the same category, by the way, they're either pieces intrinsic to HTTP or to multi-platform Common Lisp applications. The only remaining parts of the server we still need to look at are

1. The event loop itself
2. The buffering subsystem

and

3. The subscription subsystem

These are going to change, whether mildly or radically, depending on what kind of server you're writing and what specifically you want it to do. So with that in mind, it's about time you understood some of the basic decisions in this space and the basis on which they're made.

### The Basics of Event-Driven Servers

At the 10k-foot-level, an HTTP exchange is one request and one response. A client sends a request, which includes a resource identifier, an HTTP version tag, some headers and some parameters. The receiving server parses that request, figures out what to do about it, and sends a response which includes the same HTTP version tag, a response code, some headers and a request body.

And that's it.

Because this is the total of the basic protocol, many minimal servers take the thread-per-request approach. That is, for each incoming request, spin up a thread to do the work of parse/figure-out-what-to-do-about-it/send-response, and spin it down when it's done. The idea is that since each of these connections is very short lived, that won't start up too many threads at once, and it'll let you simplify a lot of the implementation. Specifically, it lets you program as though there were only one connection present at any given time, and it lets you do things like kill orphaned connections just by killing the corresponding thread and letting the garbage collector do its job.

There's a couple of things missing in this description though. First, as described, there's no mechanism for a server to send updates to a client without that client specifically requesting them. Second, there's no identity mechanism, which you need in order to confidently assert that a number of requests come from the same client (or, from the client perspective, to make sure you're making a request from the server you think you're talking to). We won't be solving the second problem in the space of this write-up; a full session implementation would nudge us up to ~560 lines of code, and we've got a hard limit of 500. If you'd like to take a look, feel free peek at the full implementation of `:house` [here](https://github.com/Inaimathi/house). The `session.lisp` file should have what you're after, though a couple of changes you need to make are also part of `handle-request` and the `define-handler` subsystem.

The first problem is interesting though. It's interesting if you've ever wanted to put together multi-user web-applications for whatever reason. The simplest base-case is the anonymous chat room. Consider the situation where you've got two people entering text into a text-box with the intent that both of them should see each message. When Adrian types in a message, you can send him the updated chat room immediately. But if Beatrice were to type a message, according to the system we've described above, there's no built-in way to update Adrian's view of the world with that new message until he makes another request. What you want to be able to do is to push messages from the server at Adrian without him having to take any deliberate action. 

#### Server Push

Here are our options:

##### Comet/Longpoll

Build the client such that it automatically sends the server a new request as soon as it receives a response. Instead of fulfilling that request right away, the server then sits on it until there's new information to send, like say, a new message from Beatrice. The end result is that Adrian gets new updates as soon as they happen, rather than just when he takes action. It's a bit of a semantic distinction though, since the client *is* taking action on his behalf on every update.

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

For an example of option `c`, have a look at Yaws (the [web server](http://hyber.org/), not the [tropical infection](http://en.wikipedia.org/wiki/Yaws)). `b` strikes me as ridiculous. The reason for using a thread-per-request model is that it mechanically simplifies server implementation, but adding the requirement of a separate long-lived connection subsystem seems like it would result in a net complexity *increase*. So, if we want server pushing in the absence of really, *really*, **really** cheap threads, we're dealing with an event-driven server. Which means dealing with non-blocking IO (we'll see why that is later on), and potentially dealing with a single thread.

### High Level

A server is basically going to be

1. Listening for connections on a TCP port
2. When a client connects, read from until you get a complete HTTP request
3. Parse the request
4. Route the parsed request to the appropriate handler
5. Call that handler to generate a response
6. Send the response out to the client

That's the base case, of course. If you *also* want to be pushing data at your clients, there's also two special cases in steps `4` and `5`. Instead of fulfilling a standard request, you might have a situation where

- The requester wants to subscribe to an update feed you have running
- The requester is making a request that requires an update to be published to one or more existing feeds

[[Note to editor: What's the rule on pics? This section seems like it could benefit massively from a diagram or two.]]

### Implementation

Now, we've already seen most of the implementation above, so there's very little left to discuss here. Lets keep going with the current tradition and start from the end, working our way towards the beginning.

#### Subscriptions and Broadcasts

There's basically two operations we need to make SSEs a thing, and they're both very simple. We want to be able to subscribe a particular connection to a channel, and we want to be able to send a message out to all subscribers of a channel, if it exists.

	(defparameter *channels* (make-hash-table))

	(defmethod subscribe! ((channel symbol) (sock usocket))
	  (push sock (gethash channel *channels*))
	  nil)

	(defmethod publish! ((channel symbol) (message string))
	  (awhen (gethash channel *channels*)
	    (loop with msg = (make-instance 'sse :data message)
	       for sock in it (progn (write! msg sock)
				     (force-output (socket-stream sock))))))

There. `*channels*` is a hash table of channel names to subscriber lists. The `subscribe!` method takes a channel and a socket, and adds the socket to the particular channels' subscription list. Finally, `publish!` takes a channel and a message, and publishes that message (in `SSE` format) to each listener on that particular channel. The first two will actually, factually work in a real system, but that `publish!` method is simplified; it leaves out error handling code, and doesn't clean up connections that have stopped listening. All it does is get a channel, then iterate over all of `it`s sockets, calling `write!` on the message we want to publish.

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

#### Buffering

Because we're doing event-driven client handling, we can't just wait on a client connection until we reach connection timeout. If we were writing a thread-per-request server, that approach might make sense because each client would kind of be isolated thanks to the separate thread, but in an event-driven context, if you block on one particular client connection, you block all of them for the duration. That's less than ideal, and it's why I mentioned that we'll have to be using non-blocking IO earlier. If we could block on a connection until a particular timeout, there wouldn't be an issue. However, as it stands we'll want our server to keep moving on to connections that have data ready for reading rather than sticking at the first one in. So what we want is to read all available data from a particular port, check whether what we have so far constitutes a complete request and proceed on that basis. If it *is* complete, then handle it, otherwise buffer the input so far and let it hang around until its turn comes up again. Here's how we do that

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

That method takes a buffer, grabs its stream, notes the read attempt by incrementing `tries`, and finally reads until it hits one of `:eof`, `NIL` or `\r\n\r\n` (which signals a complete request). The function `read-char-no-hang` is the non-blocking part; if the character stream you try to read from has a character waiting, it's returned. Otherwise, the return value is `NIL`, which essentially means "this connection has nothing for us right now, but might in the future". A result of `:eof` means the connection was terminated for some reason other than us disconnecting. Most likely, this means the client disconnected before getting the response back, which means `:eof` translates to "this connection has nothing for us right now, and never will". We'll see how that's handled later. The return value from this method is just the last character return value from `read-char-no-hang`, so that its caller can decide how to proceed. Notice also, we've got this entire operation wrapped in a `handler-case` that'll return `:eof` in the case of any error. More on that in a bit, but first, lets take a look at the `buffer` class.

	(defclass buffer ()
	  ((tries :accessor tries :initform 0)
	   (contents :accessor contents :initform nil)
	   (bi-stream :reader bi-stream :initarg :bi-stream)
	   (found-crlf? :accessor found-crlf? :initform nil)
	   (content-size :accessor content-size :initform 0)
	   (started :reader started :initform (get-universal-time))))

Exactly what you expected, I presume. Storage slots for `tries`, `contents`, the `stream`, a `found-crlf?` flag, a number keeping track of `content-size` and a time-stamp designating when this particular buffer was instantiated. `contents` is where we keep the characters read so far, `bi-stream` is where they come from, and `found-crlf?` is set to `t` by `buffer!` in the event that we read `\r\n\r\n`. The other three are all pieces of tracking data on the basis of which we might want to terminate a connection before sending back a response; in the case that the `buffer` is too old, too big or too needy.

#### The Event Loop Proper

This is it. This is the actual implementation of the core of the server. The piece that actually listens on a port and does stuff with incoming connections. Once you understand this, it's all over bar one or two pieces of interesting trivia.

	(defmethod start ((port integer))
	  (let ((server (socket-listen usocket:*wildcard-host* port :reuse-address t :element-type 'octet))
		    (conns (make-hash-table))
	        (buffers (make-hash-table)))
	    (unwind-protect
		     (loop (loop for ready in (wait-for-input (cons server (alexandria:hash-table-keys conns)) :ready-only t)
			             do (process-ready ready conns buffers)))
	       (loop for c being the hash-keys of conns
		         do (loop while (socket-close c)))
	       (loop while (socket-close server)))))
	
	(defmethod process-ready ((ready stream-server-usocket) (conns hash-table) (buffers hash-table))
	  (setf (gethash (socket-accept ready :element-type 'octet) conns) :on))
	
	(defmethod process-ready ((ready stream-usocket) (conns hash-table) (buffers hash-table))
	  (let ((buf (or (gethash ready buffers)
		             (setf (gethash ready buffers) (make-instance 'buffer :bi-stream (flex-stream ready))))))
	    (when (eq :eof (buffer! buf))
	      (remhash ready conns)
	      (remhash ready buffers)
	      (ignore-errors (socket-close ready)))
	    (let ((complete? (found-crlf? buf))
		      (too-big? (> (content-size buf) +max-request-size+))
		      (too-old? (> (- (get-universal-time) (started buf)) +max-request-age+))
		      (too-needy? (> (tries buf) +max-buffer-tries+)))
	      (when (or complete? too-big? too-old? too-needy?)
		    (remhash ready conns)
		    (remhash ready buffers)
		    (cond (too-big?
			       (error! +413+ ready))
		          ((or too-old? too-needy?)
		           (error! +400+ ready))
		          (t
				   (handler-case
			           (handle-request ready (parse buf))
			         (http-assertion-error () (error! +400+ ready))
			         ((and (not warning)
			               (not simple-error)) (e)
			            (error! +500+ ready e)))))))))

One step at a time here. You're almost there.

`process-ready` takes advantage of Common Lisps' built-in multiple dispatch to figure out what to do in two different situations. The first is handling a ready `stream-server-usocket`.

	(defmethod process-ready ((ready stream-server-usocket) (conns hash-table) (buffers hash-table))
	  (setf (gethash (socket-accept ready :element-type 'octet) conns) :on))

When a `stream-server-usocket` signals that it's `ready`, it means that said server has a connection request. So what we do is accept the connection and store it in the connections table.

The second case is a ready `stream-socket`. When a non-sever `usocket` signals that it's ready, that means it has data ready to be read.

	(defmethod process-ready ((ready stream-usocket) (conns hash-table) (buffers hash-table))
	  (let ((buf (or (gethash ready buffers)
			         (setf (gethash ready buffers) (make-instance 'buffer :bi-stream (flex-stream ready))))))
	    (when (eq :eof (buffer! buf))
	      (remhash ready conns)
	      (remhash ready buffers)
	      (ignore-errors (socket-close ready)))
	    (let ((complete? (found-crlf? buf))
		      (too-big? (> (content-size buf) +max-request-size+))
		      (too-old? (> (- (get-universal-time) (started buf)) +max-request-age+))
		      (too-needy? (> (tries buf) +max-buffer-tries+)))
	      (when (or complete? too-big? too-old? too-needy?)
		    (remhash ready conns)
		    (remhash ready buffers)
		    (cond (too-big?
			       (error! +413+ ready))
		          ((or too-old? too-needy?)
		           (error! +400+ ready))
		          (t
				   (handler-case
			           (handle-request ready (parse buf))
			         (http-assertion-error () (error! +400+ ready))
			         ((and (not warning)
			               (not simple-error)) (e)
			            (error! +500+ ready e)))))))))

So in that case, we need to grab a `buffer`. Either the one associated with this connection, or a fresh one if no associated buffer exists. We then `buffer!` input from the `usocket` into its `buffer`. If the result of that is `:eof`, we just discard the connection and the buffer, and close the socket from our end. If we didn't get back an `:eof`, we need to check a few more conditions. If the resulting `buffer` is any of `complete?`, `too-big?`, `too-old?` or `too-needy?`, we remove it from the buffer and connection tables; one way or the other, we're not dealing with more input from this connection (This would be a bad assumption to make if we were building a websocket server. Try to work out what you'd actually want to do in that case).

If the `buffer` is `too-big?`, we send a `413` error. If it's `too-old?` or `too-needy?`, we send a generic `400` error. Finally, if it's `complete?`, we try to dispatch to a handler. If *that* fails in a way that bubbles up to the `handler-case` clause found here, that means we ran into some execution error along the way, and we need to send a `500` error admitting fault.

Whew.

Ok, last piece.

	(defmethod start ((port integer))
	  (let ((server (socket-listen usocket:*wildcard-host* port :reuse-address t :element-type 'octet))
		    (conns (make-hash-table))
	        (buffers (make-hash-table)))
	    (unwind-protect
		     (loop (loop for ready in (wait-for-input (cons server (alexandria:hash-table-keys conns)) :ready-only t)
			             do (process-ready ready conns buffers)))
	       (loop for c being the hash-keys of conns
		         do (loop while (socket-close c)))
	       (loop while (socket-close server)))))

The top-level method `start` takes a local `port` and listens on it for all incoming connections. It sets up the connection and buffer tables, and then loops forever, waiting for ready signals from all its connections, including the initial listener. If something happens, it dispatches the event using the `process-ready` method we saw above. If any error propagates *this* high up the handler chain, it either means the user has issued an interrupt, or something seriously weird has exploded. In that case, the top level just cleans up after itself and calls it a day.

### All Together Now

That was seriously it. We did the entire thing backwards because that's the path that exposed you to as much Lisp-specific code first and got to the mundane details last, but you now understand exactly how to write an event-driven server in Common Lisp. Having done all of the above, we can finally achieve our goal.

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

Once you fill in the `<insert some javascript UI here>` piece, this will in fact start an HTTP chat server on port `4242` and listen for incoming connections, handling them all appropriately.

And you know exactly how it's happening, down to the sockets.

[[TODO: Take a crack at putting together a light JS UI so that users can actually run this.]]

### Bonus Stage

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

The feature is `:before`/`:after` hooks. And I guess the built-in language feature that `defmethod` isn't some special piece of syntax that can only be run at the top level, but I sort of take that for granted these days. Defining a `:before` hook ona particular method lets you specify a bit of code that'll be executed before every call to that method. `:after` is similar, except the stuff you specify happens after the main method is called. You can specialize these `:before`/`:after` hooks as arbitrarily as the main methods, and only the relevant one will actually run. One possible use for this is the above.

As I mentioned, most of the `:house` server is written using `defmethod`, which means I have plenty of places to hook up debugging/logging statements so I can see what's going on. Clustering all such print statements inside of the `debug!` procedures means that the rest of my code gets to stay unchanged when I need to add a `printf` somewhere, and it means that the `printf`s don't get run unless I specifically ask for them by calling `(debug!)` at some point in my session. 

[[Note to Editor: Anything else I should go over that doesn't easily fit into the body of the piece?]]
