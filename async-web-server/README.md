## TODO
- Change interface for handler definitions, and pertinent part of the writeup (singular `define-handler` with a keyword arg to specify `:close-stream?` or not)

# :house
##### Minimal asynchronous Common Lisp web server

`:house` started out as part of [the `:deal` project](https://github.com/Inaimathi/deal) ("deal"? "house"? get it?). I hacked it out in an attempt to generalize it and use it other projects with similar objectives. It's intentionally minimal, single-threaded and stresses readability over efficiency both in its internals and in the interfacing code it requires.

This particular copy of the `:house` project is going to be additionally simplified and de-coupled for explanatory purposes as part of AOSA 2014.

### Usage

##### Quick start

    (define-closing-handler (hello-world :content-type "text/plain") ()
      "Hello world!")
	(house:start 4040)

You should then be able to hop over to a browser and visit `http://localhost:4040/hello-world` to see the plaintext `"Hello world!"` response.

##### Threaded quick start

Because `:house` is single-threaded, using `house:start` directly as above will monopolize your REPL. You might not care about that in certain circumstances, or perhaps your deployment environment can't afford the extra thread. If you do and it can, you should start `:house` in a separate thread to retain your ability to evaluate things against the running system. You can do that the usual way:

    (defparameter *server* (bordeaux-threads:make-thread (lambda () (house:start 4040))

Obviously, you'll need to load `bordeaux-threads` (or whatever threading implementation you want to use) before doing that.

##### Static Files

**House is not a file server**. Static file serving capability is provided for ease of testing, and maybe for *very* small-scale deployment. If you're going to be taking any significant traffic, get a reverse proxy going with something like [`nginx`](http://www.cyberciti.biz/tips/using-nginx-as-reverse-proxy.html).

You can define a static file handler with

    (define-file-handler [file-or-directory])

It'll handle individual files by serving them, and it'll handle directories by serving all contained files recursively.

##### Redirecting

You can set up re-directors with

    (define-redirect-handler (name :permanent? t) "/static/name.html")

Requests for `"/name"` will now instead serve a `301 Moved Permanently` response with a target of `"/static/name.html"` (if you leave out `:permanent? t`, it'll be a `307 Temporary Redirect` instead). House isn't optimized for redirecting, either from the performance or the notation perspective. If you're going to be re-directing any significant number of pages, consider having your reverse proxy handling that too.

##### Using the type annotations

You can specify desired argument types for your handlers. For example:

    (define-closing-handler (handler) ((foo :json) (bar :integer)))
       ...)

You can then use `bar` as an integer and `foo` as an parsed JSON s-expression in the body of that handler. The built-in types are `:string`, `:integer`, `:json`, `:keyword`, `:list-of-keyword` and `:list-of-integer`. If you need a more specific type, you can use `define-http-type`. For example:

    (define-http-type (:game)
	     :type-expression `(gethash ,parameter *game-table*)
	     :lookup-assertion `(typep ,parameter 'game))

Once that's done, you can annotate parameters with the `:game` label.

    (define-closing-handler (handler) ((foo :game) ...) ...)

`foo` will then be looked up in `*game-table*`, and `assert-http`-ed to be of type `'game` before the handler body is evaluated.

All this is entirely optional. If you don't care about it, just pass un-annotated arguments to your handlers, and they'll do exactly what you'd expect. You'll then be able to handle the type-conversion/assertions entirely manually.

### External API

#### Basics
###### `start`

Takes a port-number and starts the server listening on that port.

#### Handlers
###### `define-closing-handler`

Defines a handler that will close its connection when it finishes sending. The handler body has access to three bound symbols in addition to its parameters:

- `sock`: the requesting socket (should only really be used for `subscribe!` calls, but you can also write things to it if you need to send stuff before the request proper)
- `session`: the session belonging to the requesting user
- `parameters`: the raw parameters `alist` (note that each expected parameter is also bound to the corresponding symbol)

###### `define-json-handler`

Defines a `closing-handler` that responds with `"application/json"`, and automatically JSON-encodes its response.

###### `define-stream-handler `

Defines a handler that keeps its connection open when it finishes sending. It's meant to be used for event stream handlers. Has the same bound symbols as `define-closing-handler`.

#### Event Streams
###### `subscribe!`

Subscribes the specified socket to the specified channel. Should only be used with the `define-stream-handler` macro, since the socket will need to be kept open for a subscription to be relevant.

###### `publish!`

Publishes a message to all subscribers of the specified channel.

*The rest is still TODO.*

#### Handler types
###### `define-http-type`

#### Macro-symbols
###### `parameter`
###### `restrictions`
###### `assert-http`
###### `root`
###### `sock`
###### `session`
###### `parameters`

