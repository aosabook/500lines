# A Simple Web Server

FIXME: intro

## Background

Pretty much every program on the web
runs on a family of communication standards called Internet Protocol (IP).
IP breaks messages down into small packets,
each of which is forwarded from one machine to another
along any available route to its destination,
where the whole message is reassembled.

IP is built in layers.
The one that concerns us is the Transmission Control Protocol (TCP/IP).
It guarantees that every packet we send is received,
and that packets are received in the right order.
Putting it another way,
it turns an unreliable stream of disordered packets
into a reliable, ordered stream of data
so that communication between computers looks like reading and writing files.

Programs using IP communicate through sockets.
Each socket is one end of a point-to-point communication channel,
just like a phone is one end of a phone call.
A socket consists of an IP address that identifies a particular machine
and a port number on that machine.
The IP address consists of four 8-bit numbers,
such as `174.136.14.108`.
The Domain Name System (DNS) matches these numbers to symbolic names like `aosabook.org`
that are easier for human beings to remember.

A port number is just a number in the range 0-65535
that uniquely identifies the socket on the host machine.
(If the IP address is like a company's phone number,
then the port number is the extension.)
Ports 0-1023 are reserved for the operating system's use;
anyone else can use the remaining ports.

The Hypertext Transfer Protocol (HTTP) describes one way programs can exchange data over IP.
The communicating parties were originally web browsers and web servers,
but HTTP is now used by many other kinds of applications as well.

In principle,
HTTP is simple:
the client sends a request specifying what it wants over a socket connection,
and the server sends some data in response.
The data may be copied from a file on disk,
generated dynamically by a program,
or some mix of the two.
In all cases,
though,
an HTTP request has the same parts:

FIXME: diagram

The HTTP method is almost always either `"GET"` (to fetch information)
or `"POST"` (to submit form data or upload files).
The URL specifies what the client wants;
it is often a path to a file on disk,
such as `/research/experiments.html`,
but (and this is the crucial part)
it's completely up to the server to decide what to do with it.
The HTTP version is usually `"HTTP/1.0"` or `"HTTP/1.1"`;
the differences between the two don't matter to us.

HTTP headers are key/value pairs like the three shown below:

~~~
Accept: text/html
Accept-Language: en, fr
If-Modified-Since: 16-May-2005
~~~

Unlike the keys in hash tables,
keys may appear any number of times in HTTP headers.
This allows a request to do things like
specify that it's willing to accept several types of content.

Finally,
the body of the request is any extra data associated with the request.
This is used when submitting data via web forms,
when uploading files,
and so on.
There must be a blank line between the last header and the start of the body
to signal the end of the headers;
forgetting it is a common mistake.

One header,
called `Content-Length`,
tells the server how many bytes to expect to read in the body of the request.
There's no magic in any of this:
an HTTP request is just text,
and any program that wants to can create one or parse one.

HTTP responses are formatted like HTTP requests:

FIXME: diagram

The version, headers, and body have the same form and meaning.
The status code is a number indicating what happened when the request was processed:
200 means "everything worked",
404 means "not found",
and other codes have other meanings.
The status phrase repeats that information in a human-readable phrase like "OK" or "not found".

  Code   Name                    Meaning
  ------ ----------------------- ---------------------------------------------------------------------------
  100    Continue                Client should continue sending data
  200    OK                      The request has succeeded
  204    No Content              The server has completed the request, but doesn't need to return any data
  301    Moved Permanently       The requested resource has moved to a new permanent location
  307    Temporary Redirect      The requested resource is temporarily at a different location
  400    Bad Request             The request is badly formatted
  401    Unauthorized            The request requires authentication
  404    Not Found               The requested resource could not be found
  408    Timeout                 The server gave up waiting for the client
  418    I'm a teapot            No, really
  500    Internal Server Error   An error occurred in the server that prevented it fulfilling the request
  601    Connection Timed Out    The server did not respond before the connection timed out

For the purposes of this chapter
there are only two other things we need to know about HTTP
The first is that it is stateless:
each request is handled on its own,
and the server doesn't remember anything between one request and the next.
If an application wants to keep track of something like a user's identity,
it must do so itself.
The usual way to do this is with a cookie,
which is just a short character string that the server sends to the client,
and the client later returns to the server:

FIXME: diagram

When a user signs in,
the server creates a new cookie,
stores it in a database,
and sends it to her browser.
Each time her browser sends the cookie back,
the server uses it to look up information about what the user is doing.

The second is that a URL is often not enough on its own.
For example,
if we're using a search engine,
we have to specify what our search terms are.
We could add these to the path in the URL,
but what we should do is add parameters to the URL.
We do this by adding '?' to the URL
followed by 'key=value' pairs separated by '&amp;'.
For example,
the URL `http://www.google.ca?q=Python`
ask Google to search for pages related to Python:
the key is the letter 'q',
and the value is 'Python'.
The longer query
`http://www.google.ca/search?q=Python&amp;client=Firefox`
tells Google that we're using Firefox,
and so on.
We can pass whatever parameters we want,
but again,
it's up to the application running on the web site to decide
which ones to pay attention to,
and how to interpret them.

Of course,
if '?' and '&amp;' are special characters,
there must be a way to escape them.
The URL encoding standard
represents special characters using '%' followed by a 2-digit code,
and replaces spaces with the '+' character.
Thus,
to search Google for "grade&nbsp;=&nbsp;A+" (with the spaces),
we would use the URL `http://www.google.ca/search?q=grade+%3D+A%2B`.

  Character Encoding
  --------- --------
  #         %23
  $         %24
  %         %25
  &amp;     %26
  +         %2B
  ,         %2C
  /         %2F
  :         %3A
  ;         %3B
  =         %3D
  ?         %3F
  @         %40


Opening sockets, constructing HTTP requests, and parsing responses is tedious,
so most people use libraries to do most of the work.
Python comes with such a library called `urllib2`
(because it's a replacement for an earlier library called `urllib`),
but it exposes a lot of plumbing that most people never want to care about.
Instead,
we recommend using the [Requests](https://pypi.python.org/pypi/requests) library.
Here's an example that uses it to download a page from our web site:

~~~ {.input file="requests-01.py"}
import requests
response = requests.get("http://aosabook.org/en/500lines/testpage.html")
print 'status code:', response.status_code
print 'content length:', response.headers['content-length']
print response.text
~~~
~~~ {.output}
FIXME: paste output once example page is up
~~~

`request.get` does an HTTP GET on a URL
and returns an object containing the response.
That object's `status_code` member is the response's status code;
its `content_length` member  is the number of bytes in the response data,
and `text` is the actual data
(in this case, an HTML page).

Encoding things by hand is very error-prone,
so the Requests library lets us use
a dictionary of key-value pairs instead
via the keyword argument `params`:

~~~ {.input file="requests-02.py"}
import requests
parameters = {'q' : 'Python', 'client' : 'Firefox'}
response = requests.get('http://www.google.com/search', params=parameters)
print 'actual URL:', response.url
~~~
~~~ {.output}
actual URL: http://www.google.com/search?q=Python&client=Firefox
~~~

We should always let the library build the URL for us,
rather than doing it ourselves:
even if there weren't subtleties (and there are),
there's no point duplicating code that's already been written and tested.

## Hello, Web

We're now ready to write our first simple web server.
The basic idea is simple:

1.  wait for someone to connect to our server and send an HTTP request;
2.  parse that request;
3.  figure out what it's asking for;
4.  fetch that data (or generate it dynamically);
5.  format the data as HTML; and
6.  send it back.

Steps 1, 2, and 6 are the same from one application to another,
so the Python standard library has a module called `BaseHTTPServer`
that does those for us.
We just have to take care of steps 3-5,
which we do in the little program below:

~~~ {file="00-hello-web/server.py"}
import BaseHTTPServer

class RequestHandler(BaseHTTPServer.BaseHTTPRequestHandler):
    '''Handle HTTP requests by returning a fixed 'page'.'''

    # Page to send back.
    Page = '''\
<html>
<body>
<p>Hello, web!</p>
</body>
</html>
'''

    # Handle a GET request.
    def do_GET(self):
        self.send_response(200)
        self.send_header("Content-Type", "text/html")
        self.send_header("Content-Length", str(len(self.Page)))
        self.end_headers()
        self.wfile.write(self.Page)

#----------------------------------------------------------------------

if __name__ == '__main__':
    serverAddress = ('', 8080)
    server = BaseHTTPServer.HTTPServer(serverAddress, RequestHandler)
    server.serve_forever()
~~~

The library's `BaseHTTPRequestHandler` class
takes care of parsing the incoming HTTP request
and deciding what method it contains.
If the method is GET,
the class calls a method named `do_GET`.
Our class `RequestHandler` overrides this method
to dynamically generate a simple page:
the text is stored in the class-level variable `Page`,
which we send back to the client after sending
a 200 response code,
a `Content-Type` header telling the client to interpret our data as HTML,
and the page's length.
(The `end_headers` method call inserts the blank line
that separates our headers from the page itself.)

But `RequestHandler` isn't the whole story:
we still need the last three lines to actually start a server running.
The first of these lines defines the server's address as a tuple:
the empty string means "run on the current machine",
and 8080 is the port.
We then create an instance of `BaseHTTPServer.HTTPServer`
with that address and the name of our request handler class as parameters,
then ask it to run forever
(which in practice means until we kill it with Control-C).

If we run this program from the command line,
it doesn't display anything:

~~~
$ python server.py
~~~

If we then go to `http://localhost:8080` with our browser,
though,
we get this in our browser:

~~~
Hello, web!
~~~

and this in our shell:

~~~
127.0.0.1 - - [24/Feb/2014 10:26:28] "GET / HTTP/1.1" 200 -
127.0.0.1 - - [24/Feb/2014 10:26:28] "GET /favicon.ico HTTP/1.1" 200 -
~~~

The first line is straightforward:
since we didn't ask for a particular file,
our browser has asked for '/' (the root directory of whatever the server is serving).
The second line appears because our browser actually sends two requests:
one for the page,
and one for a file called `/favicon.ico`,
which it will display as an icon in the address bar if it exists.
We'll look at this in more detail later.

## Displaying Values

Let's modify our web server to display some of the values
included in the HTTP request.
(We'll do this pretty frequently when debugging,
so we might as well get some practice.)
To keep our code clean,
we'll separate creating the page from sending it:

~~~ {file="01-echo-request-info/server.py"}
class RequestHandler(BaseHTTPServer.BaseHTTPRequestHandler):

    ...page template...

    def do_GET(self):
        page = self.create_page()
        self.send_page(page)

    def create_page(self):
        ...fill in...

    def send_page(self, page):
        ...fill in...
~~~

`send_page` is pretty much what we had before:

~~~ {file="01-echo-request-info/server.py"}
    def send_page(self, page):
        self.send_response(200)
        self.send_header("Content-type", "text/html")
        self.send_header("Content-Length", str(len(page)))
        self.end_headers()
        self.wfile.write(page)
~~~

The template for the page we want to display is
just an HTML table with some formatting placeholders:

~~~ {file="01-echo-request-info/server.py"}
    Page = '''\
<html>
<body>
<table>
<tr>  <td>Date and time</td>  <td>%(date_time)s</td>   </tr>
<tr>  <td>Client host</td>    <td>%(client_host)s</td> </tr>
<tr>  <td>Client port</td>    <td>%(client_port)s</td> </tr>
<tr>  <td>Command</td>        <td>%(command)s</td>     </tr>
<tr>  <td>Path</td>           <td>%(path)s</td>        </tr>
</body>
</html>
'''
~~~

and the method that fills this in is:

~~~ {file="01-echo-request-info/server.py"}
    def create_page(self):
        values = {
            'date_time'   : self.date_time_string(),
            'client_host' : self.client_address[0],
            'client_port' : self.client_address[1],
            'command'     : self.command,
            'path'        : self.path
        }
        page = self.Page % values
        return page
~~~

The main body of the program is unchanged:
as before,
it creates an instance of the `HTTPServer` class
with an address and this request handler as parameters,
then serves requests forever.
If we run it and send a request from a browser
for `http://localhost:8080/something.html`,
we get:

  Header         Value
  ------         -----
  Date and time  Mon, 24 Feb 2014 17:17:12 GMT
  Client host    127.0.0.1
  Client port    54548
  Command        GET
  Path           /something.html

Notice that we do *not* get a 404 error,
even though the page `something.html` doesn't exist.
Our web server isn't doing anything with the URL but echo it;
in particular,
it isn't interpreting it as a file path.
That's up to us.

## Serving Static Pages

The obvious next step is to start serving pages off the disk
instead of generating them on the fly.
We'll start by rewriting `do_GET`:

~~~ {file="02-serve-static/server.py"}
    def do_GET(self):
        try:

            # Figure out what exactly is being requested.
            full_path = os.getcwd() + self.path

            # It doesn't exist...
            if not os.path.exists(full_path):
                raise ServerException("'%s' not found" % self.path)

            # ...it's a file...
            elif os.path.isfile(full_path):
                self.handle_file(full_path)

            # ...it's something we don't handle.
            else:
                raise ServerException("Unknown object '%s'" % self.path)

        # Handle errors.
        except Exception, msg:
            self.handle_error(msg)
~~~

This method assumes that it's allowed to serve any files in or below
the directory that the web server is running in
(which it gets using `os.getcwd`).
It combines this with the path provided in the URL
(which the library automatically puts in `self.path`,
and which always starts with a leading '/')
to get the path to the file the user wants.

If that doesn't exist,
or if it isn't a file,
the method reports an error by raising and catching an exception.
If the path matches a file,
on the other hand,
it calls a helper method named `handle_file`
to read and return the contents.
This method just reads the file
and uses our existing `send_content` to send it back to the client:

~~~ {file="02-serve-static/server.py"}
    def handle_file(self, full_path):
        try:
            with open(full_path, 'r') as input:
                content = input.read()
            self.send_content(content)
        except IOError, msg:
            msg = "'%s' cannot be read: %s" % (self.path, msg)
            self.handle_error(msg)
~~~

To finish off this class,
we need to write the error handling method
and the template for the error reporting page:

~~~ {file="02-serve-static/server.py"}
    Error_Page = """\
        <html>
        <body>
        <h1>Error accessing %(path)s</h1>
        <p>%(msg)s</p>
        </body>
        </html>
        """

    def handle_error(self, msg):
        content = self.Error_Page % {'path' : self.path,
                                     'msg'  : msg}
        self.send_content(content)
~~~

## Listing Directories

FIXME: explain how to list directories (small change to existing code)

FIXME: point out that '..' doesn't work in paths, and explain why
