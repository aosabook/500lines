Author: Greg Wilson
Project: Web Server
Requirements: Python

This directory holds a simple web server in Python, which I am
building up in stages.  Each sub-directory holds a more complex
version; the final chapter will discuss the changes between these
versions in order to explain features, and show how adding them
requires earlier decisions to be revisited or elaborated.

*   00-hello-web: respond with fixed content.
*   01-echo-request-info: show HTTP request headers.
*   02-serve-static: serve static files and directories.
*   03-errcode-pathnorm: error handling, path normalization, and logging.
*   04-mimetypes: MIME types.
*   05-simple-cgi: basic CGI scripts.
*   06-sockets: replace Python HTTP library with our own socket/parsing code.
