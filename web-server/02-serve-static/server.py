import sys, os, BaseHTTPServer

class ServerException(Exception):
    '''For internal error reporting.'''
    pass

class RequestHandler(BaseHTTPServer.BaseHTTPRequestHandler):
    '''
    If the requested path maps to a file, that file is served.  If the
    path maps to a directory, a listing of the directory is
    constructed and served.  If anything goes wrong, an error page is
    constructed.

    This class's RootDirectory variable must be set to the root of the
    files being served before any instances of this class are created.
    '''

    # Root directory (must be set before requests are handled).
    Root_Directory = None

    # How to display a single item in a directory listing.
    Listing_Item = "<li>%s</li>"

    # How to display a whole page of listings.
    Listing_Page = """\
        <html>
        <body>
        <h1>Listing for %(path)s</h1>
        <ul>
        %(filler)s
        </ul>
        </body>
        </html>
        """

    # How to display an error.
    Error_Page = """\
        <html>
        <body>
        <h1>Error accessing %(path)s</h1>
        <p>%(msg)s</p>
        </body>
        </html>
        """

    # Classify and handle request.
    def do_GET(self):

        # Handle any errors that aren't handled elsewhere.
        try:

            # Class not yet initialized.
            if self.Root_Directory is None:
                raise ServerException, "Root directory not set"

            # Figure out what exactly is being requested.
            full_path = self.Root_Directory + self.path

            # It doesn't exist...
            if not os.path.exists(full_path):
                raise ServerException, "'%s' not found" % self.path

            # ...it's a file...
            elif os.path.isfile(full_path):
                self.handle_file(full_path)

            # ...it's a directory...
            elif os.path.isdir(full_path):
                self.handle_dir(full_path)

            # ...we can't tell.
            else:
                raise ServerException, "Unknown object '%s'" % self.path

        # Handle errors.
        except Exception, msg:
            self.handle_error(msg)

    def handle_file(self, full_path):
        try:
            input = file(full_path, "r")
            content = input.read()
            input.close()
            self.send_content(content)
        except IOError, msg:
            msg = "'%s' cannot be read: %s" % (self.path, msg)
            self.handle_error(msg)

    def handle_dir(self, full_path):
        try:
            listing = os.listdir(full_path)
            filler = '\n'.join([(self.Listing_Item % item) for item in listing])
            content = self.Listing_Page % {'path'   : self.path,
                                           'filler' : filler}
            self.send_content(content)
        except IOError, msg:
            msg = "'%s' cannot be listed: %s" % (self.path, msg)
            self.handle_error(msg)

    # Handle unknown objects.
    def handle_error(self, msg):
        content = self.Error_Page % {'path' : self.path,
                                     'msg'  : msg}
        self.send_content(content)

    # Send actual content.
    def send_content(self, content):
        self.send_response(200)
        self.send_header("Content-type", "text/html")
        self.send_header("Content-Length", str(len(content)))
        self.end_headers()
        self.wfile.write(content)

#----------------------------------------------------------------------

if __name__ == '__main__':

    # Set the handler's root directory.
    if len(sys.argv) < 2:
        print >> sys.stderr, "Usage: server.py base_directory"
        sys.exit(1)
    root = os.path.abspath(sys.argv[1])
    if not os.path.isdir(root):
        print >> sys.stderr, "No such directory '%s'" % root
        sys.exit(1)
    RequestHandler.Root_Directory = root

    # Create and run server.
    serverAddress = ('', 8080)
    server = BaseHTTPServer.HTTPServer(serverAddress, RequestHandler)
    server.serve_forever()
