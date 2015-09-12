import sys, os, BaseHTTPServer

class RequestHandler(BaseHTTPServer.BaseHTTPRequestHandler):

    # Root of files being served.
    Root_Directory = None

    # Is debugging output on?
    Debug = False

    # HTTP error codes.
    ERR_NO_PERM   = 403
    ERR_NOT_FOUND = 404
    ERR_INTERNAL  = 500

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

    # Classify and handle request.
    def do_GET(self):

        self.log("path is '%s'" % self.path)

        # Handle any errors that arise.
        try:

            # Class not yet initialized.
            if self.Root_Directory is None:
                self.err_internal("Root directory not set")
                return

            # Figure out what exactly is being requested.
            abs_path = self.create_abs_path()
            self.log("abs_path is '%s'" % abs_path)

            # It isn't below the root path.
            if not self.is_parent_dir(self.Root_Directory, abs_path):
                self.log("abs_path not below root directory")
                msg = "Path '%s' not below root directory '%s'" % \
                      (abs_path, self.Root_Directory)
                self.err_no_perm(msg)

            # It doesn't exist.
            elif not os.path.exists(abs_path):
                self.log("abs_path doesn't exist")
                self.err_not_found(abs_path)

            # It's a file.
            elif os.path.isfile(abs_path):
                self.log("abs_path is a file")
                self.handle_file(abs_path)

            # It's a directory.
            elif os.path.isdir(abs_path):
                self.log("abs_path is a directory")
                self.handle_dir(abs_path)

            # ...we can't tell.
            else:
                self.log("can't tell what abs_path is")
                self.err_not_found(abs_path)

        # Handle general errors.
        except Exception, msg:
            self.err_internal("Unexpected exception: %s" % msg)

    def create_abs_path(self):
        head = os.path.abspath(self.Root_Directory)
        result = os.path.normpath(head + self.path)
        return result

    def is_parent_dir(self, left, right):
        return os.path.commonprefix([left, right]) == left

    def handle_file(self, abs_path):
        try:
            input = file(abs_path, "r")
            content = input.read()
            input.close()
            self.send_content(content)
        except IOError, msg:
            msg = "'%s' cannot be read: %s" % (self.path, msg)
            self.err_no_perm(msg)

    def handle_dir(self, abs_path):
        try:
            listing = os.listdir(abs_path)
            filler = '\n'.join([(self.Listing_Item % item) for item in listing])
            content = self.Listing_Page % {'path'   : self.path,
                                           'filler' : filler}
            self.send_content(content)
        except IOError, msg:
            msg = "'%s' cannot be listed: %s" % (self.path, msg)
            self.send_error(self.ERR_NO_PERM, msg)

    # Send actual content.
    def send_content(self, content):
        self.send_response(200)
        self.send_header("Content-type", "text/html")
        self.send_header("Content-Length", str(len(content)))
        self.end_headers()
        self.wfile.write(content)

    # Report internal errors.
    def err_internal(self, msg):
        self.send_error(self.ERR_INTERNAL, msg)

    # Handle missing object errors.
    def err_not_found(self, abs_path):
        self.send_error(self.ERR_NOT_FOUND, "'%s' not found" % self.path)

    # Handle no permission errors.
    def err_no_perm(self, msg):
        self.send_error(self.ERR_NO_PERM, msg)

    # Write a log message if in debugging mode
    def log(self, msg):
        if self.Debug:
            print msg

#----------------------------------------------------------------------

if __name__ == '__main__':

    # Main libraries
    import getopt

    # How to use
    Usage = "server.py [-v] root_directory"

    # Handle command-line arguments
    options, rest = getopt.getopt(sys.argv[1:], "v")

    for (flag, arg) in options:
        if flag == "-v":
            RequestHandler.Debug = True
        else:
            print >> sys.stderr, Usage
            sys.exit(1)

    if not rest:
        print >> sys.stderr, Usage
        sys.exit(1)
    root = os.path.abspath(rest[0])
    if not os.path.isdir(root):
        print >> sys.stderr, "No such directory '%s'" % root
        sys.exit(1)
    RequestHandler.Root_Directory = root

    # Create and run server.
    server_address = ('', 8080)
    server = BaseHTTPServer.HTTPServer(server_address, RequestHandler)
    server.serve_forever()
