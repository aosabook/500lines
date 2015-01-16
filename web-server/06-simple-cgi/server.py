import sys, os, BaseHTTPServer, mimetypes, gettext

class RequestHandler(BaseHTTPServer.BaseHTTPRequestHandler):

    # Root of files being served.
    Root_Directory = None

    # Is debugging output on?
    Debug = False

    # HTTP error codes.
    ERR_NO_PERM   = 403
    ERR_NOT_FOUND = 404
    ERR_INTERNAL  = 500

    # MIME types of files.
    File_Types = mimetypes.types_map

    # Filename extensions that identify executables.
    Exec_Extensions = {
        ".py" : None
    }

    # Classify and handle request.
    def do_GET(self):

        # Handle any errors that arise.
        try:

            # Class not yet initialized.
            if self.Root_Directory is None:
                self.err_internal("Root directory not set")
                return

            # Figure out what exactly is being requested.
            abs_path, query_params = self.parse_path()
            self.log("abs_path is '%s'" % abs_path)
            self.log("query_params is '%s'" % query_params)

            # It isn't below the root path.
            if not self.is_parent_dir(self.Root_Directory, abs_path):
                self.log("abs_path not below root directory")
                self.err_no_perm("Path '%s' not below root directory '%s'" % \
                                 (abs_path, self.Root_Directory))

            # It doesn't exist.
            elif not os.path.exists(abs_path):
                self.log("abs_path doesn't exist")
                self.err_not_found(abs_path)

            # It's a file. (Ignore query parameters if the file is
            # not being executed.)
            elif os.path.isfile(abs_path):
                if self.is_executable(abs_path):
                    self.log("abs_path is an executable")
                    self.handle_executable(abs_path, query_params)
                else:
                    self.log("abs_path is a file")
                    self.handle_static_file(abs_path)

            # It's a directory --- ignore query parameters.
            elif os.path.isdir(abs_path):
                self.log("abs_path is a directory")
                self.handle_dir(abs_path)

            # ...we can't tell.
            else:
                self.log("can't tell what abs_path is")
                self.err_not_found(abs_path)

        # Handle general errors.
        except Exception, msg:
            self.err_internal("Unexpected exception in main despatch: %s" % msg)

    def parse_path(self):
        '''Create the absolute path for a request, and extract the query
        parameter string (if any).'''
        parts = self.path.split("?")
        if len(parts) == 1:
            request_path, queryString = self.path, ""
        elif len(parts) == 2:
            request_path, queryString = parts
        else:
            pass
        head = os.path.abspath(self.Root_Directory)
        result = os.path.normpath(head + request_path)
        return result, queryString

    def is_parent_dir(self, left, right):
        return os.path.commonprefix([left, right]) == left

    def guess_file_type(self, path):
        base, ext = os.path.splitext(path)
        if ext in self.File_Types:
            return self.File_Types[ext]
        ext = ext.lower()
        if ext in self.File_Types:
            return self.File_Types[ext]
        return self.File_Types['']

    def is_executable(self, abs_path):
        '''Does this path map to an executable file?'''
        root, ext = os.path.splitext(abs_path)
        return ext in self.Exec_Extensions

    def handle_static_file(self, abs_path):
        '''Handle static files.  Must read in binary mode!'''
        try:
            input = file(abs_path, "rb")
            content = input.read()
            input.close()
            file_type = self.guess_file_type(abs_path)
            self.send_content(content, file_type)
        except IOError, msg:
            self.err_no_perm("'%s' cannot be read: %s" % (self.path, msg))

    # Handle directories.
    def handle_dir(self, abs_path):

        # How to display a single item in a directory listing.
        listing_item = "<li>%s</li>"

        # How to display a whole page of listings.
        listing_page = \
            "<html>" + \
            "<body>" + \
            "<h1>Listing for " + "%(path)s" + "</h1>" + \
            "<ul>" + \
            "%(filler)s" + \
            "</ul>" + \
            "</body>" + \
            "</html>"

        try:
            listing = os.listdir(abs_path)
            filler = '\n'.join([(listing_item % item) for item in listing])
            content = listing_page % {'path'   : self.path,
                                      'filler' : filler}
            self.send_content(content)
        except IOError, msg:
            self.err_no_perm("'%s' cannot be listed: %s" % msg)

    # Handle executable file.
    def handle_executable(self, abs_path, query_params):
        # Passing query parameters?
        if query_params:
            os.environ["REQUEST_METHOD"] = "GET"
            os.environ["QUERY_STRING"] = query_params
        cmd = "python " + abs_path
        childInput, childOutput = os.popen2(cmd)
        childInput.close()
        response = childOutput.read()
        childOutput.close()
        self.log("handle_executable: response length is %d" % len(response))
        self.send_response(200)
        self.wfile.write(response)

    # Send actual content.
    def send_content(self, content, fileType="text/html"):
        length = str(len(content))
        self.log("sending content, fileType '%s', length %s" % (fileType, length))
        self.send_response(200)
        self.send_header("Content-type", fileType)
        self.send_header("Content-Length", length)
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

    # Handle execution errors.
    def errExec(self, msg):
        self.send_error(self.ERR_NO_PERM, msg)

    # Write a log message if in debugging mode
    def log(self, msg):
        if self.Debug:
            print "nitinat:", msg

#----------------------------------------------------------------------

if __name__ == '__main__':

    # Main libraries
    import getopt

    # How to handle fatal startup errors
    def fatal(msg):
        print >> sys.stderr, "nitinat:", msg
        sys.exit(1)

    # Defaults
    host = ''
    port = 8080
    root = None

    # How to use
    Usage = "server.py [-h host] [-p port] [-v] -r|Root_Directory"

    # Handle command-line arguments
    options, rest = getopt.getopt(sys.argv[1:], "h:p:rv")

    for (flag, arg) in options:
        if flag == "-h":
            host = arg
            if not arg:
                msg = "No host given with -h"
                fatal(msg)
        elif flag == "-p":
            try:
                port = int(arg)
            except ValueError, msg:
                fatal("Unable to convert '%s' to integer: %s" % (arg, msg))
        elif flag == "-r":
            root = os.getcwd()
        elif flag == "-v":
            RequestHandler.Debug = True
        else:
            fatal(Usage)

    # Make sure root directory is set, and is a directory.
    if (root and rest) or (not root and not rest):
        fatal(Usage)
    if not root:
        root = os.path.abspath(rest[0])
    if not os.path.isdir(root):
        fatal("No such directory '%s'" % root)
    RequestHandler.Root_Directory = root

    # Create and run server.
    server_address = (host, port)
    server = BaseHTTPServer.HTTPServer(server_address, RequestHandler)
    server.serve_forever()
