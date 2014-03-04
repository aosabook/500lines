import re
import SocketServer
import time
import argparse

import helpers


class ThreadingTCPServer(SocketServer.ThreadingMixIn, SocketServer.TCPServer):
    dispatcher_server = None
    busy = False


def serve():
    parser = argparse.ArgumentParser()
    parser.add_argument("--host",
                        help="runner's host, by default it uses localhost",
                        default="localhost",
                        action="store")
    parser.add_argument("--port",
                        help="runner's port, by default it uses values >=8900",
                        default=8900,
                        action="store")
    parser.add_argument("--dispatcher-server",
                        help="dispatcher host:port, by default it uses localhost:8888",
                        default="localhost:8888",
                        action="store")
    args = parser.parse_args()

    # Create the server, binding to localhost on port 9999
    #TODO: add logic to use values above 8900
    server = ThreadingTCPServer((args.host, args.port), TestHandler)

    host, port = args.dispatcher_server.split(":")
    server.dispatcher_server = {"host":host, "port":port}
    response = helpers.communicate(server.dispatcher_server, "register:%s:%s" % (args.host, args.port))
    if response != "OK":
        raise("Can't register with dispatcher!")
        sys.exit(1)

    # Activate the server; this will keep running until you
    # interrupt the program with Ctrl-C
    server.serve_forever()


class TestHandler(SocketServer.BaseRequestHandler):
    """
    The RequestHandler class for our server.
    """

    command_re = re.compile(r"""(\w*)(?::(\w*))*""")
    last_communication = None
    REPO_FOLDER = "/Users/mdas/Code/500/repo" # repo location local to test runner's computer

    def handle(self):
        # self.request is the TCP socket connected to the client
        self.data = self.request.recv(1024).strip()
        command_groups = self.command_re.match(self.data)
        command = command_groups.group(1)
        if not command:
            self.request.sendall("Invalid command")
            return
        if (command == "ping"):
            self.last_communication = time.time()
            self.request.sendall("pong")
        elif (command == "runtest"):
            #TODO: check our job queue and return either OK or BUSY
            if self.server.busy:
                self.request.sendall("BUSY")
            else:
                #TODO: error handling
                print 'running'
                path = command_groups.group(2)
                self.server.busy = True
                results = self.run_tests(path)
                #TODO dispatch results
                helpers.communicate(self.server.dispatcher_server, "results:%s:%s" % (path, results))
                self.server.busy = False
                self.request.sendall("OK")

    def run_tests(self, path):
        #TODO: need to dl from url
        #TODO: for now, use local file
        # update repo
        """
        cd = subprocess.Popen(['cd', REPO_FOLDER],
                              stdout=subprocess.PIPE,
                              stderr=subprocess.STDOUT)
        if cd.wait() != "0":
            raise IOError("Repository folder not found!")
        pull = subprocess.Popen(['git', 'pull'],
                                stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT)
        if pull.returncode != 0:
            raise Exception('Could not successfully call git pull')
        with open(self.manifest_path, 'r') as manifest_file:
            tests_to_run = manifest_file.read_lines()
        """
        return


if __name__ == "__main__":
    #TODO: use argparse to accept a new coordinator address
    serve()
