import argparse
import os
import re
import SocketServer
import time

import helpers


class ThreadingTCPServer(SocketServer.ThreadingMixIn, SocketServer.TCPServer):
    runners = []
    error = None


def serve():
    parser = argparse.ArgumentParser()
    parser.add_argument("--host",
                        help="dispatcher's host, by default it uses localhost",
                        default="localhost",
                        action="store")
    parser.add_argument("--port",
                        help="dispatcher's port, by default it uses 8888",
                        default=8888,
                        action="store")
    args = parser.parse_args()

    # Create the server, binding to localhost on port 9999
    server = ThreadingTCPServer((args.host, int(args.port)), DispatcherHandler)

    # Activate the server; this will keep running until you
    # interrupt the program with Ctrl-C
    server.serve_forever()


class DispatcherHandler(SocketServer.BaseRequestHandler):
    """
    The RequestHandler class for our dispatcher.
    This will dispatch test runners against the incoming commit
    and handle their test results
    """

    command_re = re.compile(r"""(\w*)([:]*.*)""")
    last_communication = None
    REPO_FOLDER = "/Users/mdas/Code/500/repo"

    def handle(self):
        # self.request is the TCP socket connected to the client
        self.data = self.request.recv(1024).strip()
        command_groups = self.command_re.match(self.data)
        if not command_groups:
            self.request.sendall("Invalid command")
            return
        command = command_groups.group(1)
        if (command == "status"):
            print "in status"
            self.last_communication = time.time()
            if self.server.error:
                self.request.sendall(self.server.error)
            else:
                self.request.sendall("OK") #TODO: send back ERRORs if anything fails to notify the poller
        elif (command == "dispatch"):
            print "going to dispatch"
            commit_hash = command_groups.group(2)
            if not self.server.runners:
                self.request.sendall("No runners are registered")
            else:
                # The coordinator can trust us to dispatch the test
                self.request.sendall("OK")
                #TODO: add ability to batch tests using manifests
                self.dispatch_tests(commit_hash)
        elif (command == "register"):
            #TODO register the new runner
            print "register"
            address = command_groups.group(2)
            host, port = re.findall(r":(\w*)", address)
            runner = {"host": host, "port":port}
            self.server.runners.append(runner)
            self.request.sendall("OK") #TODO: send back ERRORs if anything fails to notify the poller
        elif (command == "results"):
            #TODO get hash and store results in test_results
            results = command_groups.group(2)
            commit_hash = re.findall(r":(\w*):.*", results)[0]
            if not os.path.exists("test_results"):
                os.makedirs("test_results")
            with open("test_results/%s" % commit_hash, "w") as f:
                data = self.data.split(":")[1:]
                data = "\n".join(data)
                f.write(data)
                f.close()
            import pdb; pdb.set_trace()
            self.request.sendall("OK")
        else:
            self.request.sendall("Invalid command")


    def dispatch_tests(self, commit_hash):
        # let the server know we're going to handle the request
        # TODO: if too many runners are busy for too long, 
        # we should alert the coordinator
        tries = 5
        while (tries > 0):
            for runner in self.server.runners:
                response = helpers.communicate(runner, "runtest:%s" % commit_hash)
                if response == "OK":
                    return
            tries -= 1
            time.sleep(10)
        # TODO: clear this elsewhere.
        self.server.error = "All runners are backed up!"


if __name__ == "__main__":
    #TODO: use argparse to accept a new coordinator address
    serve()
