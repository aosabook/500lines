import argparse
import os
import re
import socket
import SocketServer
import subprocess
import time
import unittest

import helpers


class ThreadingTCPServer(SocketServer.ThreadingMixIn, SocketServer.TCPServer):
    dispatcher_server = None
    busy = False
    repo_folder = None
    test_folder = None


def serve():
    parser = argparse.ArgumentParser()
    parser.add_argument("--host",
                        help="runner's host, by default it uses localhost",
                        default="localhost",
                        action="store")
    parser.add_argument("--port",
                        help="runner's port, by default it uses values >=8900",
                        action="store")
    parser.add_argument("--dispatcher-server",
                        help="dispatcher host:port, by default it uses " \
                        "localhost:8888",
                        default="localhost:8888",
                        action="store")
    parser.add_argument("repo", metavar="REPO", type=str,
                        help="path to the repository this will observe")
    parser.add_argument("tests_dir", metavar="TESTS_DIR", type=str,
                        help="path to the tests folder")
    args = parser.parse_args()

    # Create the server, binding to localhost on port 9999
    runner_host = args.host
    runner_port = None
    tries = 0
    if not args.port:
        runner_port = 8900
        while tries < 100:
            try:
                server = ThreadingTCPServer((runner_host, runner_port),
                                            TestHandler)
                print server
                print runner_port
                break
            except socket.error as e:
                if e.errno == 48:
                    tries+=1
                    runner_port = runner_port + tries
                    continue
                else:
                    raise e
        else:
            raise Exception("Could not bind to ports in range 8900-9000")
    else:
        runner_port = int(args.port)
        server = ThreadingTCPServer((runner_host, runner_port), TestHandler)
    server.repo_folder = args.repo
    server.test_folder = args.tests_dir

    dispatcher_host, dispatcher_port = args.dispatcher_server.split(":")
    server.dispatcher_server = {"host":dispatcher_host, "port":dispatcher_port}
    response = helpers.communicate(server.dispatcher_server, "register:%s:%s" %
                                  (runner_host, runner_port))
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


    def handle(self):
        # self.request is the TCP socket connected to the client
        self.data = self.request.recv(1024).strip()
        command_groups = self.command_re.match(self.data)
        command = command_groups.group(1)
        if not command:
            self.request.sendall("Invalid command")
            return
        if (command == "ping"):
            print "pinged"
            self.last_communication = time.time()
            self.request.sendall("pong")
        elif (command == "runtest"):
            print "got runtest: am I busy? %s" % self.server.busy
            if self.server.busy:
                self.request.sendall("BUSY")
            else:
                self.request.sendall("OK")
                print "running"
                commit_hash = command_groups.group(2)
                self.server.busy = True
                results = self.run_tests(commit_hash,
                                         self.server.repo_folder,
                                         self.server.test_folder)
                self.server.busy = False

    def run_tests(self, commit_hash, repo_folder, test_folder):
        # update repo
        output = subprocess.check_output(["./test_runner_script.sh %s %s" % 
                                        (repo_folder, commit_hash)], shell=True)
        print output
        # run the tests
        suite = unittest.TestLoader().discover(test_folder)
        result_file = open("results", "w")
        program = unittest.TextTestRunner(result_file).run(suite)
        result_file.close()
        result_file = open("results", "r")
        # give the dispatcher the location of the results
        # NOTE: typically, we upload results to the result server,
        # which will be used by a webinterface
        output = result_file.read()
        send_results = helpers.communicate(self.server.dispatcher_server,
                                           "results:%s:%s" % (commit_hash,
                                                              output))


if __name__ == "__main__":
    serve()
