import argparse
import os
import re
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
                        default=8900,
                        action="store")
    parser.add_argument("--dispatcher-server",
                        help="dispatcher host:port, by default it uses localhost:8888",
                        default="localhost:8888",
                        action="store")
    parser.add_argument("repo", metavar="REPO", type=str,
                        help="path to the repository this will observe")
    parser.add_argument("tests_dir", metavar="TESTS_DIR", type=str,
                        help="test subdirectory")
    args = parser.parse_args()

    # Create the server, binding to localhost on port 9999
    #TODO: add logic to use values above 8900
    runner_host, runner_port = args.host, int(args.port)
    server = ThreadingTCPServer((runner_host, runner_port), TestHandler)
    server.repo_folder = args.repo
    server.test_folder = args.tests_dir

    dispatcher_host, dispatcher_port = args.dispatcher_server.split(":")
    server.dispatcher_server = {"host":dispatcher_host, "port":dispatcher_port}
    response = helpers.communicate(server.dispatcher_server, "register:%s:%s" % (runner_host, runner_port))
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
            self.last_communication = time.time()
            self.request.sendall("pong")
        elif (command == "runtest"):
            #TODO: check our job queue and return either OK or BUSY
            if self.server.busy:
                self.request.sendall("BUSY")
            else:
                #TODO: error handling
                print 'running'
                commit_hash = command_groups.group(2)
                import pdb;pdb.set_trace()
                self.server.busy = True
                results = self.run_tests(commit_hash,
                                         self.server.repo_folder,
                                         self.server.test_folder)
                #TODO dispatch results
                #helpers.communicate(self.server.dispatcher_server, "results:%s:%s" % (commit_hash, results))
                self.server.busy = False
                self.request.sendall("OK")

    def run_tests(self, commit_hash, repo_folder, test_folder):
        # update repo
        cd = subprocess.Popen(['cd', repo_folder],
                              stdout=subprocess.PIPE,
                              stderr=subprocess.STDOUT)
        if cd.wait() != 0:
            raise IOError("Repository folder not found!")
        """
        pull = subprocess.Popen(['git', 'pull'],
                                stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT)
        if pull.wait() != 0:
            raise Exception('Could not successfully call git pull')
        reset = subprocess.Popen(['git', 'reset', '--hard', commit_hash],
                                stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT)
        if reset.wait() != 0:
            raise Exception('Could not successfully update to given commit hash')
        """
        # run the tests
        suite = unittest.TestLoader().discover(test_folder)
        result_file = open('results', 'w')
        program = unittest.TextTestRunner(result_file).run(suite)
        result_file.close()
        result_file = open('results', 'r')
        # give the dispatcher the location of the results
        # NOTE: typically, we upload results to the result server, which will be used by a webinterface
        output = result_file.read()
        send_results = helpers.communicate(self.server.dispatcher_server, "results:%s:%s" % (commit_hash,
                                                                                        output))


if __name__ == "__main__":
    #TODO: use argparse to accept a new coordinator address
    serve()
