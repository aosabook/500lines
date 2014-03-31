"""
This is the repo observer.

It checks for new commits to the master repo, and will notify the dispatcher of
the latest commit hash, so the dispatcher can dispatch the tests against this
commit hash.
"""
import argparse
import os
import re
import socket
import SocketServer
import subprocess
import sys
import time

import helpers


def bail(reason):
    raise Exception(reason)


def poll():
    parser = argparse.ArgumentParser()
    parser.add_argument("--dispatcher-server",
                        help="dispatcher host:port, " \
                        "by default it uses localhost:8888",
                        default="localhost:8888",
                        action="store")
    parser.add_argument("repo", metavar="REPO", type=str,
                        help="path to the repository this will observe")
    args = parser.parse_args()
    dispatcher_host, dispatcher_port = args.dispatcher_server.split(":")
    while True:
        try:
            # call the bash script that will update the repo and check
            # for changes. If there's a change, it will drop a .commit_hash file
            # with the latest commit in the current working directory
            subprocess.check_output(["./update_repo.sh %s" % args.repo],
                                    shell=True)
            if os.path.isfile(".commit_hash"):
                # great, we have a change! let's execute the tests
                # First, check the status of the dispatcher server to see
                # if we can send the tests
                try:
                    response = helpers.communicate(dispatcher_host,
                                                   int(dispatcher_port),
                                                   "status")
                except socket.error as e:
                    bail("Could not communicate with dispatcher server: %s" % e)
                if response == "OK":
                    # Dispatcher is present, let's send it a test
                    commit = ""
                    with open(".commit_hash", "r") as f:
                        commit = f.readline()
                    response = helpers.communicate(dispatcher_host,
                                                   int(dispatcher_port),
                                                   "dispatch:%s" % commit)
                    if response != "OK":
                        bail("Could not dispatch the test: %s" %
                        response)
                    print "dispatched!"
                else:
                    # Something wrong happened to the dispatcher
                    bail("Could not dispatch the test: %s" %
                    response)
            time.sleep(5)
        except subprocess.CalledProcessError as e:
            bail("Could not update and check repository. Reason: %s" % e.output)


if __name__ == "__main__":
    poll()
