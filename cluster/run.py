import json
import logging
import random
import sys
import deterministic_network
import member_replicated
import client
from statemachine import sequence_generator
import BaseHTTPServer
from SimpleHTTPServer import SimpleHTTPRequestHandler
import os

class TimeLogger(logging.Logger):

    def makeRecord(self, name, lvl, fn, lno, msg, args, exc_info, func=None, extra=None):
        extra = extra or {}
        extra['simtime'] = self.core.now
        return logging.Logger.makeRecord(self, name, lvl, fn, lno, msg, args, exc_info, func=func, extra=extra)


def status_json(request, core):
    rv = {}
    for node_name, node in core.nodes.iteritems():
        nrv = rv[node_name] = {}
        for n in dir(node):
            if not (n.startswith('leader_') or n.startswith('acceptor_') or n.startswith('replica_')):
                continue
            v = getattr(node, n)
            if callable(v):
                continue
            if n == 'leader_scout':
                if v:
                    v = {'scout_id': v.scout_id, 'scout_ballot_num': v.scout_ballot_num}
            if n == 'acceptor_accepted':
                v = sorted(v.iteritems())
            if n == 'leader_commanders':
                continue
            nrv[n] = v

    request.send_response(200)
    request.send_header('Content-Type', 'text/json')
    request.end_headers()
    request.wfile.write(json.dumps(rv, sort_keys=True, indent=4, separators=(',', ': ')))


import string
def pick_name(core):
    while True:
        name = 'Node-' + core.rnd.choice(string.ascii_uppercase)
        if name not in core.nodes:
            return name

def run_monitor_web(core):
    class HandlerClass(SimpleHTTPRequestHandler):
        protocol_version = 'HTTP/1.0'
        def log_request(self, *args, **kwargs):
            pass
        def do_GET(self):
            if self.path == '/status':
                return status_json(self, core)
            elif self.path == '/':
                self.path = '/index.html'
            return SimpleHTTPRequestHandler.do_GET(self)
    httpd = BaseHTTPServer.HTTPServer(('', 8010), HandlerClass)

    import threading
    thd = threading.Thread(target=httpd.serve_forever)
    thd.setDaemon(1)
    thd.start()

if __name__ == "__main__":
    if len(sys.argv) == 1:
        rndseed = random.randint(0, sys.maxint)
        pause = False
    else:
        rndseed = int(sys.argv[1])
        pause = True
    print "RANDOM SEED:", rndseed
    core = deterministic_network.Core(rndseed, pause)

    if os.path.exists('run.log'):
        os.unlink('run.log')
    logging.basicConfig(
        format="%(simtime)4.4f - %(name)s - %(message)s", level=logging.DEBUG)
        #filename="run.log")
    logging.setLoggerClass(TimeLogger)
    TimeLogger.core = core

    node_names = ["Node-%s" % (chr(ord('A') + i)) for i in range(4)] 
    cluster_members = node_names + ['Seed']

    # add the seed
    seed = member_replicated.ClusterSeed(initial_state=0)
    seed.set_up_node('Seed', core)

    # set up the nodes
    for name in node_names:
        node = member_replicated.ClusterMember(sequence_generator, peers=cluster_members)
        node.set_up_node(name, core)

    # set up the client
    client = client.Client(node_names)
    client.set_up_node('Client', core)

    # kill and create nodes often
    def modify():
        if not core.rnd.randint(0, 2):
            # KILL
            if len(core.nodes) > 3:
                victim = core.rnd.choice(core.nodes.keys())
                if victim.startswith('Node-'):
                    core.nodes[victim].stop()
        else:
            # create
            if len(core.nodes) < 10:
                node_name = pick_name(core)
                node = member_replicated.ClusterMember(sequence_generator, peers=core.nodes.keys())
                node.set_up_node(node_name, core)
                node.start()

        core.set_timer(core.rnd.uniform(2.0, 3.0), 'Client', modify)
    node.set_timer(1.0, modify)

    run_monitor_web(core)

    core.run()

