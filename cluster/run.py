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

class TimeLogger(logging.Logger):

    def makeRecord(self, name, lvl, fn, lno, msg, args, exc_info, func=None, extra=None):
        extra = extra or {}
        extra['simtime'] = self.network.now
        return logging.Logger.makeRecord(self, name, lvl, fn, lno, msg, args, exc_info, func=func, extra=extra)


def status_json(request, network):
    rv = {}
    for node_name, node in network.nodes.iteritems():
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
def run_monitor_web(network):
    class HandlerClass(SimpleHTTPRequestHandler):
        protocol_version = 'HTTP/1.0'
        def log_request(self, *args, **kwargs):
            pass
        def do_GET(self):
            if self.path == '/status':
                return status_json(self, network)
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
    network = deterministic_network.Network(rndseed, pause)

    logging.basicConfig(
        format="%(simtime)4.4f - %(name)s - %(message)s", level=logging.DEBUG)
    logging.setLoggerClass(TimeLogger)
    TimeLogger.network = network

    # add the seed
    seed = member_replicated.ClusterSeed(network.new_node(), initial_state=0)

    # set up the members
    members = [member_replicated.ClusterMember(network.new_node(), sequence_generator, peers=[seed.address])
               for _ in range(3)]

    # set up the client
    client_node = network.new_node()
    client = client.Client(client_node)

    # kill and create nodes often
    def modify():
        if not network.rnd.randint(0, 2):
            # KILL
            if len(network.nodes) > 3:
                victim = network.rnd.choice(network.nodes.keys())
                if victim != client_node.address:
                    network.nodes[victim].kill()
        else:
            # create
            if len(network.nodes) < 10:
                node = member_replicated.ClusterMember(network.new_node(), sequence_generator, peers=network.nodes.keys())
                node.start()

        client_node.set_timer(network.rnd.uniform(2.0, 3.0), modify)
    client_node.set_timer(1.0, modify)

#    run_monitor_web(network)

    for member in members:
        member.start()
    client.start()
    network.run()
