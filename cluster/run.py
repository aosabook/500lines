import logging
import random
import sys
import deterministic_network
import member_replicated
import client
from statemachine import sequence_generator
import os

class TimeLogger(logging.Logger):

    def makeRecord(self, name, lvl, fn, lno, msg, args, exc_info, func=None, extra=None):
        extra = extra or {}
        extra['simtime'] = self.core.now
        return logging.Logger.makeRecord(self, name, lvl, fn, lno, msg, args, exc_info, func=func, extra=extra)

if __name__ == "__main__":
    if len(sys.argv) == 1:
        rndseed = random.randint(0, sys.maxint)
    else:
        rndseed = int(sys.argv[1])
    print "RANDOM SEED:", rndseed
    core = deterministic_network.Core(rndseed)

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

    # kill node D after 2.3s or so
    def kill():
        core.nodes['Node-D'].stop()
    client.set_timer(2.3, kill)

    core.run()
