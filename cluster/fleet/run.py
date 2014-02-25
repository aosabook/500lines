from . import client
from . import deterministic_network
from . import member_replicated
from .statemachine import sequence_generator
import logging
import random
import sys

class TimeLogger(logging.Logger):

    def makeRecord(self, name, lvl, fn, lno, msg, args, exc_info, func=None, extra=None):
        extra = extra or {}
        extra['simtime'] = self.network.now
        return logging.Logger.makeRecord(self, name, lvl, fn, lno, msg, args, exc_info, func=func, extra=extra)

def main():
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
    cltt_node = network.new_node()
    clt = client.Client(cltt_node)

    # kill and create nodes often
    def modify():
        if not network.rnd.randint(0, 2):
            # KILL
            if len(network.nodes) > 5:
                victim = network.rnd.choice(network.nodes.keys())
                if victim != cltt_node.address:
                    network.nodes[victim].kill()
        else:
            # create
            if len(network.nodes) < 10:
                node = member_replicated.ClusterMember(network.new_node(), sequence_generator, peers=network.nodes.keys())
                node.start()

        cltt_node.set_timer(network.rnd.uniform(2.0, 3.0), modify)
    cltt_node.set_timer(1.0, modify)

    for member in members:
        member.start()
    clt.start()
    try:
        network.run()
    finally:
        print "RANDOM SEED:", rndseed

if __name__ == "__main__":
    main()
