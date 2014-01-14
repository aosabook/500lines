import logging
import deterministic_network
import member_replicated
import client
from statemachine import sequence_generator
import os

if __name__ == "__main__":
    if os.path.exists('run.log'):
        os.unlink('run.log')
    logging.basicConfig(
        format="%(name)s %(message)s", level=logging.DEBUG)
        #filename="run.log")

    core = deterministic_network.Core(1235)
    node_names = ["Node-%s" % (chr(ord('A') + i)) for i in range(3)] 
    cluster_members = node_names + ['Seed']
    seed = member_replicated.ClusterSeed(initial_state=0)
    seed.set_up_node('Seed', core)
    for name in node_names:
        node = member_replicated.ClusterMember(sequence_generator, peers=cluster_members)
        node.set_up_node(name, core)
    client = client.Client(cluster_members[0])
    client.set_up_node('Client', core)
    core.run()
