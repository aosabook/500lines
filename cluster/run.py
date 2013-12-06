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
        format="%(name)s %(message)s", level=logging.DEBUG,
        filename="run.log")

    core = deterministic_network.Core(1235)
    cluster_members = ["Node-%s" % (chr(ord('A') + i)) for i in range(3)]
    nodes = []
    for name in cluster_members:
        node = member_replicated.ClusterMember(cluster_members)
        node.set_up_node(name, core)
        nodes.append(node)
    for node in nodes:
        node.initialize_replica(sequence_generator, initial_value=0)
    client = client.Client(cluster_members[0])
    client.set_up_node('Client', core)
    core.run()
