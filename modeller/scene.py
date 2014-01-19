import sys
class Scene(object):
    """ Base class for scene nodes.
        Scene nodes currently only include primitives """

    def __init__(self):
        self.node_list = list()

    def render(self):
        for node in self.node_list:
            node.render()

    def picking(self, start, direction, mat):
        mindist = sys.maxint;
        closest_node = None
        for node in self.node_list:
            hit, distance = node.picking(start, direction, mat)
            if hit and distance < mindist:
                closest_node = node
                mindist = distance

        print "Closest hit"
        print closest_node, mindist

    def add_node(self, node):
        self.node_list.append(node)

