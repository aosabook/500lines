class Scene(object):
    """ Base class for scene nodes.
        Scene nodes currently only include primitives """

    def __init__(self):
        self.node_list = list()

    def render(self):
        for node in self.node_list:
            node.render()

    def add_node(self, node):
        self.node_list.append(node)
