import sys
import numpy
from node import Sphere, Cube
class Scene(object):
    """ Base class for scene nodes.
        Scene nodes currently only include primitives """

    PLACE_DEPTH = 15.0

    def __init__(self):
        self.node_list = list()
        self.selected_node = None

    def render(self):
        for node in self.node_list:
            node.render()

    def picking(self, start, direction, mat):
        mindist = sys.maxint;
        closest_node = None
        for node in self.node_list:
            node.select(False)
            hit, distance = node.picking(start, direction, mat)
            if hit and distance < mindist:
                closest_node = node
                mindist = distance

        print "Closest hit"
        if closest_node is not None:
            closest_node.select()
            closest_node.depth = mindist
            closest_node.selected_loc = start + direction * mindist
            self.selected_node = closest_node
            print closest_node.selected_loc
        else:
            self.selected_node = None
        print closest_node, mindist

    def move(self, start, direction, mat):
        if self.selected_node is None: return

        node = self.selected_node

        print "MOVE"
        depth = node.depth
        print depth
        oldloc = node.selected_loc
        print oldloc
        newloc = (start + direction * depth)

        print "OLDLOC, NEWLOC"
        print oldloc , newloc

        translation = newloc - oldloc
        pre_tran = numpy.array([translation[0], translation[1], translation[2], 0])
        translation = mat.dot( pre_tran)
        print translation


        node.translate(translation[0], translation[1], translation[2])

        node.selected_loc = newloc


    def add_node(self, node):
        self.node_list.append(node)

    def place(self, shape, start, direction, mat):
        new_node = Sphere()
        if shape == 'sphere':
            new_node = Sphere()
        elif shape == 'cube':
            new_node = Cube()
        new_node.set_color(0.4, 0.4, 0.4)
        self.add_node(new_node)

        # place the node at the cursor
        translation = (start + direction * self.PLACE_DEPTH)
        pre_tran = numpy.array([translation[0], translation[1], translation[2], 0])
        translation = mat.dot(pre_tran)
        new_node.translate(translation[0], translation[1], translation[2])

