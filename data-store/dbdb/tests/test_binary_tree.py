import cPickle as pickle
import random

from nose.tools import assert_raises, eq_

from dbdb.binary_tree import BinaryNode, BinaryTree, NodeRef


class StubStorage(object):
    def __init__(self):
        self.d = [0]
        self.locked = False

    def lock(self):
        if not self.locked:
            self.locked = True
            return True
        else:
            return False

    def unlock(self):
        pass

    def get_root_address(self):
        return 0

    def write(self, string):
        address = len(self.d)
        self.d.append(string)
        return address

    def read(self, address):
        return self.d[address]


class TestBinaryTree(object):
    def setup(self):
        self.tree = BinaryTree(StubStorage())

    def test_get_missing_key_raises_key_error(self):
        with assert_raises(KeyError):
            self.tree.get('Not A Key In The Tree')

    def test_set_and_get_key(self):
        self.tree.set('a', 'b')
        eq_(self.tree.get('a'), 'b')

    def test_random_set_and_get_keys(self):
        ten_k = list(range(10000))
        pairs = zip(random.sample(ten_k, 10), random.sample(ten_k, 10))
        for k, v in pairs:
            self.tree.set(k, v)
        for k, v in pairs:
            eq_(self.tree.get(k), v)

    def test_overwrite_and_get_key(self):
        self.tree.set('a', 'b')
        self.tree.set('a', 'c')
        eq_(self.tree.get('a'), 'c')

    def test_pop_non_existent_key(self):
        with assert_raises(KeyError):
            self.tree.pop('Not A Key In The Tree')

    def test_del_leaf_key(self):
        self.tree.set('b', '2')
        self.tree.pop('b')
        with assert_raises(KeyError):
            self.tree.get('b')

    def test_del_left_node_key(self):
        self.tree.set('b', '2')
        self.tree.set('a', '1')
        self.tree.pop('b')
        with assert_raises(KeyError):
            self.tree.get('b')
        self.tree.get('a')

    def test_del_right_node_key(self):
        self.tree.set('b', '2')
        self.tree.set('c', '3')
        self.tree.pop('b')
        with assert_raises(KeyError):
            self.tree.get('b')
        self.tree.get('c')

    def test_del_full_node_key(self):
        self.tree.set('b', '2')
        self.tree.set('a', '1')
        self.tree.set('c', '3')
        self.tree.pop('b')
        with assert_raises(KeyError):
            self.tree.get('b')
        self.tree.get('a')
        self.tree.get('c')


class TestBinaryNode(object):
    def test_to_string_leaf(self):
        n = BinaryNode(NodeRef(), 'k', 'v', NodeRef())
        pickled = n.to_string()
        d = pickle.loads(pickled)
        eq_(d['left'], 0)
        eq_(d['key'], 'k')
        eq_(d['value'], 'v')
        eq_(d['right'], 0)

    def test_to_string_nonleaf(self):
        left_ref = NodeRef(address=123)
        right_ref = NodeRef(address=321)
        n = BinaryNode(left_ref, 'k', 'v', right_ref)
        pickled = n.to_string()
        d = pickle.loads(pickled)
        eq_(d['left'], 123)
        eq_(d['key'], 'k')
        eq_(d['value'], 'v')
        eq_(d['right'], 321)
