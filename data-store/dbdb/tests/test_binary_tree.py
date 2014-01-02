import cPickle as pickle
import random

from nose.tools import assert_raises, eq_

from dbdb.binary_tree import BinaryNode, BinaryTree


class TestBinaryTree(object):
    def test_get_missing_key_raises_key_error(self):
        tree = BinaryTree()
        with assert_raises(KeyError):
            tree.get('Not A Key In The Tree')

    def test_set_and_get_key(self):
        tree = BinaryTree()
        tree.set('a', 'b')
        eq_(tree.get('a'), 'b')

    def test_random_set_and_get_keys(self):
        tree = BinaryTree()
        ten_k = list(range(10000))
        pairs = zip(random.sample(ten_k, 10), random.sample(ten_k, 10))
        for k, v in pairs:
            tree.set(k, v)
        for k, v in pairs:
            eq_(tree.get(k), v)

    def test_overwrite_and_get_key(self):
        tree = BinaryTree()
        tree.set('a', 'b')
        tree.set('a', 'c')
        eq_(tree.get('a'), 'c')

    def test_pop_non_existent_key(self):
        tree = BinaryTree()
        with assert_raises(KeyError):
            tree.pop('Not A Key In The Tree')

    def test_del_leaf_key(self):
        tree = BinaryTree()
        tree.set('b', '2')
        tree.pop('b')
        with assert_raises(KeyError):
            tree.get('b')

    def test_del_left_node_key(self):
        tree = BinaryTree()
        tree.set('b', '2')
        tree.set('a', '1')
        tree.pop('b')
        with assert_raises(KeyError):
            tree.get('b')
        tree.get('a')

    def test_del_right_node_key(self):
        tree = BinaryTree()
        tree.set('b', '2')
        tree.set('c', '3')
        tree.pop('b')
        with assert_raises(KeyError):
            tree.get('b')
        tree.get('c')

    def test_del_full_node_key(self):
        tree = BinaryTree()
        tree.set('b', '2')
        tree.set('a', '1')
        tree.set('c', '3')
        tree.pop('b')
        with assert_raises(KeyError):
            tree.get('b')
        tree.get('a')
        tree.get('c')


class TestBinaryNode(object):
    def test_to_string_leaf(self):
        n = BinaryNode(None, 'k', 'v', None)
        pickled = n._to_string()
        d = pickle.loads(pickled)
        eq_(d['left'], None)
        eq_(d['key'], 'k')
        eq_(d['value'], 'v')
        eq_(d['right'], None)

    def test_to_string_nonleaf(self):
        left = BinaryNode(None, 'left', 'left', None)
        left.address = 123
        right = BinaryNode(None, 'right', 'right', None)
        right.address = 321
        n = BinaryNode(left, 'k', 'v', right)
        pickled = n._to_string()
        d = pickle.loads(pickled)
        eq_(d['left'], 123)
        eq_(d['key'], 'k')
        eq_(d['value'], 'v')
        eq_(d['right'], 321)
