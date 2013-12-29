import random

from nose.tools import assert_raises, eq_

from dbdb.binary_tree import BinaryTree


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
