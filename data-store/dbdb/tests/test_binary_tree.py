from nose.tools import assert_raises, eq_

from dbdb.binary_tree import BinaryTree


class TestBinaryTree(object):
    def test_get_missing_key_raises_key_error(self):
        tree = BinaryTree()
        with assert_raises(KeyError):
            tree['Not A Key In The Tree']

    def test_set_and_get_key(self):
        tree = BinaryTree()
        tree['a'] = 'b'
        eq_(tree['a'], 'b')

    def test_overwrite_and_get_key(self):
        tree = BinaryTree()
        tree['a'] = 'b'
        tree['a'] = 'c'
        eq_(tree['a'], 'c')

    def test_del_key(self):
        tree = BinaryTree()
        tree['a'] = 'b'
        del tree['a']
        with assert_raises(KeyError):
            tree['a']
