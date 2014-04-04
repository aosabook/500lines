from .. import ship
from .. import network
from .. import member_replicated
import mock
import unittest

Node = mock.create_autospec(network.Node)
ClusterMember = mock.create_autospec(member_replicated.ClusterMember)
ClusterSeed = mock.create_autospec(member_replicated.ClusterSeed)


class FakeRequest(object):

    def __init__(self, member, input_value, callback):
        self.member = member
        self.input_value = input_value
        self.callback = callback

    def start(self):
        self.callback(('ROTATED', self.member, self.input_value))


class Tests(unittest.TestCase):

    def setUp(self):
        super(Tests, self).setUp()
        self.state_machine = mock.Mock(name='state_machine')
        self.cls_args = dict(node_cls=Node, clustermember_cls=ClusterMember,
                             clusterseed_cls=ClusterSeed)
        for m in self.cls_args.itervalues():
            m.reset_mock()

    def test_Ship_no_seed(self):
        """With no seed, the Ship constructor builds a Node and a ClusterMember"""
        sh = ship.Ship(self.state_machine, port=9999,
                       peers=['p1', 'p2'], **self.cls_args)
        Node.assert_called_once_with(9999)
        self.failIf(ClusterSeed.called)
        ClusterMember.assert_called_with(
            sh.node, execute_fn=self.state_machine, peers=['p1', 'p2'])

    def test_Ship_seed(self):
        """With a seed, the Ship constructor builds a Node and a ClusterSeed"""
        sh = ship.Ship(
            self.state_machine, port=9999, peers=['p1', 'p2'], seed=44,
            **self.cls_args)
        Node.assert_called_once_with(9999)
        ClusterSeed.assert_called_with(
            sh.node, initial_state=44, peers=['p1', 'p2'])
        self.failIf(ClusterMember.called)

    def test_start(self):
        """Ship.start starts the cluster_member and node in self.thread"""
        sh = ship.Ship(self.state_machine, port=9999,
                       peers=['p1', 'p2'], **self.cls_args)
        sh.start()
        sh.thread.join()
        sh.cluster_member.start.assert_called_once_with()
        sh.node.run.assert_called_once_with()

    def test_invoke(self):
        """Ship.invoke makes a new Request, starts it, and waits for its callback to be called."""
        sh = ship.Ship(self.state_machine, port=9999,
                       peers=['p1', 'p2'], **self.cls_args)
        res = sh.invoke('ROTATE', request_cls=FakeRequest)
        self.assertEqual(sh.current_request, None)
        self.assertEqual(res, ('ROTATED', sh.cluster_member, 'ROTATE'))
