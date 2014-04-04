from .. import member_replicated
from .. import replica
from .. import acceptor
from .. import leader
from .. import commander
from .. import scout
from .. import bootstrap
from .. import seed
from . import utils
import mock


Replica = mock.create_autospec(replica.Replica)
Acceptor = mock.create_autospec(acceptor.Acceptor)
Leader = mock.create_autospec(leader.Leader)
Commander = mock.create_autospec(commander.Commander)
Scout = mock.create_autospec(scout.Scout)
Bootstrap = mock.create_autospec(bootstrap.Bootstrap)
Seed = mock.create_autospec(seed.Seed)


class Tests(utils.ComponentTestCase):

    def setUp(self):
        super(Tests, self).setUp()
        for m in Replica, Acceptor, Leader, Commander, Scout, Bootstrap, Seed:
            m.reset_mock()

    def test_ClusterMember(self):
        """ClusterMember bootstraps initially, and then sets up the other components
        in the bootstrap callback."""
        execute_fn = mock.Mock(name='execute_fn')
        member = member_replicated.ClusterMember(
            self.node, execute_fn, ['p1', 'p2'],
            replica_cls=Replica, acceptor_cls=Acceptor, leader_cls=Leader,
            commander_cls=Commander, scout_cls=Scout, bootstrap_cls=Bootstrap)
        member.start()
        Bootstrap.assert_called_with(member, ['p1', 'p2'], mock.ANY)
        self.failIf(Replica.called)

        # invoke the bootstrap callback and see that it does the right thing
        bootstrap_cb = Bootstrap.call_args_list[0][0][2]
        bootstrap_cb('state', 10, {})

        Replica.assert_called_with(member, execute_fn=execute_fn)
        replica = Replica(member, execute_fn=execute_fn)
        Acceptor.assert_called_with(member)
        Leader.assert_called_with(member, unique_id=self.node.unique_id,
                                  peers=['p1', 'p2'], commander_cls=Commander,
                                  scout_cls=Scout)
        replica.start.assert_called_with(
            state='state', slot_num=10, decisions={},
            peers=['p1', 'p2'])

    def test_ClusterSeed(self):
        """ClusterSeed sets up a Seed component on initialization"""
        seed = member_replicated.ClusterSeed(self.node, 'state', ['p1', 'p2'], seed_cls=Seed)
        Seed.assert_called_with(seed, 'state', ['p1', 'p2'])
