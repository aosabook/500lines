from .. import member, Join
from . import utils
from . import fake_network
import mock


class MyComp(member.Component):

    def on_birthday_event(self, **kwargs):
        self.birthday = kwargs


class Tests(utils.ComponentTestCase):

    def setUp(self):
        super(Tests, self).setUp()

    def test_registration(self):
        """A component registers itself with the member on creation and
        unregisters on stop."""
        comp = MyComp(self.member)
        self.assertEqual(self.member.components, [comp])
        self.assertEqual(self.member.node.components, [comp])
        comp.stop()
        self.assertEqual(self.member.components, [])
        self.assertEqual(self.member.node.components, [])

    @mock.patch.object(fake_network.FakeNode, 'set_timer')
    def test_timer(self, set_timer):
        """Set_timer calls through to the node"""
        comp = MyComp(self.member)
        t = comp.set_timer(2, 'cb')
        set_timer.assert_called_with(2, 'cb')

    def test_events(self):
        """Events are propagated to all components."""
        comp1 = MyComp(self.member)
        comp2 = MyComp(self.member)
        comp1.event('birthday', arg='val')
        self.assertEqual(comp1.birthday, {'arg': 'val'})
        self.assertEqual(comp2.birthday, {'arg': 'val'})

    def test_send(self):
        """Message-sending calls through to the node"""
        comp = MyComp(self.member)
        comp.send(['p1'], Join())
        self.assertMessage(['p1'], Join())
