from .. import request
from .. import INVOKE_RETRANSMIT
from . import utils
import mock

CLIENT_ID = 999999


class Tests(utils.ComponentTestCase):

    def setUp(self):
        super(Tests, self).setUp()
        self.callback = mock.Mock(name='callback')
        with mock.patch.object(request.Request, 'client_ids') as client_ids:
            client_ids.next.return_value = CLIENT_ID
            self.req = request.Request(self.member, 10, self.callback)
        self.assertEqual(self.req.client_id, CLIENT_ID)

    def test_function(self):
        """Request should repeatedly send INVOKE until receiving a matching INVOKED"""
        self.req.start()
        self.assertMessage(['F999'], 'INVOKE', caller='F999', client_id=CLIENT_ID, input_value=10)
        self.node.tick(INVOKE_RETRANSMIT)
        self.assertMessage(['F999'], 'INVOKE', caller='F999', client_id=CLIENT_ID, input_value=10)
        self.node.fake_message('INVOKED', client_id=333, output=22)  # non-matching
        self.node.tick(INVOKE_RETRANSMIT)
        self.assertMessage(['F999'], 'INVOKE', caller='F999', client_id=CLIENT_ID, input_value=10)
        self.failIf(self.callback.called)
        self.node.fake_message('INVOKED', client_id=CLIENT_ID, output=20)
        self.callback.assert_called_with(20)
        self.assertUnregistered()
