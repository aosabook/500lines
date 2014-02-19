class FakeNode(object):

    def __init__(self):
        self.unique_id = 999
        self.address = 'F999'
        self.component = None
        self.timers = []
        self.sent = []

    def register(self, component):
        assert not self.component
        self.component = component

    def set_timer(self, seconds, callable):
        self.timers.append([seconds, callable, True])
        return self.timers[-1]

    def cancel_timer(self, timer):
        timer[2] = False

    def send(self, destinations, action, **kwargs):
        self.sent.append((destinations, action, kwargs))

    def fake_message(self, action, **kwargs):
        fn = getattr(self.component, 'do_%s' % action)
        fn(**kwargs)
