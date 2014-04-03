class FakeNode(object):

    def __init__(self):
        self.unique_id = 999
        self.address = 'F999'
        self.components = []
        self._now = 0.0
        self.timers = []
        self.sent = []

    def register(self, component):
        assert component not in self.components
        self.components.append(component)

    def unregister(self, component):
        assert component in self.components
        self.components.remove(component)

    def set_timer(self, seconds, callback):
        self.timers.append([self._now + seconds, callback, True])
        return self.timers[-1]

    def cancel_timer(self, timer):
        timer[2] = False

    def tick(self, seconds):
        until = self._now + seconds
        self.timers.sort()
        while self.timers and self.timers[0][0] <= until:
            when, callback, active = self.timers.pop(0)
            self._now = when
            if active:
                callback()
        self._now = until

    def get_times(self):
        return sorted([t[0]-self._now for t in self.timers if t[2]])

    def send(self, destinations, action, **kwargs):
        self.sent.append((destinations, action, kwargs))

    def fake_message(self, action, **kwargs):
        for component in self.components:
            fn = getattr(component, 'do_%s' % action)
            fn(**kwargs)
