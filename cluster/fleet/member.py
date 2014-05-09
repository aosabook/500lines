import logging


class Member(object):  # TODO: rename

    def __init__(self, node):
        self.node = node
        self.address = self.node.address
        self.components = []

    def register(self, component):
        self.components.append(component)
        self.node.register(component)

    def unregister(self, component):
        self.components.remove(component)
        self.node.unregister(component)

    def event(self, message, **kwargs):
        method = 'on_' + message + '_event'
        for comp in self.components:
            if hasattr(comp, method):
                getattr(comp, method)(**kwargs)

    def start(self):
        pass


class Component(object):  # TODO: rename

    def __init__(self, member):
        self.member = member
        self.member.register(self)
        self.address = member.address
        self.logger = logging.getLogger("%s.%s" %
                                        (self.address, type(self).__name__))

    def event(self, message, **kwargs):
        self.member.event(message, **kwargs)

    def send(self, destinations, action, **kwargs):
        self.member.node.send(destinations, action, **kwargs)

    def set_timer(self, seconds, callback):
        return self.member.node.set_timer(seconds, callback)

    def stop(self):
        self.member.unregister(self)
