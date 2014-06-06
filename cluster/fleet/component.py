class Component(object):  # TODO: rename

    def __init__(self, member):
        self.member = member
        self.member.register(self)
        self.address = member.address
        self.logger = member.node.logger.getChild(type(self).__name__)

    def event(self, message, **kwargs):
        self.member.event(message, **kwargs)

    def send(self, destinations, action, **kwargs):
        self.member.node.send(destinations, action, **kwargs)

    def set_timer(self, seconds, callback):
        return self.member.node.set_timer(seconds, callback)

    def stop(self):
        self.member.unregister(self)

