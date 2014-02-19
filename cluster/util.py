class defaultlist(list):

    def set_len(self, l):
        if l > len(self):
            self.extend([None] * (l - len(self)))

    def __getitem__(self, i):
        self.set_len(i + 1)
        return list.__getitem__(self, i)

    def __setitem__(self, i, v):
        self.set_len(i + 1)
        list.__setitem__(self, i, v)


def view_primary(viewid, peers):
    return peers[viewid % len(peers)]


