from StringIO import StringIO
import os

from nose.tools import eq_

from dbdb.persister import Persister


class TestPersister(object):
    EMPTY_SUPERBLOCK = ('\x00' * Persister.SUPERBLOCK_SIZE)

    def setup(self):
        self.f = StringIO()
        self.p = Persister(self.f)

    def _get_superblock_and_data(self, value):
        superblock = value[:Persister.SUPERBLOCK_SIZE]
        data = value[Persister.SUPERBLOCK_SIZE:]
        return superblock, data

    def test_init_ensures_superblock(self):
        self.f.seek(0, os.SEEK_END)
        value = self.f.getvalue()
        eq_(value, self.EMPTY_SUPERBLOCK)

    def test_write(self):
        self.p.write('ABCDE')
        value = self.f.getvalue()
        superblock, data = self._get_superblock_and_data(value)
        eq_(data, '\x00\x00\x00\x00\x00\x00\x00\x05ABCDE')

    def test_read(self):
        self.f.seek(Persister.SUPERBLOCK_SIZE)
        self.f.write('\x00\x00\x00\x00\x00\x00\x00\x0801234567')
        value = self.p.read(Persister.SUPERBLOCK_SIZE)
        eq_('01234567', value)
