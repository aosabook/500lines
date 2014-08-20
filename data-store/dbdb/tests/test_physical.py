import os
import tempfile

from nose.tools import eq_

from dbdb.physical import Storage


class TestStorage(object):

    def setup(self):
        self.f = tempfile.NamedTemporaryFile()
        self.p = Storage(self.f)

    def _get_superblock_and_data(self, value):
        superblock = value[:Storage.SUPERBLOCK_SIZE]
        data = value[Storage.SUPERBLOCK_SIZE:]
        return superblock, data

    def _get_f_contents(self):
        self.f.flush()
        with open(self.f.name, 'rb') as f:
            return f.read()

    def test_init_ensures_superblock(self):
        EMPTY_SUPERBLOCK = (b'\x00' * Storage.SUPERBLOCK_SIZE)
        self.f.seek(0, os.SEEK_END)
        value = self._get_f_contents()
        eq_(value, EMPTY_SUPERBLOCK)

    def test_write(self):
        self.p.write(b'ABCDE')
        value = self._get_f_contents()
        superblock, data = self._get_superblock_and_data(value)
        eq_(data, b'\x00\x00\x00\x00\x00\x00\x00\x05ABCDE')

    def test_read(self):
        self.f.seek(Storage.SUPERBLOCK_SIZE)
        self.f.write(b'\x00\x00\x00\x00\x00\x00\x00\x0801234567')
        value = self.p.read(Storage.SUPERBLOCK_SIZE)
        eq_(value, b'01234567')

    def test_commit_root_address(self):
        self.p.commit_root_address(257)
        root_bytes = self._get_f_contents()[:8]
        eq_(root_bytes, b'\x00\x00\x00\x00\x00\x00\x01\x01')

    def test_get_root_address(self):
        self.f.seek(0)
        self.f.write(b'\x00\x00\x00\x00\x00\x00\x02\x02')
        root_address = self.p.get_root_address()
        eq_(root_address, 514)

    def test_workflow(self):
        a1 = self.p.write(b'one')
        a2 = self.p.write(b'two')
        self.p.commit_root_address(a2)
        a3 = self.p.write(b'three')
        eq_(self.p.get_root_address(), a2)
        a4 = self.p.write(b'four')
        self.p.commit_root_address(a4)
        eq_(self.p.read(a1), b'one')
        eq_(self.p.read(a2), b'two')
        eq_(self.p.read(a3), b'three')
        eq_(self.p.read(a4), b'four')
        eq_(self.p.get_root_address(), a4)
