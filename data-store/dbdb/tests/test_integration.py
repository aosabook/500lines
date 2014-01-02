import os
import tempfile

from nose.tools import eq_

import dbdb


class TestDatabase(object):
    def setup(self):
        with tempfile.NamedTemporaryFile(delete=False) as temp_f:
            self.tempfile_name = temp_f.name

    def teardown(self):
        os.remove(self.tempfile_name)

    def test_persistence(self):
        db = dbdb.connect(self.tempfile_name)
        db['b'] = 'bee'
        db['a'] = 'aye'
        db['c'] = 'see'
        db.commit()
        db.close()
        db = dbdb.connect(self.tempfile_name)
        eq_(db['a'], 'aye')
        eq_(db['b'], 'bee')
        eq_(db['c'], 'see')
        db.close()
