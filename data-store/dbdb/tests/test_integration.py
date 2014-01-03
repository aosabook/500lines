import os
import tempfile

from nose.tools import assert_raises, eq_

import dbdb


class TestDatabase(object):
    def setup(self):
        with tempfile.NamedTemporaryFile(delete=False) as temp_f:
            self.tempfile_name = temp_f.name
        self.new_tempfile_name = self.tempfile_name + 'z'

    def teardown(self):
        for filename in (self.tempfile_name, self.new_tempfile_name):
            try:
                os.remove(filename)
            except:
                pass

    def test_new_database_file(self):
        db = dbdb.connect(self.new_tempfile_name)
        db['a'] = 'aye'
        db.commit()
        db.close()

    def test_persistence(self):
        db = dbdb.connect(self.tempfile_name)
        db['b'] = 'bee'
        db['a'] = 'aye'
        db['c'] = 'see'
        db.commit()
        db['d'] = 'dee'
        db.close()
        db = dbdb.connect(self.tempfile_name)
        eq_(db['a'], 'aye')
        eq_(db['b'], 'bee')
        eq_(db['c'], 'see')
        with assert_raises(KeyError):
            db['d']
        db.close()
