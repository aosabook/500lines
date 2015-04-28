import os
import os.path
import shutil
import subprocess
import tempfile

from nose.tools import assert_raises, eq_

import dbdb
import dbdb.tool


class TestDatabase(object):
    def setup(self):
        self.temp_dir = tempfile.mkdtemp()
        self.new_tempfile_name = os.path.join(self.temp_dir, 'new.db')
        self.tempfile_name = os.path.join(self.temp_dir, 'exisitng.db')
        open(self.tempfile_name, 'w').close()

    def teardown(self):
        shutil.rmtree(self.temp_dir, ignore_errors=True)

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
        eq_(len(db), 4)
        db.close()
        db = dbdb.connect(self.tempfile_name)
        eq_(db['a'], 'aye')
        eq_(db['b'], 'bee')
        eq_(db['c'], 'see')
        with assert_raises(KeyError):
            db['d']
        eq_(len(db), 3)
        db.close()


class TestTool(object):
    def setup(self):
        with tempfile.NamedTemporaryFile(delete=False) as temp_f:
            self.tempfile_name = temp_f.name

    def teardown(self):
        os.remove(self.tempfile_name)

    def _tool(self, *args):
        return subprocess.check_output(
            ['python', '-m', 'dbdb.tool', self.tempfile_name] + list(args),
            stderr=subprocess.STDOUT,
        )

    def test_get_non_existent(self):
        self._tool('set', 'a', b'b')
        self._tool('delete', 'a')
        with assert_raises(subprocess.CalledProcessError) as raised:
            self._tool('get', 'a')
        eq_(raised.exception.returncode, dbdb.tool.BAD_KEY)

    def test_tool(self):
        expected = b'b'
        self._tool('set', 'a', expected)
        actual = self._tool('get', 'a')
        eq_(actual, expected)
