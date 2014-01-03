try:
    from cStringIO import StringIO
except ImportError:
    import StringIO

import os

from dbdb.interface import DBDB


def connect(dbname):
    if dbname == ':memory:':
        f = StringIO()
    else:
        try:
            f = open(dbname, 'r+b')
        except IOError:
            fd = os.open(dbname, os.O_RDWR | os.O_CREAT)
            f = os.fdopen(fd, 'r+b')
    return DBDB(f)
