try:
    from cStringIO import StringIO
except ImportError:
    import StringIO

from dbdb.interface import DBDB


def connect(dbname):
    if dbname == ':memory:':
        f = StringIO()
    else:
        f = open(dbname, 'w+b')
    return DBDB(f)
