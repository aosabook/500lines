from __future__ import print_function
import sys

import dbdb


OK = 0
BAD_ARGS = 1
BAD_VERB = 2
BAD_KEY = 3


def usage():
    print("Usage:", file=sys.stderr)
    print("\tpython -m dbdb.tool DBNAME get KEY", file=sys.stderr)
    print("\tpython -m dbdb.tool DBNAME set KEY VALUE", file=sys.stderr)


def main():
    if not (4 <= len(sys.argv) <= 5):
        usage()
        return BAD_ARGS
    dbname, verb, key, value = (sys.argv[1:] + [None])[:4]
    if verb not in ('get', 'set'):
        usage()
        return BAD_VERB
    db = dbdb.connect(dbname)
    if verb == 'get':
        try:
            sys.stdout.write(db[key])
        except KeyError:
            print("Key not found", file=sys.stderr)
            return BAD_KEY
    else:
        db[key] = value
        db.commit()
    return OK


if __name__ == '__main__':
    sys.exit(main())
