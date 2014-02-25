#!/usr/bin/python
from __future__ import print_function

import itertools
import os
import re
import sys

def postings_from_dir(dirname):
    # XXX re.compile?
    for dirpath, dirnames, filenames in os.walk(dirname):
        for filename in filenames:
            pathname = os.path.join(dirpath, filename)
            with open(pathname) as fo:
                for line in fo:
                    for word in re.findall('\w+', line):
                        yield word, pathname

def sorted_uniq_inplace(lst):
    lst.sort()
    return (k for k, _ in itertools.groupby(lst))

# 1M items gives a few tens to hundreds of megs of memory usage, which
# seems about right.  Preliminary stats: on linux-3.2.41/arch/mips,
# 300k postings for 69k distinct terms (from 11M of source) are
# generated in 15s, occupy 10.6M, and gzip to 1.5M.  split(1)ing the
# file into 302 small files of about 32K each, they gzip to 2.4M.  25%
# of corpus size is not a great index size but probably acceptable;
# 15% would be better.  5K per filesystem file is probably also
# suboptimal.  split -l 10000 gives us instead 31 files, which gzip to
# about 50K eaach, totaling 1.5M (15% of original size).

# Indexing the whole arch/ subdirectory (118M) gives:
# real	9m36.842s
# user	6m27.896s
# sys	0m10.569s
# and a index file which is also 118M, which was nine sorted chunks.

# Splitting it into 8192-line chunks yielded 383 files, which
# compressed to 16M.

# A simple Python program is able to parse about 150 000 lines per
# second looking for a search term, which is some 5× slower than gzip
# is able to decompress; this suggests that the optimal chunk size for
# query speed is perhaps closer to 1500 lines than 8192 lines.  Going
# to 4096 should get most of the benefit (27ms per chunk parsed)
# without hurting compression too much, and will work better on faster
# machines like the ones in the future.  Ha ha.

def sorted_uniq_chunks(iterator, max_chunk_size=1000*1000):
    chunk = []
    for item in iterator:
        chunk.append(item)
        if len(chunk) == max_chunk_size:
            yield sorted_uniq_inplace(chunk)
            del chunk[:]

    if chunk:
        yield sorted_uniq_inplace(chunk)

if __name__ == '__main__':
    for chunk in sorted_uniq_chunks(postings_from_dir(sys.argv[1])):
        for word, pathname in chunk:
            print(word, pathname)
