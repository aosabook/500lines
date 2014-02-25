#!/usr/bin/python
from __future__ import print_function

import itertools
import os
import re
import sys

def postings_from_dir(dirname):
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
