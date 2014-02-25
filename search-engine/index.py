#!/usr/bin/python
# -*- coding: utf-8 -*-
from __future__ import print_function

import gzip
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

# 1M items gives about 60MB of memory usage, which seems about right,
# but may be a bit low.

# Preliminary stats: indexing linux-3.2.41/arch takes 5m35s,
# generating 3.1M postings for 709K distinct terms (from 117MB of source) are
# occupying 17M gzipped (15% of the corpus size) in 755 separate chunks.

# A simple Python program is able to parse about 150 000 lines per
# second looking for a search term, which is some 5× slower than gzip
# is able to decompress; this suggests that the optimal chunk size for
# query speed is perhaps closer to 1500 lines than 4096 lines, 
# at least on my netbook.  This size will work better than 1500 on faster
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

def break_up(seq, chunk_size=4096):
    chunk = []
    for item in seq:
        chunk.append(item)
        if len(chunk) == chunk_size:
            yield chunk
            chunk = []

    yield chunk

# XXX this needs another level of chunking ahead of it
def write_to_disk(dirname, chunks):
    for ii, chunk in enumerate(chunks):
        with gzip.GzipFile(os.path.join(dirname, str(ii)+'.gz'), 'w') as output:
            output.writelines("%s %s\n" % item for item in chunk)

def build_skip_file(dirname):
    chunk_names = os.listdir(dirname)
    with open(os.path.join(dirname, 'skip'), 'w') as skip_file:
        for chunk in chunk_names:
            with gzip.GzipFile(os.path.join(dirname, chunk)) as infile:
                word, pathname = infile.readline().split()
                skip_file.write("%s %s\n" % (word, chunk))

if __name__ == '__main__':
    os.mkdir(sys.argv[2])
    postings = postings_from_dir(sys.argv[1])
    for ii, chunk in enumerate(sorted_uniq_chunks(postings)):
        subdir = os.path.join(sys.argv[2], str(ii))
        os.mkdir(subdir)
        write_to_disk(subdir, break_up(chunk))
        build_skip_file(subdir)
