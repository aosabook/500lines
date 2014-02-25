#!/usr/bin/python
# -*- coding: utf-8 -*-
from __future__ import print_function

import gzip
import heapq
import itertools
import os
import re
import sys
import shutil

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

# 1Mi items gives about 60MB of memory usage, which seems about right,
# but may be a bit low.  Weirdly, it took 5m35s the first time, 13m33s
# the second time.

# At 2Mi items, we use 140MB of memory and 11m20s.  Also the index is half a meg bigger.
# At 4Mi items, we use 269MB of memory and 11m8s.  Another half meg bigger again.
# At 8Mi items, we use 536MB of memory and 9m53s, and another half meg.

# Preliminary stats: indexing linux-3.2.41/arch takes 5m35s,
# generating 3.1M postings for 709K distinct terms (from 117MB of source) are
# occupying 17M gzipped (15% of the corpus size) in 755 separate chunks.

# A simple Python program is able to parse about 150 000 lines per
# second looking for a search term, which is some 5× slower than gzip
# is able to decompress; this suggests that the optimal chunk size for
# query speed is perhaps closer to 1500 lines than 4096 lines, 
# at least on my netbook.  This size will work better than 1500 on faster
# machines like the ones in the future.  Ha ha.

# XXX this should probably be called like "break_into_segments" or
# some shit.  2**22 is chosen as the standard max_chunk_size (XXX
# max_segment_size) because that uses typically about a quarter gig,
# which is a reasonable size these days.
def sorted_uniq_chunks(iterator, max_chunk_size=2**22):
    chunk = []
    for item in iterator:
        chunk.append(item)
        if len(chunk) == max_chunk_size:
            yield sorted_uniq_inplace(chunk)
            del chunk[:]

    if chunk:
        yield sorted_uniq_inplace(chunk)

def break_up(seq, chunk_size=4096):
    seq = iter(seq)
    while True:
        yield tuple(itertools.islice(seq, chunk_size)) or next(seq)

def write_to_disk(dirname, chunks):
    for ii, chunk in enumerate(chunks):
        with gzip.GzipFile(os.path.join(dirname, str(ii)+'.gz'), 'w') as output:
            output.writelines("%s %s\n" % item for item in chunk)

def write_new_segment(pathname, postings):
    os.mkdir(pathname)
    write_to_disk(pathname, break_up(postings))
    build_skip_file(pathname)

def merge_segments(dirname, segments):
    postings = heapq.merge(*[read_segment(os.path.join(dirname, segment))
                             for segment in segments])
    ii = 0 # XXX factor out
    while os.path.exists(os.path.join(dirname, str(ii))):
        ii += 1
    write_new_segment(os.path.join(dirname, str(ii)), postings)

    for segment in segments:
        shutil.rmtree(os.path.join(dirname, segment))

def read_segment(pathname):
    for _, chunk in skip_file_entries(pathname):
        with gzip.GzipFile(os.path.join(pathname, chunk)) as infile:
            for line in infile:
                yield line.split()

def pathnames(indexdir, terms):
    "Actually evaluate a query."
    return set.intersection(*(term_pathnames(indexdir, term) for term in terms))

def term_pathnames(indexdir, term):
    segments = (os.path.join(indexdir, segment)
                for segment in os.listdir(indexdir))
    return itertools.chain.from_iterable(segment_term_pathnames(segment, term)
                                         for segment in segments)

def segment_term_pathnames(segment, term):
    for chunk_name in segment_term_chunks(segment, term):
        with gzip.GzipFile(os.path.join(segment, chunk_name)) as chunk:
            for line in chunk:
                term_2, pathname = line.split()
                if term_2 == term:
                    yield pathname
                if term_2 > term:
                    break

def segment_term_chunks(segment, term):
    for headword, chunk in skip_file_entries(segment):
        if headword >= term:
            yield last_chunk
        if headword > term:
            break

        last_chunk = chunk
    else:                   # executed if we don't break
        # XXX what if it was empty?
        yield last_chunk

def skip_file_entries(indexdir):
    with open(os.path.join(pathname, 'skip')) as skip_file:
        for line in sorted(skip_file):
            yield line.split()

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
        write_new_segment(os.path.join(sys.argv[2], str(ii)), chunk)
    #merge_segments(sys.argv[2], os.listdir(sys.argv[2]))
