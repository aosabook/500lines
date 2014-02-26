#!/usr/bin/python
# -*- coding: utf-8 -*-
# XXX somehow we need to handle too many chunks in a segment. Maybe subdirs %100.
from __future__ import print_function

import gzip
import heapq
import itertools
import os
import re
import sys
import shutil
import traceback

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

# Preliminary stats: indexing linux-3.2.41/arch takes 5m35s,
# generating 3.1M postings for 709K distinct terms (from 117MB of source) are
# occupying 17M gzipped (15% of the corpus size) in 755 separate chunks.

# At 4Mi items, we use 269MB of memory and 20m32s.  The index is like
# 18MB instead of the 17MB that it took with a segment size of 1Mi.
# Almost half of the time is spent in running the merge, and more than
# half is spent compressing with gzip.

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
        with gzip.GzipFile(os.path.join(dirname, str(ii)+'.gz'), 'w') as output:  # XXX what about fsync?
            output.writelines("%s %s\n" % item for item in chunk)

def write_new_segment(pathname, postings):
    os.mkdir(pathname)
    write_to_disk(pathname, break_up(postings))
    build_skip_file(pathname)

def merge_segments(dirname, segments):
    if len(segments) == 1:
        return

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
                yield tuple(line.split())

def pathnames(indexdir, terms):
    "Actually evaluate a query."
    return set.intersection(*(set(term_pathnames(indexdir, term))
                              for term in terms))

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
    with open(os.path.join(indexdir, 'skip')) as skip_file:
        for line in sorted(skip_file):
            yield line.split()

def build_skip_file(dirname):
    chunk_names = os.listdir(dirname)
    with open(os.path.join(dirname, 'skip'), 'w') as skip_file: # XXX what about fsync?
        for chunk in chunk_names:
            with gzip.GzipFile(os.path.join(dirname, chunk)) as infile:
                word, pathname = infile.readline().split()
                skip_file.write("%s %s\n" % (word, chunk))

def build_index(indexdir, corpus):
    os.mkdir(indexdir)
    postings = postings_from_dir(corpusdir)
    for ii, chunk in enumerate(sorted_uniq_chunks(postings)):
        write_new_segment(os.path.join(indexdir, str(ii)), chunk)
    merge_segments(indexdir, os.listdir(indexdir))

def grep(indexdir, terms):
    for pathname in pathnames(indexdir, terms):
        try:
            with open(pathname) as text:
                for line in text:
                    if any(term in line for term in terms):
                        sys.stdout.write("%s:%s" % (pathname, line))
        except:                 # The file might e.g. no longer exist.
            traceback.print_exc()

if __name__ == '__main__':
    if sys.argv[1] == 'index':
        build_index(sys.argv[2], sys.argv[3])
    elif sys.argv[1] == 'query':
        for pathname in pathnames(sys.argv[2], sys.argv[3:]):
            print(pathname)
    elif sys.argv[1] == 'grep':
        grep(sys.argv[2], sys.argv[3:])
    else:
        raise Exception("%s (index|query|grep) indexdir ..." % (sys.argv[0]))
