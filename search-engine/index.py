#!/usr/bin/python
# -*- coding: utf-8 -*-

import gzip
import heapq                    # for heapq.merge
import itertools
import os
import re                       # used to extract words from input
import sys
import shutil                   # to remove directory trees
import traceback
import urllib                   # for quote and unquote
def write_tuples(outfile, tuples):
    for item in tuples:
        line = ' '.join(urllib.quote(str(field)) for field in item)
        outfile.write(line + "\n")
def read_tuples(infile):
    for line in infile:
        yield tuple(urllib.unquote(field) for field in line.split())
# XXX maybe these functions don't need to exist?
def read_metadata(index_path):
    with path['documents'].open() as metadata_file:
        return dict((pathname, (int(mtime), int(size)))
                    for pathname, size, mtime in read_tuples(metadata_file))

# XXX we probably want files_unchanged
def file_unchanged(metadatas, path):
    return any(metadata.get(path.name) == get_metadata(path)
               for metadata in metadatas)
def doc_ids(index_path, terms):
    "Actually evaluate a query."
    doc_id_sets = (set(term_doc_ids(index_path, term)) for term in terms)
    return set.intersection(*doc_id_sets)
def term_doc_ids(index_path, term):
    doc_id_sets = (segment_term_doc_ids(segment, term)
                   for segment in index_segments(index_path))
    return itertools.chain(*doc_id_sets)

# XXX make this a method of the Index object, perhaps returning Segment objects
def index_segments(index_path):
    return [path for path in index_path if path.basename().startswith('seg_')]
class Path:                     # like java.lang.File
    def __init__(self, name):
        self.name = name
    __getitem__  = lambda self, child: Path(os.path.join(self.name, str(child)))
    __contains__ = lambda self, child: os.path.exists(self[child].name)
    __iter__     = lambda self: (self[child] for child in os.listdir(self.name))
    open         = lambda self, *args: open(self.name, *args)
    open_gzipped = lambda self, *args: gzip.GzipFile(self.name, *args)
    basename     = lambda self: os.path.basename(self.name)
    parent       = lambda self: Path(os.path.dirname(self.name))
    abspath      = lambda self: os.path.abspath(self.name)
def segment_term_doc_ids(segment, needle_term):
    for chunk_name in segment_term_chunks(segment, needle_term):
        with segment[chunk_name].open_gzipped() as chunk_file:
            for haystack_term, doc_id in read_tuples(chunk_file):
                if haystack_term == needle_term:
                    yield doc_id
                # Once we reach an alphabetically later term, we're done:
                if haystack_term > needle_term:
                    break
# XXX maybe return Path objects?
def segment_term_chunks(segment, term):
    previous_chunk = None
    for headword, chunk in skip_file_entries(segment):
        if headword >= term:
            if previous_chunk is not None:
                yield previous_chunk
        if headword > term:
            break

        previous_chunk = chunk
    else:                   # executed if we don't break
        if previous_chunk is not None:
            yield previous_chunk
def skip_file_entries(segment_path):
    with segment_path['skip'].open() as skip_file:
        return list(read_tuples(skip_file))
def get_metadata(path):
    s = os.stat(path.name)
    return int(s.st_mtime), int(s.st_size)

def search_ui(index_path, terms):
        # Use the crudest possible ranking: newest (largest mtime) first.
        for path in sorted(paths(index_path, terms),
                           key=get_metadata, reverse=True):
            print(path.name)
# At the moment, our doc_ids are just pathnames; this converts them to Path objects.
def paths(index_path, terms):
    parent = index_path.parent()
    for doc_id in doc_ids(index_path, terms):
        yield Path(os.path.relpath(parent[doc_id].abspath(), start='.'))
# 2**20 is chosen as the maximum segment size because that uses
# typically about a quarter gig, which is a reasonable size these
# days.
segment_size = 2**20

def build_index(index_path, corpus_path, postings_filters):
    os.mkdir(index_path.name)

    # XXX hmm, these should match the doc_ids in the index
    corpus_paths = list(find_documents(corpus_path))
    with index_path['documents'].open('w') as outfile:
        write_tuples(outfile, ((path.name,) + get_metadata(path)
                               for path in corpus_paths))

    postings = tokenize_documents(corpus_paths)
    for filter_function in postings_filters:
        postings = filter_function(postings)

    # XXX at this point we should just pass the fucking doc_id into
    # the analyzer function :(
    parent = index_path.parent()
    rel_paths = dict((path.name, os.path.relpath(path.name, start=parent.name))
                     for path in corpus_paths)
    rel_postings = ((term, rel_paths[doc_id]) for term, doc_id in postings)
    for ii, chunk in enumerate(blocked(rel_postings, segment_size)):
        write_new_segment(index_path['seg_%s' % ii], sorted(chunk))

    merge_segments(index_path, index_segments(index_path))
# From nikipore on Stack Overflow <http://stackoverflow.com/a/19264525>
def blocked(seq, block_size):
    seq = iter(seq)
    while True:
        # XXX for some reason using list(), and then later sorting in
        # place, makes the whole program run twice as slow and doesn't
        # reduce its memory usage.  No idea why.
        block = tuple(itertools.islice(seq, block_size))
        if block:
            yield block
        else:
            raise StopIteration
def find_documents(path):
    for dir_name, _, filenames in os.walk(path.name):
        dir_path = Path(dir_name)
        for filename in filenames:
            yield dir_path[filename]

def tokenize_documents(paths):
    for path in paths:
        for posting in remove_duplicates(tokenize_file(path)):
            yield posting

# Demonstrate a crude set of smart tokenizer frontends.
def tokenize_file(file_path):
    if file_path.name.endswith('.html'):
        return tokenize_html(file_path)
    else:
        return tokenize_text(file_path)

def tokenize_text(file_path):
    word_re = re.compile(r'\w+')
    with file_path.open() as fo:
        for line in fo:
            for word in word_re.findall(line):
                yield word, file_path.name

def remove_duplicates(seq):
    seen_items = set()
    for item in seq:
        if item not in seen_items:
            yield item
            seen_items.add(item)
chunk_size = 4096
def write_new_segment(path, postings):
    os.mkdir(path.name)
    chunks = blocked(postings, chunk_size)
    skip_file_contents = (write_chunk(path, '%s.gz' % ii, chunk)
                          for ii, chunk in enumerate(chunks))
    with path['skip'].open('w') as skip_file:
        write_tuples(skip_file, itertools.chain(*skip_file_contents))

# Yields one skip file entry, or, in the edge case of an empty chunk, none.
def write_chunk(path, filename, chunk):
    with path[filename].open_gzipped('w') as chunk_file:
        write_tuples(chunk_file, chunk)
    if chunk:
        yield chunk[0][0], filename
# Crude approximation of HTML tokenization.  Note that for proper
# excerpt generation (as in the "grep" command) the postings generated
# need to contain position information, because we need to run this
# tokenizer during excerpt generation too.
def tokenize_html(file_path):
    tag_re       = re.compile('<.*?>')
    tag_start_re = re.compile('<.*')
    tag_end_re   = re.compile('.*?>')
    word_re      = re.compile(r'\w+')

    with file_path.open() as fo:
        in_tag = False
        for line in fo:

            if in_tag and tag_end_re.search(line):
                line = tag_end_re.sub('', line)
                in_tag = False

            elif not in_tag:
                line = tag_re.subn('', line)[0]
                if tag_start_re.search(line):
                    in_tag = True
                    line = tag_start_re.sub('', line)
                for term in word_re.findall(line):
                    yield term, file_path.name
def merge_segments(path, segments):
    if len(segments) == 1:
        return

    postings = heapq.merge(*(read_segment(segment)
                             for segment in segments))
    write_new_segment(path['seg_merged'], postings)

    for segment in segments:
        shutil.rmtree(segment.name)
def read_segment(path):
    for _, chunk in skip_file_entries(path):
        # XXX refactor chunk reading?  We open_gzipped in three places now.
        with path[chunk].open_gzipped() as chunk_file:
            for item in read_tuples(chunk_file):
                yield item
def make_stopwords_filter(stopwords):
    stopwords = set(stopwords)
    stopwords |= ( set(word.upper()      for word in stopwords) 
                 | set(word.capitalize() for word in stopwords))
    return lambda postings: ((term, doc_id) for term, doc_id in postings
                             if term not in stopwords)
word_len = 20                   # to eliminate nonsense words
def discard_long_nonsense_words_filter(postings):
    """Drop postings for nonsense words."""
    return ((term, doc_id) for term, doc_id in postings if len(term) < word_len)
def case_insensitive_filter(postings):
    for term, doc_id in postings:
        yield term, doc_id
        if term.lower() != term:
            yield term.lower(), doc_id
def grep(index_path, terms):
    for path in paths(index_path, terms):
        try:
            with path.open() as text:
                for ii, line in enumerate(text, start=1):
                    if any(term in line for term in terms):
                        sys.stdout.write("%s:%s:%s" % (path.name, ii, line))
        except Exception:          # The file might e.g. no longer exist.
            traceback.print_exc()
def main(argv):
    # Eliminate the most common English words from queries and indices.
    stopwords = 'the of and to a in it is was that i for on you he be'.split()

    if argv[1] == 'index':
        build_index(index_path=Path(argv[2]), corpus_path=Path(argv[3]),
                    postings_filters=[discard_long_nonsense_words_filter,
                                      make_stopwords_filter(stopwords),
                                      case_insensitive_filter])
    elif argv[1] == 'query':
        search_ui(Path(argv[2]), [term for term in argv[3:]
                                  if term not in stopwords])
    elif argv[1] == 'grep':
        grep(Path(argv[2]), [term for term in argv[3:]
                             if term not in stopwords])
    else:
        raise Exception("%s (index|query|grep) index_dir ..." % (argv[0]))

if __name__ == '__main__':
    main(sys.argv)
