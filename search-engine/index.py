#!/usr/bin/python
# -*- coding: utf-8 -*-
# XXX somehow we need to handle too many chunks in a segment. Maybe subdirs %100.

import gzip
import heapq                    # for heapq.merge
import itertools
import os
import re                       # used to extract words from input
import sys
import shutil                   # to remove directory trees
import traceback
import urllib                   # for quote and unquote

class Path:                     # like java.lang.File
    def __init__(self, name):
        self.name = name
    __getitem__  = lambda self, child: Path(os.path.join(self.name, str(child)))
    __contains__ = lambda self, child: os.path.exists(self[child].name)
    __iter__     = lambda self: (self[child] for child in os.listdir(self.name))
    open         = lambda self, *args: open(self.name, *args)
    open_gzipped = lambda self, *args: gzip.GzipFile(self.name, *args)

def postings_from_dir(path):
    # XXX re.compile?
    for dir_name, _, filenames in os.walk(path.name):
        dir_path = Path(dir_name)
        for filename in filenames:
            for posting in remove_duplicates(tokenize_file(dir_path[filename])):
                yield posting

# Demonstrate a crude set of smart tokenizer frontends.
def tokenize_file(file_path):
    if file_path.name.endswith('.html'):
        return tokenize_html(file_path)
    else:
        return tokenize_text(file_path)

def remove_duplicates(seq):
    seen_postings = set()
    for posting in seq:
        if posting not in seen_postings:
            yield posting
            seen_postings.add(posting)

def tokenize_text(file_path):
    word_re = re.compile(r'\w+')
    with file_path.open() as fo:
        for line in fo:
            for word in word_re.findall(line):
                yield word, file_path.name

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

def get_metadata(path):
    s = os.stat(path.name)
    return int(s.st_mtime), int(s.st_size)

def dir_metadata(path):
    for dir_name, _, filenames in os.walk(path.name):
        dir_path = Path(dir_name)
        for filename in filenames:
            file_path = dir_path[filename]
            yield (file_path.name,) + get_metadata(file_path)

def write_tuples(context_manager, tuples):
    with context_manager as outfile:
        for item in tuples:
            line = ' '.join(urllib.quote(str(field)) for field in item)
            outfile.write(line + "\n")

def read_tuples(context_manager):
    with context_manager as infile:
        for line in infile:
            yield tuple(urllib.unquote(field) for field in line.split())

# XXX maybe these functions don't need to exist?
def write_metadata(path, metadata):
    write_tuples(path['metadata'].open('w'), metadata)

def read_metadata(path):
    tuples = read_tuples(path['metadata'].open())
    return dict((pathname, (int(mtime), int(size)))
                for pathname, size, mtime in tuples)

def file_unchanged(metadatas, path):
    return any(metadata.get(path.name) == get_metadata(path)
               for metadata in metadatas)

# From nikipore on Stack Overflow <http://stackoverflow.com/a/19264525>
def blocked(seq, block_size):
    seq = iter(seq)
    while True:
        # XXX for some reason using list(), and then later sorting in
        # place, makes the whole program run twice as slow and doesn't
        # reduce its memory usage.  No idea why.
        yield tuple(itertools.islice(seq, block_size)) or next(seq)

# Yields one skip file entry, or, in the edge case of an empty chunk, none.
def write_chunk(path, index, chunk):
    filename = '%s.gz' % index
    write_tuples(path[filename].open_gzipped('w'), chunk)
    if chunk:
        yield chunk[0][0], filename

def write_new_segment(path, postings):
    os.mkdir(path.name)
    skip_file_contents = (write_chunk(path, ii, chunk)
                          for ii, chunk in enumerate(blocked(postings, 4096)))
    write_tuples(path['skip'].open('w'), itertools.chain(*skip_file_contents))

def skip_file_entries(segment_path):
    return read_tuples(segment_path['skip'].open())

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
        for item in read_tuples(path[chunk].open_gzipped()):
            yield item

# At the moment, our doc_ids are just pathnames; this converts them to Path objects.
def paths(index_path, terms):
    return (Path(doc_id) for doc_id in doc_ids(index_path, terms))

def doc_ids(index_path, terms):
    "Actually evaluate a query."
    return set.intersection(*(set(term_doc_ids(index_path, term))
                              for term in terms))

def term_doc_ids(index_path, term):
    return itertools.chain(*(segment_term_doc_ids(segment, term)
                             for segment in index_path))

def segment_term_doc_ids(segment, term):
    for chunk_name in segment_term_chunks(segment, term):
        tuples = read_tuples(segment[chunk_name].open_gzipped())
        for term_2, doc_id in tuples:
            if term_2 == term:
                yield doc_id
            if term_2 > term:   # Once we reach an alphabetically later term,
                tuples.close()
                break           # we're done.

# XXX maybe return Path objects?
def segment_term_chunks(segment, term):
    last_chunk = None
    for headword, chunk in skip_file_entries(segment):
        if headword >= term:
            if last_chunk is not None:
                yield last_chunk
        if headword > term:
            break

        last_chunk = chunk
    else:                   # executed if we don't break
        # XXX what if it was empty?
        if last_chunk is not None:
            yield last_chunk

# 2**20 is chosen as the maximum segment size because that uses
# typically about a quarter gig, which is a reasonable size these
# days.
def build_index(index_path, corpus_path, postings_filters):
    os.mkdir(index_path.name)

    postings = postings_from_dir(corpus_path)
    for filter_function in postings_filters:
        postings = filter_function(postings)

    for ii, chunk in enumerate(blocked(postings, 2**20)):
        write_new_segment(index_path['seg_%s' % ii], sorted(chunk))

    merge_segments(index_path, list(index_path))

def discard_long_nonsense_words_filter(postings):
    """Drop postings for nonsense words.

    If we are mistakenly indexing binary data or, worse, base64 or
    uuencoded data, we get a huge number of nonsense words that take
    up a lot of space in the index while being totally useless.
    """
    return ((term, doc_id) for term, doc_id in postings if len(term) < 20)

def case_insensitive_filter(postings):
    for term, doc_id in postings:
        yield term, doc_id
        if term.lower() != term:
            yield term.lower(), doc_id

def stopwords_filter(stopwords):
    stopwords = set(stopwords)
    return lambda postings: ((term, doc_id) for term, doc_id in postings
                             if term not in stopwords)

def grep(index_path, terms):
    for path in paths(index_path, terms):
        try:
            with path.open() as text:
                for ii, line in enumerate(text, start=1):
                    if any(term in line for term in terms):
                        sys.stdout.write("%s:%s:%s" % (path.name, ii, line))
        except KeyboardInterrupt:
            return
        except:                 # The file might e.g. no longer exist.
            traceback.print_exc()

def main(argv):
    # Eliminate the most common English words from queries and indices.
    stopwords = 'the of and to a in it is was that i for on you he be'.split()
    stopwords += ([word.upper() for word in stopwords] +
                  [word.capitalize() for word in stopwords])

    if argv[1] == 'index':
        build_index(index_path=Path(argv[2]), corpus_path=Path(argv[3]),
                    postings_filters=[discard_long_nonsense_words_filter,
                                      stopwords_filter(stopwords),
                                      case_insensitive_filter])
    elif argv[1] == 'query':
        search_ui(Path(argv[2]), [term for term in argv[3:]
                                  if term not in stopwords])
    elif argv[1] == 'grep':
        grep(Path(argv[2]), [term for term in argv[3:]
                             if term not in stopwords])
    else:
        raise Exception("%s (index|query|grep) index_dir ..." % (argv[0]))

def search_ui(index_path, terms):
        # Use the crudest possible ranking: newest (largest mtime) first.
        for path in sorted(paths(index_path, terms),
                           key=get_metadata, reverse=True):
            print(path.name)

if __name__ == '__main__':
    main(sys.argv)
