* Unapplied reviewer-derived to-do items:
    * Thank reviewers: Amber Yust, Andrew Kuchling, and Dustin J. Mitchell (done)
    * clean up `read_metadata` and `file_unchanged`
    * make `write_tuples` and `read_tuples` clearer; at least rename
      `context_manager` (to `input_stream` and `output_stream`?),
      explain the exception-safety issues in the text, and maybe move
      the context-management stuff up the stack.  (Done in code, needs text)
    * Explain the reason for wanting to use relative paths in the
      index in the text.
    * `except Exception` (Done)
    * Explain that `line` is newline-terminated in `grep`
    * Explain layout of `class Path` and improve it a bit
    * Explain why `seg_merged` won't collide.
    * Change `, none` to `, zero skip file entries`
    * Explain "skip" isn't really a skip list.  <https://github.com/aosabook/500lines/pull/33#discussion_r15325968>

* Remove incomplete things about merging and incremental updating.  ☹
* Make it, by default, not recurse into `.git` or `.chispa`
* Make default index directory `.chispa`, and search for `.chispa` when
  querying

* Consistently use "doc_id" for document IDs (even though they happen
  to be pathnames)
* Be consistent about "segment", "chunk", and "index".  And "posting".
* Actually make incremental indexing work.
    * When adding new segments, use a sensible merge policy.  Maybe
      shorten the explanation of this in the chapter.
    * Keep track of enough metadata about indexed files to tell when
      they're outdated, or when new files have been added.  Maybe
      inside each segment?
    * Somehow delete old postings.  Maybe blacklist outdated docids
      when merging segments.
* Split up chunks into multiple subdirectories, like Git.
* Track deleted postings so the index doesn't grow without bound.
* Find indices on the path to root and update them automatically,
  perhaps niced in the background.
* Perhaps some kind of abstraction of different pieces: storage
  engine, term extraction engine, stemming, posting contents, etc.
  Term extraction is already
  pretty abstract, in the sense that everything else just treats its
  output as a stream of (term, docid) tuples.
* Deal with fsync and incomplete segments to enable concurrency and
  crashes.
* Maybe encode output in UTF-8 to see if that gets it to run in
  Python3?
* Don't be case-sensitive!
* Maybe make more things be methods of an index object?
* It runs fine in PyPy, and noticeably faster.  Running in Jython 2.5
  involves importing `with` from the future and not trying to import
  `print_statement` from the future.  (Its only use of print works
  fine as either a function or a statement.)  Now I have to see if I
  can get rid of `chain.from_iterable`, which didn't exist in 2.5.
  Yes, but, GzipFile wasn't a context manager in 2.5, so we can't
  close them automatically.
* Maybe exclude .git directories too by default.
* fix bug Darius pointed out where first headword is after term
* Move all the filesystem stuff into a persistence strategy object, so
  that it's theoretically possible to replace it with something that
  uses SQLite or Cassandra or whatever.
* What to do with things like PDF files and .gz files?  There's now a
  super simple HTML tokenizer.
* Maybe canonicalize posting terms to NFC?
* Handle relative pathnames properly.
