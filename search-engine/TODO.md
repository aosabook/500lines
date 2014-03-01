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