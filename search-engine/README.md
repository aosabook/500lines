# A tiny full-text search engine

<!-- XXX name it? "chispa"? Like a very small light, from Lucene. -->

In the last quarter-century,
full-text search engines
have gone from being specialized research tools,
mostly used by lawyers and journalists,
to our most common means of navigating the web.
There are a couple of different basic data structures
for search engines,
but by far the most common one
is the "posting list" or "inverted index" search engine,
which stores a dictionary
from terms (typically words)
to lists of places where those terms are found,
typically in some kind of compressed form;
then it evaluates queries
by looking up the query terms in the dictionary,
merging the resulting lists of places,
and ranking the results.

That doesn’t sound very complicated,
and it turns out that you can make it work very simply indeed,
if you aren’t too concerned about space usage
or scaling to the whole web.

In this chapter, I demonstrate
a posting-list-based search engine,
modeled after Lucene in some ways,
but highly simplified;
it can perform full-text searches
of directory trees in your filesystem,
sort of like `grep -r`,
except that it can search through hundreds of gigabytes
in hundreds of milliseconds,
with an index size about 15% of the size of the text,
although it's pretty slow at indexing.

It’s tuned to perform acceptably
even on electromechanical hard disks
coated with spinning rust.
On my laptop,
a 2.8GHz i7-3840QM,
a slightly earlier version of it
indexed 16 gigabytes of data
from the Project Gutenberg April 2010 DVD,
containing 29 500 ebooks,
in a bit over four hours,
producing a two-gigabyte index,
stored on a spinning-rust disk
with NTFS.
Then it was able to answer queries such as
`moby dick`, `Alice wonderland`, `Nemo squid`, `trochaic chrism`,
or `Bartleby Scrivener`
in between half a second and a second,
from a cold start.

<!-- Originally I said:
on XML dumps from StackOverflow.com
or other StackExchange sites,
thus providing instant help
for all common technical problems.

One problem with this: in the context of StackExchange dumps,
it’s difficult to motivate the need for incremental index updates, but
most of the time, incremental index updates are not optional, and they
can complicate a lot of things.  So I’d like to show that they can be
handled without too much fuss.  The original problem for which I wrote
dumbfts (the engine I’m updating for this chapter) was email indexing,
for which incremental updates are quite clearly motivated; but
achieving incremental updates in that context required that I limit my
mailbox mutation to appending, because once you start mutating mail in
the middle of the file that’s already been indexed, you’re kind of out
of luck on the incremental indexing thing.

So I figured the thing to do
is to make it a generic text-search program like 
Glimpse, storing the index in a directory somewhere up the parent
hierarchy like `.git`, with a list of filenames and mtimes?  Then I
can just reindex files when they change.  `grep -r e1000e
linux-3.2.41` takes almost four minutes on my netbook to search
through the 41706 files present, totaling 609MiB.

In that case, maybe mtime is the best thing to rank by?

The metadata indexing may turn out to be hairier than I expect... I
may still abandon this.

-->

The posting list
----------------

Since this search engine doesn’t do ranking,
it basically comes down to maintaining a posting list
on disk
and querying it.
We just need to be able to quickly find all the files
that contain a given term.
The simplest data structure
that supports this task
would be something like a sorted text file
with a term and a filename on each line;
for example:

    get_ds ./sh/include/asm/segment.h
    get_ds ./x86/include/asm/uaccess.h
    get_eilvt ./x86/kernel/cpu/perf_event_amd_ibs.c
    get_event ./x86/kernel/apm_32.c
    get_event_constraints ./x86/kernel/cpu/perf_event.c
    get_event_constraints ./x86/kernel/cpu/perf_event.h
    get_event_constraints ./x86/kernel/cpu/perf_event_p4.c
    get_exit_info ./x86/include/asm/kvm_host.h

You could binary-search this file
to find all the lines that begin with a given term;
if you have a billion lines in the file,
this might take as many as 60 probes into the file.
On an electromechanical hard disk,
this could take more than half a second,
and it will have to be repeated for each search term.

A somewhat simpler,
although less optimal,
approach
is to break the file up into chunks
with a compact "skip file"
which tells you what each chunk contains.
Then you can read only the chunks
that might contain the term you are looking up.
Then you typically only need to consult
the skip file
and a single chunk
to find all the postings for a term.

There’s a chunk-size tradeoff:
if the chunks are too small, the skip file will be large,
and a single query may need to read many chunks;
on the other hand, if the chunks are too large,
then reading a single chunk will take a long time.

Industrial-strength search engines
identify terms by integer indices into a term dictionary
and documents by integer document IDs,
which allows for delta compression.
Instead, we simply rely on gzip,
which typically makes our index
about 15%
of the size of the original text,
which is reasonable,
but runs more slowly
than application-specific compression schemes.

For this simple engine,
I’ve chosen to put 4096 postings in each chunk,
and each chunk in a separate file.
With my sample dataset of the Linux kernel,
4096 postings is about 20K gzipped (5 bytes per posting),
or about 150K uncompressed,
representing about 150K of original text,
and can be decompressed and parsed
on my netbook
in about 30ms.
The skip file,
which is not compressed,
is about 9 bytes per chunk
on my sample data.
For it to reach 9 megabytes,
you would need to have a million chunks,
or about 150 gigabytes of original source data.
Reading a 9-megabyte file is a bearable startup cost,
since it should take perhaps 200ms,
though far from ideal.

There's a chunk size tradeoff:
smaller chunks
mean more filesystem overhead,
worse compression,
and a larger skip file,
while larger chunks
waste more time decompressing and parsing
postings for terms before the one we're looking for.
4096 postings decompress in about 27ms on my netbook,
which is only about a factor of 3 slower than spinning-rust seek times,
but achieve most of the compression available from gzip.

Scaling up further
can be done
by using multiple levels of skip files,
making the search engine's run time proportional
to the logarithm of the number of postings
rather than its square root.

Sequential access
-----------------

To build full-text indices on spinning-rust electromechanical disks,
it’s important that the access patterns
be basically sequential.
Random access on spinning rust
involves a delay on the order of 8–12 milliseconds,
during which time
the disk could have transferred
on the order of half a megabyte of data,
if it weren’t busy seeking.
So every random seek
costs you half a megabyte of data transfer time;
if you are doing the seek to transfer much less data than that,
then the disk is spending most of its time seeking
instead of transferring data.
On the other hand,
if you are transferring much more than half a megabyte
for each seek,
then the disk’s transfer rate is close to its maximum possible.
If you have a networking background,
you could think of this number as the bandwidth-delay product
of the disk.

Nowadays, since we have a lot of RAM,
we can build fairly large indices in RAM
before writing them out to disk.
This engine <!-- XXX chispa? --> by default builds up
4 million postings in RAM
which takes up around a quarter gig of RAM
before sorting them and writing them to a file,
which typically ends up being about 12MB compressed.

On modern solid-state drives,
this kind of locality of reference
is less of a problem,
since they can handle some ten thousand "seeks" per second;
the corresponding bandwidth-delay product
is more like 20 kilobytes
rather than 500.

The classic algorithm
for producing a sorted sequence
on media that only support sequential access
is mergesort.
To index a large volume of data,
first we index blocks of it,
producing these primary index segments of some 3MB;
then, we merge the primary index segments
to produce a merged index segment.
For a sufficiently large dataset and small RAM,
we could imagine needing to do a multi-pass merge,
but we probably don’t need to worry about that nowadays;
for efficient merging,
we need only about half a megabyte of buffer memory
per input file,
so a low-end modern smartphone
with a gigabyte of RAM
can do a 2000-way merge,
merging 2000 primary segments into one merged segment,
which would be some 6GB in size,
indexing some 40 gigabytes of text.
We could create bigger primary index segments
at the cost of bogging down the computer,
up to ten times as big on that smartphone;
that would allow us to
index up to 400 gigabytes
in only two passes.
With this engine's current primary segment size
of about 13 megabytes compressed,
indexing about 100 megabytes uncompressed,
this strategy only scales up to 200 gigabytes.

Since we're restricting ourselves
to essentially sequential access
for efficiency,
the filesystem interface is very simple.
Each file
(compressed or not)
contains a sequence of tuples
whose elements are arbitrary strings.
We have a function
which takes a sequence
(in Python, known as an iterable)
and writes its contents
to a file of tuples:

    def write_tuples(context_manager, tuples):
        with context_manager as outfile:
            for item in tuples:
                line = ' '.join(urllib.quote(str(field)) for field in item)
                outfile.write(line + "\n")

<!--

XXX skip introductory Python paragraph?

-->

If you're not familiar with Python,
some of the contents of this function
may be a little puzzling.
**The `with` statement**
calls a method called `__exit__`
on the passed-in `context_manager`
when it finishes,
whether due to normal exit or due to an exception.
For file and `GzipFile` objects,
the `__exit__` method closes the file.
The `for` statement
iterates over a sequence (in this case, of tuples)
which could be **generated on the fly**
rather than precomputed before entering the function.
And the argument to `' '.join()`
is a **generator expression**
of the form `(x for y in z)`,
which generates a sequence (on the fly)
by evaluating the expression `x`
once for each item of `z`.

As it turns out,
in the current version of this search engine,
it isn't actually important that the tuples can be generated on the fly,
because the largest thing we're writing as a sequence of tuples
is the posting list,
and it has to be sorted before writing it to each chunk anyway.

`urllib.quote` encodes the strings being written to the file
to ensure they can't contain spaces.

Reading the tuples is even simpler,
using a generator function:

    def read_tuples(context_manager):
        with context_manager as infile:
            for line in infile:
                yield tuple(urllib.unquote(field) for field in line.split())

<!--

XXX more introductory Python material; omit?

-->

When the function is invoked,
it doesn't do anything but return a generator object;
each time the `.next()` method is invoked on that generator object,
the function starts running,
either from the beginning
or from the last place it suspended,
until it reaches a `yield` expression,
which suspends execution of the function
and causes `.next()` to return a value.

This kind of coroutine concurrency,
which is also available in Lua, Ruby, and Golang,
allows functions like `read_tuples()`
to be written straightforwardly as functions,
rather than as iterator classes with methods.
This search engine uses this technique extensively
to simplify its structure.

Index structure
---------------

This engine
stores its index in a directory,
with a structure like the following:

    .chispa
    .chispa/0
    .chispa/0/1.gz
    .chispa/0/2.gz
    .chispa/0/3.gz
    .chispa/0/skip
    .chispa/1
    .chispa/1/1.gz
    .chispa/1/2.gz
    .chispa/1/3.gz
    .chispa/1/skip

Each subdirectory of the top-level index directory
is a segment;
essentially an independent index.
The index results from different segments
must be combined
with the set union operation
to get the final index results.

Each segment is divided into sequential chunks,
which are gzipped,
and the skip file,
which tells which postings can be found in each chunk,
is called `skip`.

At some point,
my plan is to interpret the pathnames in the index
relative to the index's parent directory,
and to search up toward the root of the filesystem
to look for an index to consult.

Merging strategy for incremental updates
----------------------------------------

Some sets of files
never change,
and this engine in its current form
is perfectly suited to those,
because it has no way to update an index once it exists.
However,
its
index structure can handle them already,
since each segment is entirely independent of other segments;
you can just create a new segment
to contain the postings from the new or newly modified files,
and any subsequent search will then be able to find
things in those files.

(This requires some way to keep track of which files
and which versions of those files
are already indexed
and which are not yet indexed,
and we also need to eventually discard postings
that pertain to old versions of modified files.)

But if you create too many segments,
searches will become slow.
So at some point
you need to merge segments to keep your searches fast.
But, if you merge all the segments into one segment
every time you update your index,
your updates are no longer very incremental.
It's wasteful to copy an entire huge segment
just to add a few things to it.

It turns out there's a middle ground that works pretty well,
although I don't know if it has
reasonable mathematically guaranteed worst-case performance.
You find the largest index segment
that is no bigger
than all the segments
smaller than itself
put together;
and you merge it with all those smaller segments.
Intuitively, it's not too wasteful to merge it,
since it comprises no more than half of the resulting index.

For example, suppose you have existing segments
of sizes 100k, 250k, 750k, and 2500k:

    100 250 750 2500

and you create a new segment of 20k:

    20 100 250 750 2500

We can write down the total sizes of the smaller segments underneath:

      20  100  250  750 2500
       0   20  120  370 1120

All of the segments are bigger than all the smaller segments put together,
so you don't merge anything this time.
Now you create another segment of 30k:

      20   30  100  250  750 2500
       0   20   50  150  400 1150

Still nothing.
Now another segment of 50k:

      20   30   50  100  250  750 2500
       0   20   50  100  200  450 1200

Now the 100k segment is no bigger
than all the smaller segments put together,
so we combine all of them
in a four-way merge.
If we assume that the merged result
has exactly the sum of the sizes of its inputs,
which is close to true but not quite due to compression ratios,
this produces a 200k segment:

     200  250  750 2500
       0  200  450 1200

If we add another 20k segment, no merging happens:

      20  200  250  750 2500
       0   20  220  470 1220

But a second 20k segment gets merged with the first one:

      20   20  200  250  750 2500
       0   20   40  240  490 1240
      40  200  250  750 2500
       0   40  240  490 1240

Note that at this point,
due to the 200k and 250k segments
being so close in size,
another 20k segment is enough to push us over the edge
into another four-way merge:

      20   40  200  250  750 2500
       0   20   60  260  510 1260
     510  750 2500
       0  510 1260

In some sense,
because every surviving segment is constrained to be
larger than the sum of all the segments smaller than it is,
the segment sizes at any given moment
are never too far from an exponentially growing sequence,
which means that only a logarithmic number of segments can exist.

So, if that's true, then this "middle ground"
guarantees that your searches never get too slow;
but how do we know that it guarantees that you don't
waste too much time merging?

Since every merge input
is no more than half the size of the merge output,
every posting moves into a segment of at least double the size
every time it undergoes a merge.
That means that, if your total corpus has 16 billion postings,
each posting will be merged at most some 34 times;
and actually, since it's probably starting in a primary segment
with a million other postings,
it won't actually go through more than 14 merges.
That is, the total amount of merge work done with this policy
is O(N log N),
which is pretty good.

One problem is that the merge work isn't very evenly distributed.
Adding an arbitrarily small bit of index
can result in an arbitrarily large amount of work;
in my last example above, adding 20k
resulted in 510k of merging
that had been postponed previously.

Evaluating a query
------------------

If we just want to find
the doc_ids of
all the documents
that contain all the search terms,
given an index to search in,
this is simple:

    def pathnames(index_path, terms):
        return set.intersection(*(set(term_pathnames(index_path, term))
                                  for term in terms))

We want the document IDs
that are in the posting list
of every term;
that is, we want the intersection of all the posting lists.
For a search engine that deals with larger quantities of data,
we might want to be careful about our query planning,
using the most selective terms first
and probing the posting lists for the least selective ones
rather than running O(N) algorithms on them;
but for our purposes,
just doing an eager hash join like this is probably adequately fast.

To get the document IDs for a term,
we need to combine the postings
from all the segments:

    def term_pathnames(index_path, term):
        return itertools.chain.from_iterable(segment_term_pathnames(segment, term)
                                             for segment in index_path)

`chain.from_iterable` essentially concatenates
the sequences of document IDs
from the calls to `segment_term_pathnames`.

To iterate over the segments in index_path,
this is using
a `Path` object
whose `__iter__` method
returns a `Path` object
for each file or subdirectory
in the directory:

    class Path:                     # like java.lang.File
        def __init__(self, name):
            self.name = name
        __getitem__  = lambda self, child: Path(os.path.join(self.name, str(child)))
        __contains__ = lambda self, child: os.path.exists(self[child].name)
        __iter__     = lambda self: (self[child] for child in os.listdir(self.name))
        open         = lambda self, *args: open(self.name, *args)
        open_gzipped = lambda self, *args: gzip.GzipFile(self.name, *args)

Most manipulation of filesystem paths
in this engine
is done with this class.

Anyway, to find the document IDs
from a given segment
for a given term,
we read through each of the chunks
that might contain postings for the term:

    def segment_term_pathnames(segment, term):
        for chunk_name in segment_term_chunks(segment, term):
            for term_2, pathname in read_tuples(segment[chunk_name].open_gzipped()):
                if term_2 == term:
                    yield pathname
                if term_2 > term:
                    break

Here `[]` invokes `Path.__getitem__`,
and `open_gzipped` invokes decompression of the chunk.

But which chunks might contain postings?
We have to read the skip file to find out:

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

We don't know
the full range of the terms in a chunk
until we get to the skip file entry for the next chunk,
so we're always yielding the previous chunk.

The contents of `skip_file_entries` is quite simple:

    def skip_file_entries(indexdir):
        # XXX is sorted() guaranteed correct?
        return sorted(read_tuples(indexdir['skip'].open()))

This invokes the same `read_tuples` function
as `segment_term_pathnames`;
it merely converts a text file
with space-separated fields on each line
into an iterable of tuples:

    def read_tuples(context_manager):
        with context_manager as infile:
            for line in infile:
                yield tuple(line.split())

By using the `with` statement,
the file is guaranteed to be closed
when the generator is terminated,
either by garbage collection,
by finishing the iteration,
or by explicitly calling .close() on the generator.
If you don't close files,
sooner or later,
you'll run out of file descriptors,
and any future attempts to open files will fail with an error.
In CPython,
the reference-counting garbage collector
finalizes generators
as soon as they go out of scope,
but the other implementations of Python
(PyPy, Jython, and IronPython)
do not offer such guarantees.
While `skip_file_entries` is guaranteed to exhaust the generator
and thus close the file
by using `sorted()`,
`segment_term_pathnames` may exit the iteration early,
and so XXX currently leaks file descriptors in PyPy.

And that's all there is to evaluating a query:
the candidate postings for each query term,
as (term, doc_id) tuples,
are read by `read_tuples`
from the chunks supplied by `segment_term_chunks`;
they're filtered by `segment_term_pathnames`
to only the ones that really do pertain to the term;
the doc_ids from different segments are combined
with `term_pathnames`;
and finally,
the doc_ids from different terms
are combined
so that only doc_ids that contain every term remain.

Search engines on large corpuses,
or corpuses that have been maliciously poisoned by SEO consultants,
generate too many hits for even fairly specific queries
to be useful
without ranking the results
so that the results most likely to be interesting
are displayed first.
This typically uses document weights,
such as Google's PageRank or StackOverflow's scores;
and measures of relevance to the query.

The general recipe for relevance measures
is "TF/IDF":
"term frequency, inverse document frequency",
which is to say,
documents containing terms more frequently are rated higher,
while terms that occur more frequently in the corpus overall
are weighted more lightly.
It really should be called
"term frequency, inverse corpus frequency,"
but the term dates from the early years of information retrieval research,
when the terminology was different.
There are different formulas in the TF/IDF family,
which I would discuss here
if I knew anything about them.

Generating an index
-------------------

So that explains
how you would evaluate a query
given that you already have an index on disk
in the format described earlier;
but how do you generate the index?

First,
you have to generate the sorted sequence of postings
for each segment,
and then you have to write that sequence into the segment.
In outline, that is:

    def build_index(index_path, corpus_path):
        os.mkdir(index_path.name)

        postings = postings_from_dir(corpus_path)
        for ii, chunk in enumerate(blocked(postings, 2**20)):
            write_new_segment(index_path[ii], sorted(chunk))

        merge_segments(index_path, list(index_path))

The number `2**20` was chosen
to keep the memory usage of the indexer
around a quarter gigabyte on my test data,
because a quarter gigabyte is tolerable.
If you decrease it,
you'll use less memory
and generate more primary segments.

Note that the `os.mkdir` at the beginning
will keep you from accidentally building an index
in a place that already has something else in it,
because it will raise an exception if the directory already exists.

The `blocked` function
lazily breaks up a sequence of things
into a sequence of blocks of things
of up to the given size;
for example, `list(blocked("colon", 2))`
will return `[('c', 'o'), ('l', 'o'), ('n',)]`.
This set of postings is by far the largest thing present in memory
during the creation of the index,
and it can easily grow far larger than memory,
so we limit how many we keep in memory at a time.
Its code,
mostly taken from an answer on Stack Overflow,
is maybe a little too clever:

    def blocked(seq, block_size):
        seq = iter(seq)
        while True:
            yield tuple(itertools.islice(seq, block_size)) or next(seq)

<!--

More introductory Python.

-->

`itertools.islice` takes the first N items from an iterator,
but it can terminate before yielding N items
if the underlying iterator does.
In that case, you will end up with a short tuple.
If there are no items left,
you will end up with an empty tuple.
But it's not desirable for `blocked` to keep yielding empty tuples forever.
Instead, it should terminate its iteration
when there are no more items left.
A generator can terminate its iteration
either in the usual way that functions terminate,
by returning or running off the end of its code,
or by raising the `StopIteration` exception
that Python's iteration protocol uses
in place of a `hasNext` method.
The easiest way to raise `StopIteration`
is to invoke `next` on an empty iterator,
so that's what this code does.

You'll note that the explanation for this code
is about 130 words,
while the code is only 15 words.
That probably means it's bad code.
So far I've kept it in this form
instead of a more straightforward but longer form
mostly because I expect `itertools.islice`
will be faster than doing the same thing
in interpreted CPython.

`postings_from_dir`
is currently fairly crude;
it generates a sequence of (term, doc_id) tuples
given a directory
by treating every file in that directory
as a text file.
It's careful not to generate the same posting twice,
which both simplifies and speeds up the rest of the code.

    def postings_from_dir(path):
        for dir_name, _, filenames in os.walk(path.name):
            dir_path = Path(dir_name)
            for filename in filenames:
                with dir_path[filename].open() as fo:
                    seen_words = set()
                    for line in fo:
                        for word in re.findall('\w+', line):
                            if word not in seen_words:
                                yield word, dir_path[filename].name
                                seen_words.add(word)

So that's the sequence of postings
being broken into 1048576-item blocks by `blocked`
which are then sorted and passed to `write_new_segment`.
Note that the `seen_words` set could potentially get dangerously large
if this is run on a particularly large input file.
A little more code would suffice
to write the candidate postings out to a temporary file in such a case,
sort the file on disk to eliminate duplicates,
and then yield the postings.

The deep nesting in this function
is a warning sign of bad code.
I should probably factor this function into pieces.

So what does `write_new_segment` do?

    def write_new_segment(path, postings):
        os.mkdir(path.name)
        for ii, chunk in enumerate(blocked(postings, 4096)):
            write_tuples(path['%s.gz' % ii].open_gzipped('w'), chunk)
        build_skip_file(path)

This uses the `write_tuples` function explained earlier,
passing it a `gzip.GzipFile` to automatically close when it's done.
It might be more sensible to write the data in an uncompressed form,
then later launch a background `gzip` process to compress it,
since about half of the program's run time is taken up by compression.

The `build_skip_file` function is similarly simple;
its only tricky bit is that it lists the chunk names
before opening the skip file
so it won't mistake the skip file for a chunk:

    def build_skip_file(path):
        chunk_names = list(path)
        # XXX what about fsync?
        write_tuples(path['skip'].open('w'), generate_skip_entries(chunk_names))

But that's because the actual skip-file generation logic
is in `generate_skip_entries`:

    def generate_skip_entries(chunk_paths):
        for chunk_path in chunk_paths:
            chunk_tuples = read_tuples(chunk_path.open_gzipped())
            try:
                term, _ = chunk_tuples.next()
                yield term, os.path.basename(chunk_path.name)
            finally:
                chunk_tuples.close()

We simply read the first tuple from each chunk.
Since we aren't reading all the tuples,
the `read_tuples` generator won't implicitly exit automatically
and close the file.
As discussed previously,
to avoid a file descriptor leak in non-reference-counted Pythons,
we explicitly close the generator.

So that covers the whole paper path from input files
into a primary index segment.
Aside from the representational issues
like document IDs and term dictionaries,
which are essentially a matter of compression,
this presentation glosses over a couple of other issues
that more complete search engines handle:
our documents are not divided into fields,
which means that you can't, say,
search for a term in just the title,
or just the filename,
or (as in my email search engine `dumbfts` that this was modeled after)
just the from address;
the postings stored for each term
include only the document ID,
without any frequency or proximity information,
or even any stemming;
and the term extraction itself
is a simple regular expression match,
with no attention given to the type of the file.

Merging
-------

But `build_index`
also invoked `merge_segments(index_path, list(index_path))`.
How do we merge segments?

    def merge_segments(path, segments):
        if len(segments) == 1:
            return

        postings = heapq.merge(*(read_segment(segment)
                                 for segment in segments))
        ii = 0 # XXX factor out
        while ii in path:
            ii += 1
        write_new_segment(path[ii], postings)

        for segment in segments:
            shutil.rmtree(segment.name)

First, if we only have one primary segment,
there's no need to merge it;
we can just return.
But if we have more than one,
we open all of them and hook them up to a heap,
which then yields a stream of all of their items
in order.

Then we must choose an unused name for the new output segment,
and then we write the tuples to it
using the same `write_new_segment`
that we use to create primary segments.
Note that in this case
we are in fact taking advantage of the fact
that we can pass `postings` to `write_new_segment`
even though most of the postings it will yield
have not yet been read from disk.

Finally, we delete the now-obsolete input segments.

The only new function here
is `read_segment`:

    def read_segment(path):
        for _, chunk in skip_file_entries(path):
            # XXX refactor chunk reading?  We open_gzipped in three places now.
            for item in read_tuples(path[chunk].open_gzipped()):
                yield item

We use the skip file entries
to make sure we are reading the chunks in the correct order.
XXX I think this can be rewritten in terms of `itertools.chain`.

