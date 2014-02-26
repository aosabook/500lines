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
like `grep -r` but much faster.
It’s tuned to perform acceptably
even on electromechanical hard disks
coated with spinning rust.

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

Maybe the thing to do is to make it a generic text-search program like
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
which is about 20K gzipped (5 bytes per posting),
or about 150K uncompressed,
representing about 150K of original text,
and can be decompressed and parsed
on my netbook
in about 30ms.
The index of chunks,
which is not compressed,
is about 9 bytes per chunk
on my sample data.
For it to reach 9 megabytes,
you would need to have a million chunks,
or about 150 gigabytes of original source data.
Reading a 9-megabyte file is a bearable startup cost,
since it should take perhaps 200ms,
though far from ideal.

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
which typically ends up being about 3MB compressed. <!-- XXX check this -->

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

