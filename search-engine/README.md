# A tiny full-text search engine

In the last quarter-century,
full-text search engines
have gone from being specialized research tools,
mostly used by lawyers and journalists,
to our most common means of navigating the web.
There are a couple of different basic data structures
for search engines,
but by far the most common one
is the posting-list search engine,
which stores a dictionary
from words to lists of places where those words are found,
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
It's tuned to perform acceptably
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

Since this search engine doesn't do ranking,
it basically comes down to maintaining a posting list
on disk
and querying it.
We just need to be able to quickly find all the files
that contain a given word.
The simplest data structure
that supports this task
would be something like a sorted text file
with a word and a filename on each line;
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
to find all the lines that begin with a given word;
if you have a billion lines in the file,
this might take as many as 60 probes into the file.
On an electromechanical hard disk,
this could take more than half a second,
and it will have to be repeated for each search term.

A somewhat simpler,
although less optimal,
approach
is to break the file up into chunks
with a compact chunk index
which tells you what each chunk contains.
Then you can read only the chunks
that might contain the terms you need.

Industrial-strength search engines
avoid duplicating the vocabulary list
and the list of document filenames
(or URLs, or other identifiers)
by identifying each item with an integer.
This allows for delta compression.
Instead, we simply rely on gzip,
which typically makes our index
about 15% <!-- XXX check this -->
of the size of the original text.
