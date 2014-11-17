
==========================================
 Contingent: A Fully Dynamic Build System
==========================================

Traditional build systems are hopelessly naïve.

Whether you use the venerable ``make``, or the document build process
inside of LaTeX, or even the modern rebuild process inside of the
popular Sphinx documentation tool for Python, you will have run across
situations where you made a change and the build system failed to
discover all of its ramifications.

The alternative always seems to be rebuilding too much — as when you
notice the failure and often have to remove the entirety of your cached
output to restore the build to a working state!

Document build systems are notorious in this respect.  Cross references
in the text create dynamic relationships between the content of one page
and the content of another — think of a ``:doc:`how-to-install`\ `` call
in Sphinx and how the document in which it appears needs to (but usually
will not) rebuild every time that document’s title changes.

Even ``make`` could be bitten by this problem. Consider adding the
following line to the top of a ``source.c`` source file::

  #include "memhelpers.h"

Thanks to the addition of this line, the source file now needs to be
re-compiled every time this header file is changed. But how often do
developers forget to open their Makefile and add a line like the
following? ::

  source.c: memhelpers.h

There do exist third-party tools to write such rules on-the-fly — to
literally rewrite the ``Makefile`` — and this makes the point perfectly:
the build tool, in and of itself, is not competent to discover which
changes to which inputs require the rebuilding of which consequences.

What if we constructed a build system that were not static, rigid, and
incapable of understanding the way that relationships between inputs and
consequences can grow or disappear from one minute to the next as the
inputs are edited?  What if the build system itself were instrumented to
discover these consequences dynamically instead of having to be told
them in a separate ``Makefile`` that has to be maintained separately?

We have undertaken this challenge and produced the Python module
described in this chapter: ``contingent``, a dynamic build graph that
learns automatically the inputs that each build routine needs and, when
an input changes, always does the least work necessary to get all of the
outputs back up to date again.

Creating and Using a Build Graph
--------------------------------

Imagine that we want to automate the rebuilding of a static web site
when any of its source files change.  Contingent offers to let us get
started by instantiating a ``Graph`` object that will remember which
outputs are consequences of which inputs.

>>> from contingent.graphlib import Graph
>>> g = Graph()

Imagine a directory of three source files containing the marked up text
for three blog posts.  When each is passed to the markup engine, it will
result in an HTML body that is web-ready.  We can represent this by
creating six nodes — which as far as the graph knows are arbitrary,
opaque strings — and three edges that link the markup file sitting on
disk for each blog post with the idea of its rendered HTML “body” that
is produced once the markup is parsed.

>>> g.add_edge('A.rst', 'A.body')
>>> g.add_edge('B.rst', 'B.body')
>>> g.add_edge('C.rst', 'C.body')

We are telling the graph that an input, like ``A.rst``, has as its
consequence the node ``A.body``.  If we ask the graph “What happens if
B.rst changes?” then, quite correctly, it tells us that the body will
need to be rewritten.

>>> g.recursive_consequences_of(['B.rst'])
['B.body']

Imagine each HTML body then being injected into a larger page template
so that the blog’s text is surrounded by the navigation and design that
are standard for our blog.  The HTML body in each case is the input to
this process, and its output is a finished page ready to be pushed to
the web server.

>>> g.add_edge('A.body', 'A.html')
>>> g.add_edge('B.body', 'B.html')
>>> g.add_edge('C.body', 'C.html')

The graph understands that consequences themselves serve as inputs for
further consequences.  If we now ask it the results of editing the input
markup for one of our blog posts, it identifies two outputs that need to
be recomputed.

>>> g.recursive_consequences_of(['B.rst'])
['B.body', 'B.html']

So far, so good.

But when dealing with document collections like a Sphinx project or
statically generated blog, the graph tends to be more complicated.  For
example, each blog post’s navigation might point the way to the previous
post in the blog’s history and therefore need its title Like the body,
the value of the title is discovered when we parse the post’s input
markup:

>>> g.add_edge('A.rst', 'A.title')
>>> g.add_edge('B.rst', 'B.title')
>>> g.add_edge('C.rst', 'C.title')

As part of the navigation, each of these titles can also appear on the
HTML page for the subsequent post:

>>> g.add_edge('A.title', 'B.html')
>>> g.add_edge('B.title', 'C.html')

>>> open('diagram1.dot', 'w').write(g.as_graphviz()) and None

Editing the source for either post A or post B will now force us not
only to regenerate both its own HTML, but also the HTML of the
subsequent post, in case we edited the text of its title which appears
in the navigation of the next post’s page:

>>> g.recursive_consequences_of(['B.rst'])
['B.body', 'B.html', 'B.title', 'C.html']

By rebuilding both ``B.html`` and ``C.html`` in this case, a naïve build
system that simply runs through these recursive consequences will
usually be doing extra work — after all, most edits to ``B.rst`` will
probably not be edits to the title, but simply be edits to the body.

But if we step back for a moment, we see that in fact things are far
worse.  How, when it comes down to it, do we know that these three blog
posts belong in the order A-B-C?

So far in this example, we have simply been assuming that I named the
blog posts in the correct alphabetical order.  But real blog posts are
going to need a date attribute which dictates their order — an
attribute, like all of their other attributes, that might change over
the course of composition or need to be corrected later.

This means that an edit to a blog post, because it could move the blog
post’s position in history, might require an update of *any* of the
other output files!  To take a concrete example: answering the question
“which blog post comes immediately before blog post ``B`` in time?”
means having to examine the date of every other blog post to determine
which one has the closest previous date.

What can a build system do in this situation?  Most systems either
rebuild too few outputs and deliver greater speed but at the risk of
leaving output out of date, or they rebuild everything and usually do so
unnecessarily.

The Sphinx build system, to take one example, seems to simply ignore the
possibility that a change to document *A* might involve rewriting the
text of a cross-reference in another document *B*, or might mean that
another document’s mention of a function or class can now become a real
linked cross-reference.  This only keeps up-to-date the HTML pages for
the documents currently being edited, and lets the others fall behind
until the user forces their regeneration.

To represent this problem with blog post dates in our own graph, we need
to break the edge we hand-crafted between adjacent blog posts:

>>> g.remove_edge('A.title', 'B.html')
>>> g.remove_edge('B.title', 'C.html')

Each blog post’s date, of course, will probably be stored as a piece of
metadata in its source file markup, and need to be included somewhere on
its own HTML page.

>>> for post in 'ABC':
...     g.add_edge(post + '.rst', post + '.date')
...     g.add_edge(post + '.date', post + '.html')

But we also need to let the date of each blog post decide which of its
peers is the previous post.  All of the posting dates need to be
considered together when making this determination.  This can only be
implemented in our graph by creating a node that is the output — that is
an aggregation — of a property (the date) of every single blog post:

>>> g.add_edge('A.date', 'sorted-posts')
>>> g.add_edge('B.date', 'sorted-posts')
>>> g.add_edge('C.date', 'sorted-posts')

This aggregate output — that knows all the blog-post dates — is then a
necessary input to the routine that figures out what the word
“previous,” and specifically the words “previous title,” mean when
applied to any given blog post:

>>> g.add_edge('sorted-posts', 'A.prev.title')
>>> g.add_edge('sorted-posts', 'B.prev.title')
>>> g.add_edge('sorted-posts', 'C.prev.title')

Each of these previous-title nodes are then available for when we build
the output HTML page for each blog post, and will get injected into
their navigation bar:

>>> g.add_edge('A.prev.title', 'A.html')
>>> g.add_edge('B.prev.title', 'B.html')
>>> g.add_edge('C.prev.title', 'C.html')

We can expect all of the above edges to remain static.  Whatever the
relationships among our blog posts, they will still each need to know
the title (if any) of the chronologically previous post.

But now we reach edges that will *not* necessarily remain in place over
the lifetimes of our input files!  This is because they depend on the
current chronological order A,B,C of the blog posts, that makes A the
previous post to B, and B the previous post to C.  This could change the
moment we edit one of their dates!  But for the moment the edges look
like:

>>> g.add_edge('A.title', 'B.prev.title')
>>> g.add_edge('B.title', 'C.prev.title')

And it is this set of edges that ruin our graph.  Because of the
possibility that an edit to any blog post’s source code might make a
change to its date — although in practice this will be only a small
fraction of the number of edits made during a busy writing session — a
traditional make system will have to rebuild every single blog post when
any single one of them is edited!

>>> consequences = g.recursive_consequences_of(['B.rst'])
>>> consequences
['B.body', 'B.date', 'sorted-posts', 'A.prev.title', 'A.html', 'B.prev.title', 'B.html', 'B.title', 'C.prev.title', 'C.html']

>>> open('diagram2.dot', 'w').write(g.as_graphviz(['B.rst'] + consequences)) and None

This simple example illustrates only one of many ways that a document’s
content winds up inside of other documents in a modern document tree.
The real-world cross referencing system in Spinx, for example, also
includes a document’s URL and title in every other document where it is
referenced, and any reorganization of a library’s API documentation
will change the URL of functions and classes that might be referred to
from dozens of other documents.

Given such a dense graph, can a build system do any better than to
simply perform a complete rebuild upon every modification?

>>> open('diagram3.dot', 'w').write(g.as_graphviz()) and None

Chasing consequences
--------------------

The key insight that helps us answer the foregoing question is to note
the difference between our intuitive understanding of the build
process—that most changes disrupt only a small subset of the full
consequences graph—and the consequences graph itself, which represents
a more course-grained fact: that a given task depends on a certain set
of inputs. The consequences graph tells us, for example, that ``B.title``
uses the output of task ``B.rst`` as its input:

>>> 'B.title' in g.immediate_consequences_of('B.rst')
True

but it does not understand what sorts of changes to ``B.rst`` actually
affect ``B.title``.  Accommodating this requires us to extend the build
system such that, when notified of a change, it can determine if the
change has an effect on the task's value and therefore requires a
rebuild of that task's consequences.

``Builder`` manages build processes by augmenting the graph with a value
cache that records the output of each task's build, allowing us to
compare its current value with its value from the previous run. If a
task's value changes, we must inform its consequent tasks in the event
the new value has an impact on those consequences. ``Builder`` maintains
a ``todo_list`` of tasks for this purpose: as tasks run, the value cache
tells the ``Builder`` if the task's output has changed, requiring that
task's consequences to be placed on the todo list for reconsideration.

To illustrate, we first construct a ``Builder``

>>> from contingent.projectlib import Project
>>> b = Project()

and update its initially empty consequences graph to be the manually-
constructed graph from our example above

>>> b.graph = g

For this example, we will drive the build process manually.
In the first run of the build, the cache is empty, so each task
requires a full rebuild:

>>> roots = ['A.rst', 'B.rst', 'C.rst']
>>> for node in roots + g.recursive_consequences_of(roots):
...     # 'Initial value' is the simulated output of the build task for
...     # each node
...     b.set(node, 'Initial value')

Since each task has been freshly computed, all the tasks are up to date
and the todo list is empty:

>>> b.todo_list
set()

Changing something forces us to rebuild its consequences, but focuses
our efforts only on the particular tasks that need rebuilding.  For
example, editing the body content of file B requires examination of all
consequences of B:

>>> b.set('B.rst', 'Updated body markup for post B')
>>> sorted(b.todo_list)
['B.body', 'B.date', 'B.title']

All of these consequent tasks need to be reevaluated, but in this
instance only ``B.body``\ 's value is affected by the change, leaving
``B.date`` and ``B.title`` at their prior values:

>>> b.set('B.body', 'New body for B')
>>> b.set('B.date', 'Initial value')
>>> b.set('B.title', 'Initial value')

Since it is only post B's output HTML that needs its body content, the
``Builder`` does not need to consider the consequences of tasks
``B.date`` and ``B.title``, so the todo list peters out rather quickly:

>>> sorted(b.todo_list)
['B.html']
>>> b.set('B.html', 'HTML for post B')
>>> b.todo_list
set()

Editing B's title, on the other hand, has consequences for the HTML of
both post B and post C.

>>> b.set('B.title', 'Title B')
>>> sorted(b.todo_list)
['C.prev.title']
>>> b.set('B.html', 'New HTML for post B')
>>> b.set('C.prev.title', 'Title B')
>>> b.todo_list
{'C.html'}
>>> b.set('C.html', 'HTML for post C')
>>> b.todo_list
set()

And, finally, in the presence of a change or edit that makes no
difference the cache does not demand that we rebuild any consequences at
all.

>>> b.set('B.title', 'Title B')
>>> b.todo_list
set()

But while this approach has started to reduce our work, a rebuild can
still involve extra steps.  Walking naively forward through consequences
like this can be inefficient, because we might rebuild a given
consequence several times.  Imagine, for example, that we update B’s
date so that it now comes after C on the timeline.

>>> b.set('B.rst', 'Markup for post B dating it after post C')
>>> sorted(b.todo_list)
['B.body', 'B.date', 'B.title']
>>> b.set('B.body', 'Initial value')
>>> b.set('B.date', '2014-05-15')
>>> b.set('B.title', 'Title B')
>>> sorted(b.todo_list)
['B.html', 'sorted-posts']
>>> b.set('B.html', 'Rebuilt HTML #1')
>>> b.set('sorted-posts', 'A, C, B')
>>> sorted(b.todo_list)
['A.prev.title', 'B.prev.title', 'C.prev.title']
>>> b.set('A.prev.title', 'Initial value')
>>> b.set('B.prev.title', 'Title C')
>>> b.set('C.prev.title', 'Title A')
>>> sorted(b.todo_list)
['B.html', 'C.html']
>>> b.set('B.html', 'Rebuilt HTML #2')
>>> b.set('C.html', 'Rebuilt HTML')

As you can see, this update to B’s date has both an immediate and
certain consequence — that its HTML needs to be rebuilt to reflect the
new date — and also a consequence that takes longer to play out: it now
comes after post C, so its “Previous Post” link now needs to display C’s
title instead of A’s title.

The reason that we wound up rebuilding B twice in the session above is
that we lacked the big picture of how our graph is connected.  There are
two routes of different lengths between ``B.date`` and the final
``B.html`` output, but we went ahead and rebuilt ``B.html`` as soon as
any of its inputs changed instead of waiting to see how all of the paths
played out.

The solution is that instead of letting ``todo()`` results drive us
forwards, we should try ordering the consequences of ``B.date`` using
what graph theorists call a *topological sort* that is careful to order
nodes so that consequences always fall after their inputs in the
resulting ordering.  If used correctly, a depth-first search can produce
such an ordering.

Topological sort is built into the graph method
``recursive_consequences_of()`` that we glanced at briefly above.  If we
use its ordering instead of simply rebuilding nodes as soon as they
appear in the ``todo()`` list, then we will minimize the number of
rebuilds we need to perform:

>>> consequences = g.recursive_consequences_of(['B.rst'])
>>> consequences
['B.body', 'B.date', 'sorted-posts', 'A.prev.title', 'A.html', 'B.prev.title', 'B.html', 'B.title', 'C.prev.title', 'C.html']

Had we followed this ordering, we would have regenerated both ``B.date``
and ``B.prev.title`` before reaching and finally rebuilding ``B.html``.
Our final algorithm will therefore use the topological sort to minimize
redundant work.

But we should note that, in the general case, that once we finish our
topologically sorted rebuild we will still have to pay attention to the
``todo()`` list and keep looping until it is empty.  That is because
nodes can actually change their input list each time they run, and that
therefore the pre-ordering we compute might not reflect the real state
of the graph as it evolves.

Why would the graph change as we are calculating it?

The edges we have considered so far between documents are the result of
static site design — here, the fact that each HTML page has a link to
the preceding blog post.  But sometimes edges arise from the content
itself!  Blog posts, for example, might refer to each other
dynamically::

    I have been learning even more about the Pandas library.
    You can read about my first steps in using it by visiting
    my original `learning-pandas`_ blog post from last year.

When this paragraph is rendered the output should look like:

    ...original `Learning About Pandas`_ blog post from last year.

Therefore this HTML will need to be regenerated every time the title in
``learning-pandas.rst`` is edited and changed.

After running a rebuild step for a consequence, therefore, we will need
to rebuild the edges leading to it so that they reflect exactly the
inputs it in fact used during its rebuild.  In the rare case that the
new set of edges includes one from a yet-to-be-rebuilt consequence
further along in the current topological sort, this will correctly
assure that the consequence then reappears in the ``todo()`` set.  A
full replacement of all incoming edges is offered through a dedicated
graph method.  If an update were added to the text of post A to mention
the later post C, then an edge would need to be generated to capture
that:

>>> g.add_edge('C.title', 'A.html')

Thanks to this new edge, post A will now be considered one of
consequences of a change to the title of post C.

>>> g.recursive_consequences_of(['C.title'])
['A.html']

How can this mechanism be connected to actual code that takes the
current value of each node and builds the resulting consequences?
Python gives us many possible approaches.  [Show various ways of
registering routines?]


----


A Functional Blog Builder
-------------------------

``example/`` demonstrates a functional blog builder constructed in a
Clean Architecture style: the build process is defined by functions that
accept and return simple data structures and are ignorant of the manager
processes surrounding them. These functions perform the typical
operations that allow the blog framework to produce the rendered blog
from its sources: reading and parsing the source texts, extracting
metadata from individual posts, determining the overall ordering of the
entire blog, and rendering to an output format.

>>> from example.blog_project import project
>>> from example.blog_project import read_text_file, parse, body_of  # etc.

In this implementation, each *task* is a function and argument list
tuple that captures both the function to be performed and the input
arguments unique to that task:

>>> task = read_text_file.wrapped, ('A.rst',)

This particular task depends upon the content of the file ``A.rst`` —
its ``path`` argument — and returns the contents of that file as its
output. Its consequences are any tasks that require the raw text of the
file as input, such as the task ``(parse, ('A.rst',))``.

[TODO: explain that the Project decorator wrapped each function so that
it can intercept calls.]

This indirection gives ``Builder`` the opportunity to perform its two
crucial functions: consequence discovery and task caching. As tasks run,
``Builder`` carefully tracks when each task requests outputs from other
tasks, dynamically building up its consequences graph as the build runs.
If at any point, a task requests an input ``Builder`` has recently
computed, the value is returned directly from the cache, effectively
halting the rebuild of tasks along that graph path.

We can manually force an initial value for our read task using
``Builder.set()``

>>> project.set(task, 'Text of A')

Since this is the first task this ``Builder`` has encountered, the task
has no consequences: nothing as of yet has requested its output,

>>> project.graph.immediate_consequences_of(task)
set()

and, since it is freshly computed, requests for the task's value can be
serviced directly from ``Builder``'s cache.

>>> project.start_tracing()
>>> read_text_file('A.rst')
'Text of A'
>>> print(project.end_tracing())
returning cached read_text_file('A.rst')

Requesting the value for a new task, ``(body_of, ('A.rst',))``,
illuminates the back and forth between the ``Builder`` and the
framework: a request is made to the ``Builder`` for A's value, but,
since it has never seen this task before, ``Builder`` immediately
returns a request to the framework's ``compute`` function for a hard
rebuild of the value. The function ``body_of``, when invoked, transfers
control back to the ``Builder`` by requesting the value of ``(parse,
('A.rst',))``, which is also missing and must be computed. Finally,
``parse`` requests the value from ``read_text_file``, which the
``Builder`` *does* have cached, thus ending the call chain.

>>> project.start_tracing()
>>> body_of('A.rst')
'<p>Text of A</p>\n'
>>> print(project.end_tracing())
calling body_of('A.rst')
calling parse('A.rst')
returning cached read_text_file('A.rst')

Interposing the Builder between function calls allows it to dynamically
construct the relationship between individual tasks

>>> project.graph.immediate_consequences_of(task)
{(<function parse at 0x...>, ('A.rst',))}

and the entire chain of consequences leading from that task.

>>> project.graph.recursive_consequences_of([task], include=True)
[(<function read_text_file at 0x...>, ('A.rst',)), (<function parse at 0x...>, ('A.rst',)), (<function body_of at 0x...>, ('A.rst',))]

If nothing changes, subsequent requests for ``(body_of, ('A.rst',))``
can be served immediately from the cache,

>>> project.start_tracing()
>>> body_of('A.rst')
'<p>Text of A</p>\n'
>>> print(project.end_tracing())
returning cached body_of('A.rst')

while the effects of changes that invalidate interior task's values are
minimized by the ``Builder``'s ability to detect the impact of a change
at every point on the consequences graph:

>>> project.invalidate((body_of.wrapped, ('A.rst',)))
>>> project.start_tracing()
>>> body_of('A.rst')
'<p>Text of A</p>\n'
>>> print(project.end_tracing())
calling body_of('A.rst')
returning cached parse('A.rst')

.. illustrate task stack?


.. this section is a bit rougher than the above tour:

Building Architecture
---------------------

Now that we have seen how the build system functions, we are ready to
step back and consider the structure of the whole system, its parts and
their responsibilities, various divisions of labor, and the coordination
between the components to effect a build. The build system we have
constructed consists of three main components: Graph, Builder, and the
framework components for the artifact we are building.

Graph maintains the directed task consequences graph. As we have seen
above, it exposes methods allowing its clients to create and remove
connections between tasks and to request the inputs and consequences of
any tasks is aware of. It also provides the important topological
sorting routine that allows Builder to order its work efficiently.

For the convenience of our examples, Graph also includes a routine to
produce a text representation of itself usable as input to the
``graphviz`` visualization utility. In a larger system, we would likely
separate the responsibility for representing the graph from the
responsibility for visually formatting graph: a formatter would *accept*
a Graph object as input and produce output in the desired format.

Builder coordinates and manages a build process. It maintains the
consequences graph, delegating the actual representation of the graph
itself to an associated Graph object. As it is working, Builder
maintains a to-do list of stale tasks as it discovers task values have
gone out of date. A simple ``dict`` serves as the repository for cached
task values.

Notice that neither Builder nor Graph know anything whatsoever about the
tasks they are managing! To these components, tasks are simply objects:
a Graph sees tasks as nodes with inputs and consequences, i.e. inbound
and outbound edges to other nodes. Builder adds the additional notion
that tasks are associated with values; it also requires tasks to
hashable types to support its caching optimization.

This class organization and division of labor provides flexible support
for any sort of tasks a build process might want — tasks could be
strings, ints, custom objects, or functions as we've shown here. More
importantly, this division of responsibilities keeps each of our classes
simple: each has one, clearly delineated job; the implementations are
focused and free of clutter. It is easy to reason about the parts in
isolation from each other, and one can even imagine replacing, say, the
Graph implementation with an off-the-shelf library.

.. and the chapter really needs a rousing conclusion
