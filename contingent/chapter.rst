
Build systems maintain a directed graph.

>>> from graphlib import Graph
>>> g = Graph()

A system like ``make`` rebuilds an output file when one of its source
files appears to have been modified more recently.

>>> g.add_edge('A.body', 'A.html')
>>> g.add_edge('B.body', 'B.html')
>>> g.add_edge('C.body', 'C.html')

This makes it easy, if ``B.md`` is modified, to determine which output
file needs to be rebuilt.

>>> g.consequences_of(['B.body'])
['B.html']

Because a target might itself be the dependency for yet further targets,
``consequences_of`` needs to be a recursive operation that computes a
transitive closure over our dependency graph.  If the body of each blog
post is in fact imported from a Restructured Text file, for example,
then our graph will be two stories high:

>>> g.add_edge('A.rst', 'A.body')
>>> g.add_edge('B.rst', 'B.body')
>>> g.add_edge('C.rst', 'C.body')
>>> g.consequences_of(['B.rst'])
['B.body', 'B.html']

But when dealing with document collections like a Sphinx project or
statically generated blog, the dependency graph tends to be more
complicated.  For example, each blog post’s navigation might point the
way to the previous post in the blog’s history.  In real life, the
navigation would probably need at least the previous post’s title, URL,
and date.  But to keep our example simple, we will only consider that
the title of each blog post will appear on the page of the next one.
Like body, the title will need to appear on each post’s own page:

>>> g.add_edge('A.rst', 'A.title')
>>> g.add_edge('B.rst', 'B.title')
>>> g.add_edge('C.rst', 'C.title')
>>> g.add_edge('A.title', 'A.html')
>>> g.add_edge('B.title', 'B.html')
>>> g.add_edge('C.title', 'C.html')

But as part of the navigation, each title will also appear on the HTML
page for the subsequent post:

>>> g.add_edge('A.title', 'B.html')
>>> g.add_edge('B.title', 'C.html')

>>> open('diagram1.dot', 'w').write(g.as_graphviz()) and None

Editing the source for either post A or post B will now force us to
regenerate both its own HTML as well as the HTML of the subsequent post,
to make sure that an edited title is correctly reflected in the next
post’s navigation:

>>> g.consequences_of(['B.rst'])
['B.body', 'B.title', 'B.html', 'C.html']

Note that by rebuilding both ``B.html`` and ``C.html`` in this case, a
make-inspired system will usually be doing extra work — after all, most
edits to ``B.rst`` will probably be edits to the body and not changes to
the title itself.  But ``C.html`` will be rebuilt with every edit.

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
rebuild too few dependencies and deliver greater speed but at the risk
of leaving output out of date, or they rebuild everything and usually do
so unnecessarily.

The Sphinx build system, to take one example, seems to simply ignore the
possibility that a change to document *A* might involve rewriting the
text of a cross-reference in another document *B*, or might mean that
another document’s mention of a function or class can now become a real
linked cross-reference.  This only keeps up-to-date the HTML pages for
the documents currently being edited, and lets the others fall behind
until the user forces their regeneration.

To represent this problem with blog post dates in our own graph, we need
to break the dependency we hand-crafted between adjacent blog posts:

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
implemented in our graph by creating a node that depends upon a property
of every single blog post:

>>> for post in 'ABC':
...     g.add_edge(post + '.date', 'sorted-posts')
...     g.add_edge('sorted-posts', post + '.prev.title')
...     g.add_edge(post + '.prev.title', post + '.html')

>>> g.add_edge('A.title', 'B.prev.title')
>>> g.add_edge('B.title', 'C.prev.title')

And it is this set of edges that ruin our dependency graph.  Because of
the possibility that an edit to any blog post’s source code might make a
change to its date — although in practice this will be only a small
fraction of the number of edits made during a busy writing session — a
traditional make system will have to rebuild every single blog post when
any single one of them is edited!

>>> consequences = g.consequences_of(['B.rst'])
>>> consequences
['B.body', 'B.date', 'sorted-posts', 'A.prev.title', 'A.html', 'B.prev.title', 'B.title', 'B.html', 'C.prev.title', 'C.html']

>>> open('diagram2.dot', 'w').write(g.as_graphviz(['B.rst'] + consequences)) and None

This simple example illustrates only one of many ways that a document’s
content winds up inside of other documents in a modern document tree.
The real-world cross referencing system in Spinx, for example, also
includes a document’s URL and title in every other document where it is
referenced, and any reorganization of a library’s API documentation
will change the URL of functions and classes that might be referred to
from dozens of other documents.

Given such a dense dependency graph, can a build system do any better
than to simply perform a complete rebuild upon every modification?

Chasing consequences
--------------------

>>> open('diagram3.dot', 'w').write(g.as_graphviz()) and None

>>> from cachelib import Cache
>>> c = Cache(g)
>>> roots = ['A.rst', 'B.rst', 'C.rst']
>>> for node in roots + g.consequences_of(roots):
...     c[node] = 'Initial value'

>>> c.todo()
set()

Changing something forces us to rebuild its consequences, but focuses
our efforts only on the particular targets that need rebuilding.  For
example, editing file B but only updating the body peters out rather
quickly.

>>> c['B.rst'] = 'Markup for post B'
>>> sorted(c.todo())
['B.body', 'B.date', 'B.title']
>>> c['B.body'] = 'New body for B'
>>> c['B.date'] = 'Initial value'
>>> c['B.title'] = 'Initial value'
>>> sorted(c.todo())
['B.html']
>>> c['B.body'] = 'HTML for post B'

Editing its title, on the other hand, has consequences for the HTML of
both post B and post C.

>>> c['B.title'] = 'Title B'
>>> sorted(c.todo())
['B.html', 'C.prev.title']
>>> c['B.html'] = 'New HTML for post B'
>>> c['C.prev.title'] = 'Title B'
>>> c.todo()
{'C.html'}
>>> c['C.html'] = 'HTML for post C'
>>> c.todo()
set()

And, finally, in the presence of a change or edit that makes no
difference the cache does not demand that we rebuild any targets at all.

>>> c['B.title'] = 'Title B'
>>> c.todo()
set()

But while this approach has started to reduce our work, a rebuild can
still involve extra steps.  Walking naively forward through consequences
like this can be inefficient, because we might rebuild a given target
several times.  Imagine, for example, that we update B’s date so that it
now comes after C on the timeline.

>>> c['B.rst'] = 'Markup for post B dating it after post C'
>>> sorted(c.todo())
['B.body', 'B.date', 'B.title']
>>> c['B.body'] = 'Initial value'
>>> c['B.date'] = '2014-05-15'
>>> c['B.title'] = 'Title B'
>>> sorted(c.todo())
['B.html', 'sorted-posts']
>>> c['B.html'] = 'Rebuilt HTML #1'
>>> c['sorted-posts'] = 'A, C, B'
>>> sorted(c.todo())
['A.prev.title', 'B.prev.title', 'C.prev.title']
>>> c['A.prev.title'] = 'Initial value'
>>> c['B.prev.title'] = 'Title C'
>>> c['C.prev.title'] = 'Title A'
>>> sorted(c.todo())
['B.html', 'C.html']
>>> c['B.html'] = 'Rebuilt HTML #2'
>>> c['C.html'] = 'Rebuilt HTML'

As you can see, this update to B’s date has both an immediate and
certain consequence — that its HTML needs to be rebuilt to reflect the
new date — and also a consequence that takes longer to play out: it now
comes after post C, so its “Previous Post” link now needs to display C’s
title instead of A’s title.

The reason that we wound up rebuilding B twice in the session above is
that we lacked the big picture of how our graph is connected.  There are
two routes of different lengths between ``B.date`` and the final
``B.html`` output, but we went ahead and rebuilt ``B.html`` as soon as
any of its dependencies changed instead of waiting to see how all of the
paths played out.

The solution is that instead of letting ``todo()`` results drive us
forwards, we should try ordering the consequences of ``B.date`` using
what graph theorists call a *topological sort* that is careful to order
nodes so that targets always fall after their dependencies in the
resulting ordering.  If used correctly, a depth-first search can produce
such an ordering.

Topological sort is built into the graph method ``consequences_of()``
that we glanced at briefly above.  If we use its ordering instead of
simply rebuilding nodes as soon as they appear in the ``todo()`` list,
then we will minimize the number of rebuilds we need to perform:

>>> consequences = g.consequences_of(['B.rst'])
>>> consequences
['B.body', 'B.date', 'sorted-posts', 'A.prev.title', 'A.html', 'B.prev.title', 'B.title', 'B.html', 'C.prev.title', 'C.html']

Had we followed this ordering, we would have regenerated both ``B.date``
and ``B.prev.title`` before reaching and finally rebuilding ``B.html``.
Our final algorithm will therefore use the topological sort to minimize
redundant work.

But we should note that, in the general case, that once we finish our
topologically sorted rebuild we will still have to pay attention to the
``todo()`` list and keep looping until it is empty.  That is because
nodes can actually change their dependency list each time they run, and
that therefore the pre-ordering we compute might not reflect the real
state of the dependency graph as it evolves.

Why would the graph change as we are calculating it?

The dependencies we have considered so far between documents are the
result of static site design — here, the fact that each HTML page has a
link to the preceding blog post.  But sometimes dependencies arise from
the content itself!  Blog posts, for example, might refer to each other
dynamically::

    I have been learning even more about the Pandas library.
    You can read about my first steps in using it by visiting
    my original `learning-pandas`_ blog post from last year.

When this paragraph is rendered the output should look like:

    ...original `Learning About Pandas`_ blog post from last year.

Therefore this HTML will need to be regenerated every time the title in
``learning-pandas.rst`` is edited and changed.

After running a rebuild step for a target, therefore, we will need to
reset the incoming edges from its dependencies.  In the rare case that
the new set of edges includes one from a yet-to-be-rebuilt target
further along in the current topological sort, this will correctly
assure that the target then reappears in the ``todo()`` set.  A full
replacement of all incoming edges is offered through a dedicated graph
method.  If an update were added to the text of post A to mention the
later post C, then its dependencies would need to change:

>>> g.set_dependencies_of('A.html', ['A.body', 'A.date', 'A.title',
...                                  'A.prev.title', 'C.title'])

Thanks to this new list of dependencies, post A will now be considered
one of consequences of a change to the title of post C.

>>> g.consequences_of(['C.title'])
['A.html', 'C.html']

How can this mechanism be connected to actual code that takes the
current values of dependencies and builds the resulting targets?  Python
gives us many possible approaches.  [Show various ways of registering
routines?]

But the easiest way might be to suit up objects and watch attribute
access and method invocation.  Python again offers several possible
approaches.

* We could wrap every method and property that we want the caching
  system to intercept using a custom decorator.

* Our logic could be backed by a custom metaclass that would then
  automatically wrap a custom decorator around each method and property.

* We could intercept attribute access on the classes themselves to
  intercept both plain-value attribute access and method calls.

I not only prefer simplicity, but I actually need a solution to have as
few moving parts as possible if I am to have any chance of maintaining
it later.  So I am going to opt for the last of these options.  By
having our objects inherit from a base class with a single method, we
will turn them into full participants in an object graph.

>> from interceptlib import Base
>> class Post(Base):
..     def __init__(self):
..         self.content = ''


[TODO: blurb about file dates and ``touch`` and how it lets you force a
rebuild even if ``make`` cannot see that some contingency has changed]

[TODO: sprinkle some cache.__getitem__() operations through the text]
