
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

And it is this set of edges that ruin our dependency graph.  Because of
the possibility that an edit to any blog post’s source code might make a
change to its date — although in practice this will be only a small
fraction of the number of edits made during a busy writing session — a
traditional make system will have to rebuild every single blog post when
any single one of them is edited!

>>> consequences = g.consequences_of(['B.rst'])
>>> consequences
['B.body', 'B.date', 'sorted-posts', 'A.prev.title', 'A.html', 'B.prev.title', 'C.prev.title', 'C.html', 'B.title', 'B.html']

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


