
Build systems maintain a directed graph.

>>> from contingent.graphlib import Graph
>>> g = Graph()

A system like ``make`` rebuilds an output file when one of its source
files appears to have been modified more recently.

>>> g.add_edge('A.body', 'A.html')
>>> g.add_edge('B.body', 'B.html')
>>> g.add_edge('C.body', 'C.html')

This makes it easy, if ``B.rst`` is modified, to determine which output
file needs to be rebuilt.

>>> g.recursive_consequences_of(['B.body'])
['B.html']

Because a target might itself be the dependency for yet further targets,
``consequences_of`` needs to be a recursive operation that computes a
transitive closure over our dependency graph.  If the body of each blog
post is in fact imported from a Restructured Text file, for example,
then our graph will be two stories high:

>>> g.add_edge('A.rst', 'A.body')
>>> g.add_edge('B.rst', 'B.body')
>>> g.add_edge('C.rst', 'C.body')
>>> g.recursive_consequences_of(['B.rst'])
['B.body', 'B.html']

But when dealing with document collections like a Sphinx project or
statically generated blog, the dependency graph tends to be more
complicated.  For example, each blog post’s navigation might point the
way to the previous post in the blog’s history.  In real life, the
navigation would probably need at least the previous post’s title, URL,
and date.  But to keep our example simple, we will only consider that
the title of each blog post will appear on the page of the next one.
Like body, the title will need to appear on each post’s own page:

>>> for post in 'ABC':
...     g.add_edge(post + '.rst', post + '.title')
...     g.add_edge(post + '.title', post + '.html')

But as part of the navigation, each title will also appear on the HTML
page for the subsequent post:

>>> g.add_edge('A.title', 'B.html')
>>> g.add_edge('B.title', 'C.html')

>>> open('diagram1.dot', 'w').write(g.as_graphviz()) and None

Editing the source for either post A or post B will now force us to
regenerate both its own HTML as well as the HTML of the subsequent post,
to make sure that an edited title is correctly reflected in the next
post’s navigation:

>>> g.recursive_consequences_of(['B.rst'])
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

>>> consequences = g.recursive_consequences_of(['B.rst'])
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

>>> from contingent.builderlib import Builder
>>> b = Builder(callback=None)

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

Since it is only post B's output HTML that depends on its body content,
the ``Builder`` does not need to consider the consequences of tasks
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
['B.html', 'C.prev.title']
>>> b.set('B.html', 'New HTML for post B')
>>> b.set('C.prev.title', 'Title B')
>>> b.todo_list
{'C.html'}
>>> b.set('C.html', 'HTML for post C')
>>> b.todo_list
set()

And, finally, in the presence of a change or edit that makes no
difference the cache does not demand that we rebuild any targets at all.

>>> b.set('B.title', 'Title B')
>>> b.todo_list
set()

But while this approach has started to reduce our work, a rebuild can
still involve extra steps.  Walking naively forward through consequences
like this can be inefficient, because we might rebuild a given target
several times.  Imagine, for example, that we update B’s date so that it
now comes after C on the timeline.

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
any of its dependencies changed instead of waiting to see how all of the
paths played out.

The solution is that instead of letting ``todo()`` results drive us
forwards, we should try ordering the consequences of ``B.date`` using
what graph theorists call a *topological sort* that is careful to order
nodes so that targets always fall after their dependencies in the
resulting ordering.  If used correctly, a depth-first search can produce
such an ordering.

Topological sort is built into the graph method
``recursive_consequences_of()`` that we glanced at briefly above.  If we
use its ordering instead of simply rebuilding nodes as soon as they
appear in the ``todo()`` list, then we will minimize the number of
rebuilds we need to perform:

>>> consequences = g.recursive_consequences_of(['B.rst'])
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
later post C, then its dependencies would need to include that:

>>> g.add_edge('C.title', 'A.html')

Thanks to this new list of dependencies, post A will now be considered
one of consequences of a change to the title of post C.

>>> g.recursive_consequences_of(['C.title'])
['A.html', 'C.html']

How can this mechanism be connected to actual code that takes the
current values of dependencies and builds the resulting targets?  Python
gives us many possible approaches.  [Show various ways of registering
routines?]


----


A Functional Blog Builder
-------------------------

``example1/`` demonstrates a functional blog builder constructed in a
Clean Architecture style: the build process is defined by functions that
accept and return simple data structures and are ignorant of the manager
processes surrounding them. These functions perform the typical
operations that allow the blog framework to produce the rendered blog
from its sources: reading and parsing the source texts, extract metadata
from individual posts, determining the overall ordering of the entire
blog, and rendering to an output format.

>>> from example1.build import read_text_file, parse, body_of  # etc.

In this implementation, each *task* is a function and argument list
tuple that captures both the function to be performed and the input
arguments unique to that task:

>>> task = read_text_file, ('A.rst',)

This particular task depends upon the content of the file ``A.rst`` —
its ``path`` argument — and returns the contents of that file as its
output. Its consequences are any tasks that require the raw text of the
file as input, such as the task ``(parse, ('A.rst',))``.

How do these functions interact with a ``Builder``-managed process?
Rather than calling each other directly, each function accepts a
``call`` argument, a callable that allows the ``Builder`` to insert
itself between a task and any inputs it depends on.

>>> def call(task_fn, *args):
...     print('· call(', task_fn.__name__, ', ', args, ')', sep='')
...     # Get a task's value from the blog Builder, instantiated below
...     return blog.get((task_fn, args))

The task functions use ``call`` to request values from other tasks, as
when ``parse`` requests the raw content of a file at a given path:

.. code-block:: python

    def parse(call, path):
        "Parse the file at path; return a dict of the body, title, and date."

        source = call(read_text_file, path)
        # …

This indirection gives ``Builder`` the opportunity to perform its two
crucial functions: consequence discovery and task caching. As tasks run,
``Builder`` carefully tracks when each task requests outputs from other
tasks, dynamically building up its consequences graph as the build runs.
If at any point, a task requests an input ``Builder`` has recently
computed, the value is returned directly from the cache, effectively
halting the rebuild of tasks along that graph path.

If a task's current value isn't available, ``Builder`` needs a mechanism
to recompute it. To keep the ``Builder`` generic and flexible, it
accepts a compute callable that mediates this return trip to the build
framework:

>>> def compute(task, _):
...     task_fn, args = task
...     print('· compute(', task_fn.__name__, ', ', args, ')', sep='')
...     return task_fn(call, *args)

Together, ``call`` and ``compute`` form the framework/``Builder``
interface: ``call`` allows the framework to pass control to its
``Builder``; ``compute`` gives ``Builder`` the means to rebuild stale
tasks by calling back to the framework.

To illustrate, we can construct a new ``Builder`` initialized with an
empty graph and this ``compute`` callback:

>>> blog = Builder(compute)

We can manually force an initial value for our read task using
``Builder.set()``

>>> blog.set(task, 'Text of A')

Since this is the first task this ``Builder`` has encountered, the task
has no consequences: nothing as of yet has requested its output,

>>> blog.graph.immediate_consequences_of(task)
set()

and, since it is freshly computed, requests for the task's value can be
serviced directly from ``Builder``'s cache.

>>> call(read_text_file, 'A.rst')
· call(read_text_file, ('A.rst',))
'Text of A'

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

>>> call(body_of, 'A.rst')
· call(body_of, ('A.rst',))
· compute(body_of, ('A.rst',))
· call(parse, ('A.rst',))
· compute(parse, ('A.rst',))
· call(read_text_file, ('A.rst',))
'<p>Text of A</p>\n'

Interposing the Builder between function calls allows it to dynamically
construct the relationship between individual tasks

>>> blog.graph.immediate_consequences_of(task)
{(<function parse at 0x...>, ('A.rst',))}

and the entire chain of consequences leading from that task.

>>> blog.graph.recursive_consequences_of([task], include=True)
[(<function read_text_file at 0x...>, ('A.rst',)), (<function parse at 0x...>, ('A.rst',)), (<function body_of at 0x...>, ('A.rst',))]

If nothing changes, subsequent requests for ``(body_of, ('A.rst',))``
can be served immediately from the cache,

>>> call(body_of, 'A.rst')
· call(body_of, ('A.rst',))
'<p>Text of A</p>\n'

while the effects of changes that invalidate interior task's values are
minimized by the ``Builder``'s ability to detect the impact of a change
at every point on the consequences graph:

>>> blog.invalidate((body_of, ('A.rst',)))
>>> call(body_of, 'A.rst')
· call(body_of, ('A.rst',))
· compute(body_of, ('A.rst',))
· call(parse, ('A.rst',))
'<p>Text of A</p>\n'


.. illustrate task stack?


----


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

>> from contingent.interceptlib import Base
>> class Post(Base):
..     def __init__(self):
..         self.content = ''


[TODO: blurb about file dates and ``touch`` and how it lets you force a
rebuild even if ``make`` cannot see that some contingency has changed]

[TODO: sprinkle some cache.__getitem__() operations through the text]
