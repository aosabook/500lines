
==========================================
 Contingent: A Fully Dynamic Build System
==========================================

Systems to rebuild formatted documents from source texts
always seem to do too much work, or too little.
They do too much work
when they respond to a minor edit
by making you wait for unrelated chapters
to be re-parsed and re-formatted.
But they can also rebuild too little,
leaving you with an inconsistent final product.

Consider Sphinx 1.2.3, the current version
of the document builder
used for both the Python language official documentation
as well as for a number of other projects in the Python community.
Your project’s ``index.rst`` will usually include a table of contents::

   Table of Contents
   =================

   .. toctree::

      install.rst
      tutorial.rst
      api.rst

This list of chapter filenames
tells Sphinx to include a link to each chapter
when it builds the ``index.html`` output file.
It will also include links to any sections within each chapter.
Stripped of its markup, the result might look like::

  Table of Contents

  • Installation

  • Newcomers Tutorial
      • Hello, World
      • Adding Logging

  • API Reference
      • Handy Functions
      • Obscure Classes

This table of contents is a mash-up
of information from four different files.
While its basic order and structure come from ``index.rst``,
the actual title of each chapter and section
is pulled from your chapter source files themselves.

If you later reconsider the tutorial’s chapter title —
after all, the word “newcomer” sounds so antique,
as if your users are settlers who have just arrived in pioneer Wyoming —
then you would edit the first line of ``tutorial.rst``
and write something better::

  -Newcomers Tutorial
  +Beginners Tutorial
   ==================

   Welcome to the tutorial!
   This text will take you through the basics of...

When you are ready to rebuild,
Sphinx will do exactly the right thing!
It will rebuild both the tutorial chapter itself,
and also rebuild the index.
(Piping the output into ``cat`` makes Sphinx
display each rebuilt file on its own line,
instead of overwriting a single line with its progress updates.)
::

   $ make html | cat
   ⋮
   writing output... [ 50%] index
   writing output... [100%] tutorial

Sphinx chose to rebuild both documents.
Not only will the top of ``tutorial.html`` now feature its new title,
but the output ``index.html`` will display the updated title
in the table of contents.
Sphinx has rebuilt everything so that the output is consistent.

What if your edit to ``tutorial.rst`` is more minor? ::

   Beginners Tutorial
   ==================

  -Welcome to the tutorial!
  +Welcome to our tutorial!
   This text will take you through the basics of...

In this case there is no need to rebuild ``index.html``
because this minor edit to the interior of a paragraph
does not change any of the information in the table of contents.
But it turns out that Sphinx is not quite as clever
as it might have at first appeared.
Just in case the chapter title or a section title has changed,
it goes ahead and does the redundant work of rebuilding
``index.html`` even though it will come out exactly the same. ::

   writing output... [ 50%] index
   writing output... [100%] tutorial

You can run ``diff``
on the “before” and “after” versions of ``index.html``
to confirm that your small edit
has had zero effect on the project front page —
yet Sphinx made you wait while it was rebuilt anyway.

You might not even notice the extra rebuild effort
for small documents that are easy to compile.
But the delay to your workflow can become significant
when you are making frequent tweaks and edits
to documents that are long, complex, or that involve the generation
of multimedia like plots or animations.
While Sphinx is at least making an effort here
by not rebuilding every chapter in your project —
it has not, for example, rebuilt ``install.html`` or ``api.html``
in response to your ``tutorial.rst`` edit —
it is doing more than is necessary.

But it turns out that Sphinx does something even worse:
it sometimes does too little.

To see one of Sphinx’s simplest failure modes,
add a cross reference to the top of your API documentation::

   API Reference
   =============

  +Before reading this, try reading our :doc:`tutorial`!

   The sections below list every function
   and every single class and method offered...

With its usual caution as regards the table of contents,
Sphinx will rebuild both this API reference document
as well as the ``index.html`` home page of your project::

   writing output... [ 50%] api
   writing output... [100%] index

In the ``api.html`` output file you can confirm
that Sphinx has include the attractive human-readable title
of the tutorial chapter into the cross reference’s anchor tag::

   <p>Before reading this, try reading our
   <a class="reference internal" href="tutorial.html">
     <em>Beginners Tutorial</em>
   </a>!</p>

What if you now make another edit
to the title at the top of the ``tutorial.rst`` file?
You will have invalidated three output files.
The change needs to be reflected
at the top of ``tutorial.html`` itself,
in the table of contents in ``index.rst``,
and in this embedded cross reference
in the first paragraph of ``api.html``.
What does Sphinx do? ::

   writing output... [ 50%] index
   writing output... [100%] tutorial

Sphinx has failed to correctly rebuild your documentation.
If you now push your HTML to the web,
users will see one title in the cross reference
at the top of ``api.html``
but then a different title
once the link has carried them to ``tutorial.html`` itself.
This happens many kinds of cross reference
that Sphinx supports:
chapter titles, section titles, paragraphs,
classes, methods, and functions.

Experienced Sphinx users have a time-honored solution
to the cross-reference problem.
The solution has been honed and practiced for decades,
and in various forms it goes all the way back
to the original habits of users of the Document Workbench
with which Unix was originally marketed. ::

   $ rm -r _build
   $ make html

This certainly solves the problem
of guaranteeing consistency before publishing your documentation.
Everything gets rebuilt from scratch before going to the publisher!

But could we construct a better approach?

What if your build system were a persistent process
that remembered every title, every section, and every cross reference
that passed from the source code of one document
to the text of another?
Its decisions about whether to rebuild other documents
after a change to a single source file could be precise,
instead of mere guesses,
and correct,
instead of leaving the output in an inconsistent state.

The result would be a system like the old static ``make`` tool,
but which learned the dependencies between files as they were built —
that added and removed dependencies dynamically
as cross references were added, updated, and then later deleted.

In the sections that follows we will construct such a tool in Python,
named Contingent,
that guarantees correctness in the presence of dynamic dependencies
while performing the fewest possible rebuild steps.
While Contingent can be applied to any problem domain,
we will run it against a small version of the problem outlined above.

Linking Tasks To Make a Graph
=============================

The Contingent system needs a way to remember
that the output of a task like
“get the title of the API chapter”
is then needed as an input of tasks like
“build the ``index.html`` output file.”
And this relationship between tasks might be transitive.
The task “get the title of the API chapter” that we just mentioned
might itself need another task to run as its input,
like “parse the ``api.rst`` source text file.”

When you represent input tasks and their consequences on paper,
you probably use arrows to connect them,
as shown in Figure 1.

.. image:: figure1.png

Mathematicians call this kind of diagram a *graph,*
which is an unfortunate name — to most people,
*graph* means a rectangular plot that uses a jagged line
to display the unemployment rate or the stock market.
But computer programming was born as a discipline of mathematics,
and so Contingent also uses the term *graph*
for a collection of boxes and arrows —
or, as mathematicians say, *nodes* and *edges* —
like those in Figure 1.

At the core of Contingent is a ``Graph`` library [Brandon runs out of steam]

OKAY, DAN!

Time to go to town!  Everything from here down is (a) great code that I
have just edited, with (b) rough notes for you to work from.  Turn on
your CS prof superpowers and explain everything clearly, concisely, and
whimsically.

Also, edit any of my stuff above that you'd like.  And I can edit you
when you have turned the notes that follow into text, and after our
mutual edits hopefully it will read fairly clearly.

I have adjusted the code below so that it shows what Graph() does more
simply than before, so we can work on explaining the implementation.
Put words around these code samples, talking about the following points
(based on Debo's email to us) (these do not need to be long parts each,
by the way, you just need to make each point clearly, along with other
points that I'm sure will occur to you as you quote sections of
graphlib.py and discuss them (try to include them with RST file-line
inclusion, not by cutting and pasting):

* A graph needs to store edges.
* You might need to do lookup either way: what edges arrive here?  What
  edges go away from this node?  And for each question, outline a
  situation in which Contingent will need to answer a question.
* Talk about how we solve problems in Python: not with a Node class and an
  Edge class and an OutgoingEdgesCollection class, but with simple
  generic data structures.
* Do four bullet points with a sentence each: tuple, list, dict, set
* Discover that we should use a dict-of-sets pointing in each direction.
* Show __init__() and talk about defaultdict
* Then add_edge() then remove_edge()
* Contingent will sometimes need to rebuild, in case ``api.rst``
  is edited to add or remove a cross reference: clear_inputs_of()
* (leave the rest out for now?)
* Finally, step back: our API only asks that nodes be represented
  with hashable types. Discuss that tuples are fine too, not just
  strings like we are using in this section.
* Another step back: note that our API hides the data structures
  COMPLETELY! We could change to a stupid Node class and Edge class
  any time we wanted, because only task IDs pass our API border.

There's probably more great points we can make about Python and our
wonderful API, but those are the ones that come to mind right now that I
wanted to get down before I forgot them. :)



>>> from contingent import graphlib
>>> g = graphlib.Graph()

.. The code in this comment makes sets print in stable order:
 >>> from pprint import pformat, pprint
 >>> def key_of(thing):
 ...     if isinstance(thing, tuple):
 ...         return (thing[0].__name__, thing[1])
 ...     return thing
 >>> class pretty_set(set):
 ...     def __repr__(self):
 ...         if not self:
 ...             return 'set()'
 ...         items = list(self)
 ...         joiner = ',\n ' if isinstance(items[0], tuple) else ', '
 ...         items = sorted(self, key=key_of)
 ...         return '{' + joiner.join(repr(item) for item in items) + '}'
 ...     def __or__(self, other):
 ...         return pretty_set(set(self) | other)
 >>> graphlib.set = pretty_set
 >>> g = graphlib.Graph()




>>> g.add_edge('index.rst', 'index.html')
>>> g.add_edge('tutorial.rst', 'tutorial.html')
>>> g.add_edge('api.rst', 'api.html')


..
 >>> open('figure1.dot', 'w').write(g.as_graphviz()) and None



>>> g.add_edge('tutorial.rst', 'tutorial-title')
>>> g.add_edge('api.rst', 'api-title')

>>> g.add_edge('tutorial-title', 'index.html')
>>> g.add_edge('api-title', 'index.html')


..
 >>> open('figure2.dot', 'w').write(g.as_graphviz()) and None


FINALLY -

is it time here to describe and justify the consequences methods?
Maybe?


>>> g.immediate_consequences_of('index.rst')
{'index.html'}

That is simple.  But this is a several-step cascade,
we have to follow to the bottom:

>>> sorted(g.immediate_consequences_of('api.rst'))
['api-title', 'api.html']
>>> g.immediate_consequences_of('api-title')
{'index.html'}
>>> g.immediate_consequences_of('index.html')
set()
>>> g.immediate_consequences_of('api.html')
set()

Whenever things change we want to do that, but to be careful of the
order.  [Ugh - should we even explain?  Maybe just mention for the
advanced people: to avoid rerunning a task several times, we need a
topological sort.]

>>> g.recursive_consequences_of(['api.rst'])
['api-title', 'index.html', 'api.html']

Wow look it did what we did manually above!  It's great!

Learning Connections
====================

Okay: if we keep our edges up to date,
we will never again have the problem of rebuilding too little.

But how can the edges be kept up to date?

We can use wrappers plus a stack.

Yay!  Fundamental computer science like Debo wanted,
with a great chance to show how easily these are implemented in Python.

We need some fake files.
For illustration we will do something simpler than full Sphinx/rst.

>>> index = """
... Table of Contents
... -----------------
... * `tutorial.txt`
... * `api.txt`
... """

>>> tutorial = """
... Beginners Tutorial
... ------------------
... Welcome to the tutorial!
... We hope you enjoy it.
... """

>>> api = """
... API Reference
... -------------
... You might want to read
... the `tutorial.txt` first.
... """

So we have this decorator, which adds a wrapper.

>>> from contingent.projectlib import Project
>>> project = Project()
>>> task = project.task

>>> @task
... def read(filename):
...     return {'index.txt': index,
...             'tutorial.txt': tutorial,
...             'api.txt': api}[filename]

>>> import re
>>> @task
... def parse(filename):
...     text = read(filename).strip('\n')
...     title, body = text.split('\n', 1)
...     return title, body

>>> @task
... def title_of(filename):
...     title, body = parse(filename)
...     return title

>>> @task
... def render(filename):
...     title, body = parse(filename)
...     body = re.sub(r'`([^`]+)`',
...         lambda match: title_of(match.group(1)),
...         body)
...     return title + '\n' + body


The project graph knows nothing to begin with.

>>> project.graph.all_tasks()
set()

But if we ask it to build:

>>> for filename in 'index.txt', 'tutorial.txt', 'api.txt':
...     print(render(filename))
...     print('=' * 30)
Table of Contents
-----------------
* Beginners Tutorial
* API Reference
==============================
Beginners Tutorial
------------------
Welcome to the tutorial!
We hope you enjoy it.
==============================
API Reference
-------------
You might want to read
the Beginners Tutorial first.
==============================

Now what does the graph know about?

>>> project.graph.all_tasks()
{(<function parse at 0x...>, ('api.txt',)),
 (<function parse at 0x...>, ('index.txt',)),
 (<function parse at 0x...>, ('tutorial.txt',)),
 (<function read at 0x...>, ('api.txt',)),
 (<function read at 0x...>, ('index.txt',)),
 (<function read at 0x...>, ('tutorial.txt',)),
 (<function render at 0x...>, ('api.txt',)),
 (<function render at 0x...>, ('index.txt',)),
 (<function render at 0x...>, ('tutorial.txt',)),
 (<function title_of at 0x...>, ('api.txt',)),
 (<function title_of at 0x...>, ('tutorial.txt',))}

..
 >>> open('figure3.dot', 'w').write(project.graph.as_graphviz()) and None


Caching consequences
====================

So, building-too-little is solved.

But what about building too much?

Given Figure 2, how can we avoid rebuilding index.html
if api.rst is edited but the document title is not what changed?

To solve this, we need 


The key insight that helps us answer the foregoing question is to note
the difference between our intuitive understanding of the build
process—that most changes disrupt only a small subset of the full
consequences graph—and the consequences graph itself, which represents
a more course-grained fact: that a given task depends on a certain set
of inputs. The consequences graph tells us, for example, that ``B.title``
uses the output of task ``B.rst`` as its input:

x>>> 'B.title' in g.immediate_consequences_of('B.rst')
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

x>>> from contingent.projectlib import Project
x>>> b = Project()

and update its initially empty consequences graph to be the manually-
constructed graph from our example above

x>>> b.graph = g

For this example, we will drive the build process manually.
In the first run of the build, the cache is empty, so each task
requires a full rebuild:

x>>> roots = ['A.rst', 'B.rst', 'C.rst']
x>>> for node in roots + g.recursive_consequences_of(roots):
x...     # 'Initial value' is the simulated output of the build task for
x...     # each node
x...     b.set(node, 'Initial value')

Since each task has been freshly computed, all the tasks are up to date
and the todo list is empty:

x>>> b.todo_list
set()

Changing something forces us to rebuild its consequences, but focuses
our efforts only on the particular tasks that need rebuilding.  For
example, editing the body content of file B requires examination of all
consequences of B:

x>>> b.set('B.rst', 'Updated body markup for post B')
x>>> sorted(b.todo_list)
['B.body', 'B.date', 'B.title']

All of these consequent tasks need to be reevaluated, but in this
instance only ``B.body``\ 's value is affected by the change, leaving
``B.date`` and ``B.title`` at their prior values:

x>>> b.set('B.body', 'New body for B')
x>>> b.set('B.date', 'Initial value')
x>>> b.set('B.title', 'Initial value')

Since it is only post B's output HTML that needs its body content, the
``Builder`` does not need to consider the consequences of tasks
``B.date`` and ``B.title``, so the todo list peters out rather quickly:

x>>> sorted(b.todo_list)
['B.html']
x>>> b.set('B.html', 'HTML for post B')
x>>> b.todo_list
set()

Editing B's title, on the other hand, has consequences for the HTML of
both post B and post C.

x>>> b.set('B.title', 'Title B')
x>>> sorted(b.todo_list)
['C.prev.title']
x>>> b.set('B.html', 'New HTML for post B')
x>>> b.set('C.prev.title', 'Title B')
x>>> b.todo_list
{'C.html'}
x>>> b.set('C.html', 'HTML for post C')
x>>> b.todo_list
set()

And, finally, in the presence of a change or edit that makes no
difference the cache does not demand that we rebuild any consequences at
all.

x>>> b.set('B.title', 'Title B')
x>>> b.todo_list
set()

But while this approach has started to reduce our work, a rebuild can
still involve extra steps.  Walking naively forward through consequences
like this can be inefficient, because we might rebuild a given
consequence several times.  Imagine, for example, that we update B’s
date so that it now comes after C on the timeline.

x>>> b.set('B.rst', 'Markup for post B dating it after post C')
x>>> sorted(b.todo_list)
['B.body', 'B.date', 'B.title']
x>>> b.set('B.body', 'Initial value')
x>>> b.set('B.date', '2014-05-15')
x>>> b.set('B.title', 'Title B')
x>>> sorted(b.todo_list)
['B.html', 'sorted-posts']
x>>> b.set('B.html', 'Rebuilt HTML #1')
x>>> b.set('sorted-posts', 'A, C, B')
x>>> sorted(b.todo_list)
['A.prev.title', 'B.prev.title', 'C.prev.title']
x>>> b.set('A.prev.title', 'Initial value')
x>>> b.set('B.prev.title', 'Title C')
x>>> b.set('C.prev.title', 'Title A')
x>>> sorted(b.todo_list)
['B.html', 'C.html']
x>>> b.set('B.html', 'Rebuilt HTML #2')
x>>> b.set('C.html', 'Rebuilt HTML')

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

x>>> consequences = g.recursive_consequences_of(['B.rst'])
x>>> consequences
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

    x...original `Learning About Pandas`_ blog post from last year.

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

x>>> g.add_edge('C.title', 'A.html')

Thanks to this new edge, post A will now be considered one of
consequences of a change to the title of post C.

x>>> g.recursive_consequences_of(['C.title'])
['A.html']

How can this mechanism be connected to actual code that takes the
current value of each node and builds the resulting consequences?
Python gives us many possible approaches.  [Show various ways of
registering routines?]


----


A Functional Blog Builder
=========================

``example/`` demonstrates a functional blog builder constructed in a
Clean Architecture style: the build process is defined by functions that
accept and return simple data structures and are ignorant of the manager
processes surrounding them. These functions perform the typical
operations that allow the blog framework to produce the rendered blog
from its sources: reading and parsing the source texts, extracting
metadata from individual posts, determining the overall ordering of the
entire blog, and rendering to an output format.

x>>> from example.blog_project import project
x>>> from example.blog_project import read_text_file, parse, body_of  # etc.

In this implementation, each *task* is a function and argument list
tuple that captures both the function to be performed and the input
arguments unique to that task:

x>>> task = read_text_file.wrapped, ('A.rst',)

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

x>>> project.set(task, 'Text of A')

Since this is the first task this ``Builder`` has encountered, the task
has no consequences: nothing as of yet has requested its output,

x>>> project.graph.immediate_consequences_of(task)
set()

and, since it is freshly computed, requests for the task's value can be
serviced directly from ``Builder``'s cache.

x>>> project.start_tracing()
x>>> read_text_file('A.rst')
'Text of A'
x>>> print(project.end_tracing())
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

x>>> project.start_tracing()
x>>> body_of('A.rst')
'<p>Text of A</p>\n'
x>>> print(project.end_tracing())
calling body_of('A.rst')
. calling parse('A.rst')
. . returning cached read_text_file('A.rst')

Interposing the Builder between function calls allows it to dynamically
construct the relationship between individual tasks

x>>> project.graph.immediate_consequences_of(task)
{(<function parse at 0xx...>, ('A.rst',))}

and the entire chain of consequences leading from that task.

x>>> project.graph.recursive_consequences_of([task], include=True)
[(<function read_text_file at 0xx...>, ('A.rst',)), (<function parse at 0xx...>, ('A.rst',)), (<function body_of at 0xx...>, ('A.rst',))]

If nothing changes, subsequent requests for ``(body_of, ('A.rst',))``
can be served immediately from the cache,

x>>> project.start_tracing()
x>>> body_of('A.rst')
'<p>Text of A</p>\n'
x>>> print(project.end_tracing())
returning cached body_of('A.rst')

while the effects of changes that invalidate interior task's values are
minimized by the ``Builder``'s ability to detect the impact of a change
at every point on the consequences graph:

x>>> project.invalidate((body_of.wrapped, ('A.rst',)))
x>>> project.start_tracing()
x>>> body_of('A.rst')
'<p>Text of A</p>\n'
x>>> print(project.end_tracing())
calling body_of('A.rst')
. returning cached parse('A.rst')

.. illustrate task stack?

x>>> read_text_file(['mutable', 'list'])
Traceback (most recent call last):
  x...
ValueError: arguments to project tasks must be immutable and hashable, not the unhashable type: 'list'

.. this section is a bit rougher than the above tour:

Building Architecture
=====================

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

.. _Sphinx: http://sphinx-doc.org/
