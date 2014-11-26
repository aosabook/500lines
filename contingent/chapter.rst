
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
This can happen for many kinds of cross reference that Sphinx supports:
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
['index.html']

That is simple.  But this is a several-step cascade,
we have to follow to the bottom:

>>> sorted(g.immediate_consequences_of('api.rst'))
['api-title', 'api.html']
>>> g.immediate_consequences_of('api-title')
['index.html']
>>> g.immediate_consequences_of('index.html')
[]
>>> g.immediate_consequences_of('api.html')
[]

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
Explain how the stack we bulid and tear down
reflects exactly the way the real stack is growing and shrinking.

Time to illustrate.

We need some fake files.
For illustration we will do something simpler than full Sphinx-and-rst
but instead build our own little syntax for a cross reference.

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

And we decorate four functions, that do all of the work.

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
...         lambda match: repr(title_of(match.group(1))),
...         body)
...     return title + '\n' + body


The project graph knows nothing to begin with.

>>> project.graph.all_tasks()
[]

But if we ask it to build:

>>> for filename in 'index.txt', 'tutorial.txt', 'api.txt':
...     print(render(filename))
...     print('=' * 30)
Table of Contents
-----------------
* 'Beginners Tutorial'
* 'API Reference'
==============================
Beginners Tutorial
------------------
Welcome to the tutorial!
We hope you enjoy it.
==============================
API Reference
-------------
You might want to read
the 'Beginners Tutorial' first.
==============================

It worked!
The titles got substited.

Now what does the graph know about?

Look!  All the tasks!

>>> from pprint import pprint
>>> pprint(project.graph.all_tasks())
[(<function parse at 0x...>, ('api.txt',)),
 (<function parse at 0x...>, ('index.txt',)),
 (<function parse at 0x...>, ('tutorial.txt',)),
 (<function read at 0x...>, ('api.txt',)),
 (<function read at 0x...>, ('index.txt',)),
 (<function read at 0x...>, ('tutorial.txt',)),
 (<function render at 0x...>, ('api.txt',)),
 (<function render at 0x...>, ('index.txt',)),
 (<function render at 0x...>, ('tutorial.txt',)),
 (<function title_of at 0x...>, ('api.txt',)),
 (<function title_of at 0x...>, ('tutorial.txt',))]

..
 >>> open('figure3.dot', 'w').write(project.graph.as_graphviz()) and None

So as you can see by Figure 3, it has things figured out.
By watching one function invoke another
it has automatically learned the graph of inputs and consequences.
Yay.

So it can auto-learn depenencies.
And knows all the things to rebuild.

But can it avoid rebuilding them?
Look at all the things that MIGHT need to be rebuilt
if the tutorial source text is touched.

>>> task = read, ('tutorial.txt',)
>>> pprint(project.graph.recursive_consequences_of([task]))
[(<function parse at 0x...>, ('tutorial.txt',)),
 (<function render at 0x...>, ('tutorial.txt',)),
 (<function title_of at 0x...>, ('tutorial.txt',)),
 (<function render at 0x...>, ('api.txt',)),
 (<function render at 0x...>, ('index.txt',))]

But what if the title did not change?
As you can see in Figure 3,
that should not need to touch the other documents.

What can we do?

Caching consequences
====================

We want to avoid rebuilding everything
if tutorial.rst is touched but title is not changed.

So: cache!

That is why we _get_from_cache()

So show rest of stuff from the listing?

Show how awesome Python is:
again, because functions are both 1st class objects
and are also hashable, we can use them as part of keys:
(f, args) is a completely natural key.

So what if title and body both change?

>>> task = read, ('tutorial.txt',)
>>> project.set(task, """
... The Coder Tutorial
... ------------------
... This is a new and improved
... introductory paragraph.
... """)

No surprise: everything gets rebuilt,
because every document needs to change.

>>> project.start_tracing()
>>> project.rebuild()
>>> print(project.stop_tracing())
calling parse('tutorial.txt')
calling render('tutorial.txt')
calling title_of('tutorial.txt')
calling render('api.txt')
calling render('index.txt')

But what if we edit it again,
but this time leave the title the same?

>>> project.set(task, """
... The Coder Tutorial
... ------------------
... Welcome to the coder tutorial!
... It should be read top to bottom.
... """)

This should have no effect on the other documents.

>>> project.start_tracing()
>>> project.rebuild()
>>> print(project.stop_tracing())
calling parse('tutorial.txt')
calling render('tutorial.txt')
calling title_of('tutorial.txt')

Success!
Only one document got rebuilt.

The fact that the newly computed return value
of ``title_of('tutorial.txt')`` is exactly equal to the old one
stopped the build process from having to recompute
any of the consequences downstream of it.

Subtlety
========

<!-- HMM. WAIT. By this point we are kind of set.
The story is nice and seems done.
Do we really need a secion on how the graph can
change even as you traverse it?
I am not sure. Maybe it should be axed.
What do you think, Dan?
Am leaving it hear til you give an opinion. -->


In fact:

----------------- ignore everything -----------------
--------------- from here down -------------
--------------- unless dan chooses to resurrect it ------------
----------- otherwise, we axe it in favor of a triumpant conclusion! --------




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



.. Links
.. _Sphinx: http://sphinx-doc.org/
