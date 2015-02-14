.. raw:: html

   <style>
     body {font-size: 1.3em; margin: 1em;}
     div.document {margin: 0em auto; max-width: 40em;}
     pre {background-color: #eee; padding: 0.6em;}
   </style>

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
that is used for both the official Python language documentation
and many other projects in the Python community.
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
Stripped of its markup,
the text that results from the above ``toctree`` command might be::

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
is pulled from the three chapter source files themselves.

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
announce each rebuilt file on a separate line,
instead of using bare carriage returns
to repeatedly overwrite a single line with these progress updates.)
::

   $ make html | cat
   ⋮
   writing output... [ 50%] index
   writing output... [100%] tutorial

Sphinx chose to rebuild both documents.
Not only will ``tutorial.html`` now feature its new title up at the top,
but the output ``index.html`` will display the updated chapter title
in the table of contents.
Sphinx has rebuilt everything so that the output is consistent.

What if your edit to ``tutorial.rst`` is more minor? ::

   Beginners Tutorial
   ==================

  -Welcome to the tutorial!
  +Welcome to our project tutorial!
   This text will take you through the basics of...

In this case there is no need to rebuild ``index.html``
because this minor edit to the interior of a paragraph
does not change any of the information in the table of contents.
But it turns out that Sphinx is not quite as clever
as it might have at first appeared!
It goes ahead and does the redundant work of rebuilding
``index.html`` even though the resulting contents
will be exactly the same. ::

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
While Sphinx is at least making an effort
to not rebuild every chapter when you make a single change —
it has not, for example, rebuilt ``install.html`` or ``api.html``
in response to your ``tutorial.rst`` edit —
it is doing more than is necessary.

But it turns out that Sphinx does something even worse:
it sometimes does too little.

To see one of Sphinx’s simplest failure modes,
first add a cross reference to the top of your API documentation::

   API Reference
   =============

  +Before reading this, try reading our :doc:`tutorial`!
  +
   The sections below list every function
   and every single class and method offered...

With its usual caution as regards the table of contents,
Sphinx will dutifully rebuild both this API reference document
as well as the ``index.html`` home page of your project::

   writing output... [ 50%] api
   writing output... [100%] index

In the ``api.html`` output file you can confirm
that Sphinx has included the attractive human-readable title
of the tutorial chapter into the cross reference’s anchor tag::

   <p>Before reading this, try reading our
   <a class="reference internal" href="tutorial.html">
     <em>Beginners Tutorial</em>
   </a>!</p>

What if you now make another edit
to the title at the top of the ``tutorial.rst`` file?
You will have invalidated *three* output files:

1. The title at the top of ``tutorial.html`` is now out of date,
   so the file needs to be rebuilt.

2. The table of contents in ``index.html`` still has the old title,
   so that document needs to be rebuilt.

3. The embedded cross reference in the first paragraph of ``api.html``
   still has the old chapter title,
   and also needs to be rebuilt.

What does Sphinx do? ::

   writing output... [ 50%] index
   writing output... [100%] tutorial

Whoops.

Only two files were rebuilt, not three.
Sphinx has failed to correctly rebuild your documentation.

If you now push your HTML to the web,
users will see the old title in the cross reference
at the top of ``api.html``
but then a different title — the new one —
once the link has carried them to ``tutorial.html`` itself.
This can happen for many kinds of cross reference that Sphinx supports:
chapter titles, section titles, paragraphs,
classes, methods, and functions.

Build Systems and Consistency
=============================

The problem outlined above is not specific to Sphinx.
Not only does it haunt other document systems, like LaTeX,
but it can even plague projects
that are simply trying to direct compilation steps
with the venerable ``make`` utility,
if their assets happen to cross-reference in interesting ways.

As the problem is ancient and universal,
its solution is of equally long lineage::

   $ rm -r _build/
   $ make html

You remove all of the output.

By eliminating every copy of every intermediate or output asset,
the build is forced to start over again completely
with nothing cached — with no memory of its earlier states
that could possibly lead to a stale product.
A hefty ``rm`` ``-r`` solves the problem
of guaranteeing consistency before publishing your Sphinx documentation!

But could we develop a better approach?

What if your build system were a persistent process
that noticed every chapter title, every section title,
and every cross referenced phrase
as it passed from the source code of one document
into the text of another?
Its decisions about whether to rebuild other documents
after a change to a single source file could be precise,
instead of mere guesses,
and correct,
instead of leaving the output in an inconsistent state.

The result would be a system like the old static ``make`` tool,
but which learned the dependencies between files as they were built —
that added and removed dependencies dynamically
as cross references were added, updated, and then later deleted.

In the sections that follow we will construct such a tool in Python,
named Contingent,
that guarantees correctness in the presence of dynamic dependencies
while performing the fewest possible rebuild steps.
While Contingent can be applied to any problem domain,
we will run it against a small version of the problem outlined above.

Linking Tasks To Make a Graph
=============================

Any build system needs a way to link inputs and outputs.
The three markup texts in our discussion above,
for example,
each produce a corresponding HTML output file.
The most natural way to express these relationships
is as a collection of boxes and arrows —
or, in mathematician terminology, *nodes* and *edges*
to form a *graph*:

.. figure:: figure1.png

   **Figure 1.** Three files generated by parsing three input texts.

.. * Finally, step back: our API only asks that nodes be represented
..   with hashable types. Discuss that tuples are fine too, not just
..   strings like we are using in this section.

How could we represent such a graph in Python?

The language gives priority to four generic data structures
by giving them direct support in the language syntax.
You can create new instances of the big-four data structures
by simply typing them into your source code.

.. TODO: Only two of these four bullets describe the data structure's
.. syntax. I suggest that we remove the description of the syntax since
.. we immediately *demonstrate* it with examples. But if we want to keep
.. the descriptions, we should probably be consistent and describe the
.. syntax for all four.

* The **tuple** is a read-only sequence
  whose syntax is parentheses around comma-separated items.
  Tuples are used to hold heterogeneous data —
  each slot in a tuple typically means something different.
  Here, a tuple holds together a hostname and port number,
  and would lose its meaning if the elements were re-ordered::

    ('dropbox.com', 443)

* The **list** is a mutable sequence
  whose syntax is square brackets around comma-separated items.
  Lists are used to hold homogenous data —
  each item usually has the same structure and meaning as its peers.
  Lists can be used either to preserve data’s original input order,
  or can be rearranged or sorted
  to establish a new and more useful order. ::

    ['JavaScript', 'Java', 'PHP', 'Python', 'C#']

* The **set** does not preserve order.
  Instead, it compares its items by value.
  The set remembers only whether a given value has been added,
  not how many times,
  and is therefore the go-to data structure
  for removing duplicates from a data stream.
  For example, the following two sets, once the language has built them,
  will each have three elements::

    {3, 4, 5}
    {3, 4, 5, 4, 4, 3, 5, 4, 5, 3, 4, 5}

* The **dict** lets the programmer chose the key
  by which each value is indexed,
  instead of using automatic integer indexing like the tuple and list.
  The lookup is backed by a hash table,
  which means that dict key lookup runs at the same speed
  whether the dict has a dozen or a million keys! ::

    {'ssh': 22, 'telnet': 23, 'domain': 53, 'http': 80}

A key to Python’s flexibility
is that these four data structures are composable.
The programmer can arbitrarily nest them inside each other
to produce more complex data stores
whose rules and syntax remain the simple ones
of the underlying tuples, lists, sets, and dicts.

Given that each of our graph edges needs
to know at least its origin node and its destination node,
the simplest possible representation would be a tuple.
The top edge in Figure 1 might look like::

    ('tutorial.rst', 'tutorial.html')

How can we store several edges?
While our initial impulse might be
to simply throw all of our edge tuples into a list,
that would have disadvantages.
A list is careful to maintain order,
but it is not meaningful to talk about the order of separate edges.
And a list would be perfectly happy to hold several copies
of exactly the same edge,
even though we only want it to be possible
to draw a single arrow between ``tutorial.rst`` and ``tutorial.html``.
The correct choice is thus the set,
which would have us represent Figure 1 as::

    {('tutorial.rst', 'tutorial.html'),
     ('index.rst', 'index.html'),
     ('api.rst', 'api.html')}

This would allow quick iteration across all of our edges,
fast insert and delete operations for a single edge,
and a quick way to check whether a particular edge was present.

.. TODO: Perhaps demonstrate these operations here?

Unfortunately, those are not the only operations we need.

A build system like Contingent
is going to focus its processing on nodes, not edges.
When building a particular asset like ``api.html``
it will need quick access to all of the incoming edges,
so that it can adjust them as it learns which resources
the build routine uses this time around,
and will also need access to the outgoing edges
so that any downstream tasks can be re-executed
if the content of ``api.html`` is changed.
Our set-of-tuples does not make this easy.
The entire set would need to be traversed
looking for edges that start or end at the ``api.html`` node.

Python’s only associative data structure is the dict.
We could use a dict to group edges for quick lookup by node::

    {'tutorial.rst': {('tutorial.rst', 'tutorial.html')},
     'tutorial.html': {('tutorial.rst', 'tutorial.html')},
     'index.rst': {('index.rst', 'index.html')},
     'index.html': {('index.rst', 'index.html')},
     'api.rst': {('api.rst', 'api.html')},
     'api.html': {('api.rst', 'api.html')}}

Looking up the edges of a particular node would now be blazingly fast,
at the cost of having to store every edge twice:
once in a set of incoming edges,
and once in a set of outgoing edges.
But the edges in each set would have to be examined manually
to see which are incoming and which are outgoing.
It is also slightly redundant to keep naming the node
over and over again in its set of edges.

The solution to both of these objections
is to place incoming and outgoing edges
in their own separate data structures,
which will also absolve us
of having to mention the node over and over again
for every one of the edges in which it is involved. ::

    incoming = {
        'tutorial.html': {'tutorial.rst'},
        'index.html': {'index.rst'},
        'api.html': {'api.rst'},
        }

    outgoing = {
        'tutorial.rst': {'tutorial.html'},
        'index.rst': {'index.html'},
        'api.rst': {'api.html'},
        }

Every edge in this dictionary-of-sets data structure
does get represented twice,
once as an outgoing edge from one node
and again as an incoming edge to another node.
But in return for this redundancy,
it supports the fast lookup that Contingent needs.

The Proper Use of Classes
=========================

.. TODO: rework this and following

You may have been surprised
by the absence of classes in the above discussion
of Python data structures.
After all, they are a frequent mechanism for organizing data
and get defined through a dedicated Python syntax.

But it turns out that classes are often orthogonal
to the question of data structure design.
Classes simply repeat data structures that we have already seen:

* A class instance is *implemented* as a dict.
* A class instance is *used* like a mutable tuple.

The class offers key lookup into its attribute dictionary
through a prettier syntax,
where you get to say ``graph.incoming``
instead of ``graph["incoming"]``.
But, in practice, class instances are almost never used
as generic key-value stores.
Instead, they are used to organize heterogeneous data
by attribute name instead of by an index into a tuple.

So instead of putting a hostname and a port number together in a tuple
and having to remember later which came first and which came second,
you create an ``Address`` class
whose instances each have a ``host`` and a ``port`` attribute,
pass an ``Address`` around
where otherwise you would have had anonymous tuples.
Code becomes easier to read and easier to write.
But using a class instance does not really change
any of the questions we faced above when doing data design:
it just provides a prettier and less anonymous container.

The true value of classes, then,
is not that they change the science of data design.
The value of classes
is that they let you *hide* your data design from the rest of a program!

From the outside, code can simply ask for a new ``Graph`` instance:

>>> from contingent import graphlib
>>> g = graphlib.Graph()

Behind the scenes, a pair of dictionaries has already been built
to store edges using the logic we outlined in the previous section:

.. include:: contingent/graphlib.py
    :code: python
    :start-line: 4
    :end-line: 6

.. include:: contingent/graphlib.py
    :code: python
    :start-line: 16
    :end-line: 19

The leading underscore
in front of the attribute names ``_inputs_of`` and ``_consequences_of``
is a common convention in the Python community
to signal that an attribute is private.
This convention is one way the community suggests
that programmers pass messages and warnings
through spacetime to each other.
Recognizing the need to signal differences among
public versus internal object attributes,
the community adopted the single leading underscore
as a concise and fairly consistent indicator
to other programmers,
including our future selves,
that the attribute is best treated
as part of the invisible internal machinery of the class.

Why are we using a “defaultdict” instead of a standard dict?
It automatically handles the special case of a missing key.
With a normal dict,
retrieving a key that does not exist raises a ``KeyError``:

>>> consequences_of = {}
>>> consequences_of['index.rst'].add('index.html')
Traceback (most recent call last):
     ...
KeyError: 'index.rst'

This usually requires special checks throughout the code
that uses a dict:

.. code:: python

    # Special case to handle “we have not seen this task yet”:

    if input_task not in self._consequences_of:
        self._consequences_of[input_task] = set()

    self._consequences_of[input_task].add(consequence_task)

The defaultdict lets you provide a function
that returns a value for absent keys.
When we ask about an edge that the ``Graph`` hasn't yet seen,
we will get back an empty ``set`` instead of an exception:

>>> from collections import defaultdict
>>> consequences_of = defaultdict(set)
>>> consequences_of['api.rst']
set()

Structuring our implementation this way means that
each key’s first use can look identical
to second-and-subsequent-times that a particular key is used:

>>> consequences_of['index.rst'].add('index.html')
>>> 'index.html' in consequences_of['index.rst']
True

Given these techniques, let’s build the graph for Figure 1.

First, we need a way to add edges between nodes:

.. include:: contingent/graphlib.py
    :code: python
    :start-line: 40
    :end-line: 44

This method hides the fact that two, not one,
storage steps are required for each new edge
so that we know about it in both directions.
And notice how ``add_edge()`` does not know or care
whether either node has been seen before.
Because the inputs and consequences data structures
are each a ``defaultdict(set)``,
``add_edge()`` remains blissfully ignorant
as to the novelty of a node,
while ``defaultdict`` takes care of the difference
by creating new ``set`` objects on the fly as needed.
As we saw above, ``add_edge()`` would be
three times longer had we not used ``defaultdict``.
More importantly, it would be much more difficult
to read its purpose and behavior from the resulting code.
This implementation demonstrates a Pythonic
approach to problems: simple, direct, and concise.

All of these considerations disappear
as soon as we turn away from the code inside of the class.
Code that is simply using the graph
sees only a single verb — the method call —
each time an edge is added:

>>> g.add_edge('index.rst', 'index.html')
>>> g.add_edge('tutorial.rst', 'tutorial.html')
>>> g.add_edge('api.rst', 'api.html')

Callers should also be given a simple way to visit every edge
without having to learn how to traverse our data structure:

.. include:: contingent/graphlib.py
    :code: python
    :start-line: 64
    :end-line: 69

The ``Graph.sorted()`` method, if you want to examine it later,
makes an attempt to sort the nodes
in case they have a natural sort order
(alphabetical, in this case)
that can provide a stable output order for the user.

By using this traversal method we can see that,
following our three “add” method calls earlier,
``g`` now represents the same graph that we saw in Figure 1.

>>> from pprint import pprint
>>> pprint(g.edges())
[('api.rst', 'api.html'),
 ('index.rst', 'index.html'),
 ('tutorial.rst', 'tutorial.html')]

Since we now have a real live Python object,
and not just a figure,
we can ask it interesting questions!
For example, when Contingent is building a blog from source files,
it will need to know things like “What depends on ``api.rst``?” when
the content of ``api.rst`` changes:

>>> g.immediate_consequences_of('api.rst')
['api.html']

This ``Graph`` is telling Contingent that,
when ``api.rst`` changes,
``api.html`` is now stale and must be rebuilt.
How about ``index.html``?

>>> g.immediate_consequences_of('index.html')
[]

An empty list has been returned,
signalling that ``index.html`` is at the right edge of the graph
and so nothing further needs to be rebuilt if it changes.
This query can be expressed very simply
thanks to the work that has already gone in to laying out our data:

.. include:: contingent/graphlib.py
    :code: python
    :start-line: 69
    :end-line: 72

Careful readers will have noticed that we added edges to our graph
without explicitly creating “node” and “edge” objects,
and that the nodes themselves in these early examples
are simply strings.
Coming from other languages and traditions,
one might have expected to see
user-defined classes and interfaces for everything in the system::

    Graph g = new ConcreteGraph();
    Node indexRstNode = new StringNode("index.rst");
    Node indexHtmlNode = new StringNode("index.html");
    Edge indexEdge = new DirectedEdge(indexRstNode, indexHtmlNode);
    g.addEdge(indexEdge);

The Python language and community explicitly and intentionally emphasize
using simple, generic data structures to solve problems,
instead of creating custom classes for every minute detail
of the problem we want to tackle.
This is one facet of the notion of “Pythonic” solutions that you may
have read about: Pythonic solutions try to
minimize syntactic overhead
and leverage Python's powerful built-in tools
and extensive standard library.

..
 >>> from contingent.rendering import as_graphviz
 >>> open('figure1.dot', 'w').write(as_graphviz(g)) and None

Figure 1 ignored one of the most important relationships
that we discovered in the opening section of our chapter:
the way that document titles appear in the table of contents.
Let’s fill in this detail.
We will create a node for each title string
that needs to be generated by parsing an input file
and then passed to one of our other routines:

>>> g.add_edge('api.rst', 'api-title')
>>> g.add_edge('api-title', 'index.html')
>>> g.add_edge('tutorial.rst', 'tutorial-title')
>>> g.add_edge('tutorial-title', 'index.html')

The result is a graph that could properly handle
rebuilding the table of contents that we discussed
in the opening of this chapter.

..
 >>> open('figure2.dot', 'w').write(as_graphviz(g)) and None

.. figure:: figure2.png

   **Figure 2.** Being prepared to rebuild ``index.html``
   whenever any title that it mentions gets changed.

This manual walk-through illustrates what we
will eventually have Contingent do for us:
the graph ``g`` captures the inputs and consequences
for the various artifacts in our project's documentation.

..  I wonder if we should defer talking about clear_inputs_of() until
..  later, rather than trying to conjure up a motivation for it here.

Learning Connections
====================

We now have a way for Contingent
to keep track of tasks and the relationships between them.
If we look more closely at Figure 2, however,
we see that it is actually a little hand wavy and vague:
what does it mean to say that
``index.rst``, ``api-title``, and ``tutorial.html`` are “tasks?”

Our intuitive notion of these ideas
served when we were constructing consequences graphs by hand,
but unfortunately computers are not terribly intuitive,
so we will need to be more precise about what we want.
What are tasks?
How are they defined and executed?
And how can Contingent know the connections between them?

In Contingent, tasks are modeled as functions plus arguments.
The functions define actions that the particular project
understands how to perform.
The arguments provide
the specifics: *which* source document should be read,
*which* blog title is needed.
As they are running,
these functions may in turn invoke *other* task functions,
passing whatever arguments they need answers for.

To see how this works we will actually now implement
the documentation builder described at the beginning of the chapter.
In order to prevent ourselves from wallowing around in a bog of details,
for this illustration we will work with
simplified input and output document formats.
Our input documents will consist of a title on the first line,
with the remainder of the text forming the body.
Cross references will simply be source file names
enclosed in back ticks,
which on output are replaced with the title
from the corresponding document in the output.

Here is the content of our example
``index.txt``, ``api.txt``, and ``tutorial.txt``,
illustrating titles, document bodies, and cross-references
from our little document format:

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

.. TODO: these next few paragraphs need some smoothing and redundancy removal.

Now that we have some source material to work with,
what functions would a Contingent-based blog builder
need?

In the simplistic examples above,
the HTML output files proceed directly from the source,
but in a realistic system,
turning source into markup involves several steps:
reading the raw text from disk,
parsing the text to a convenient internal representation,
processing any directives the author may have specified,
resolving cross-references or other external dependencies
(such as include files),
and applying one or more view transformations
to convert the internal representation to its output form.

Contingent manages tasks by grouping them into a ``Project``,
a sort of build system busybody
that injects itself into the middle of the build process,
noting every time one task talks to another
to construct a graph of the relationships between all the tasks.

>>> from contingent.projectlib import Project, Task
>>> project = Project()
>>> task = project.task

A build system for the example given at the beginning of the chapter
might involve a few basic tasks.

* Our ``read()`` task will pretend to read the files from disk.
  Since we really defined the source text in variables,
  all it needs to do is convert from a filename
  to the corresponding text.

    >>> filesystem = {'index.txt': index, 'tutorial.txt': tutorial,
    ...               'api.txt': api}
    ...
    >>> @task
    ... def read(filename):
    ...     return filesystem[filename]

* The ``parse()`` task interprets the raw text of the file contents
  according to the specification of our document format.
  Our format is very simple:
  the title of the document appears on the first line,
  and the rest of the content is considered the document's body.

    >>> @task
    ... def parse(filename):
    ...     lines = read(filename).strip().splitlines()
    ...     title = lines[0]
    ...     body = '\n'.join(lines[2:])
    ...     return title, body

  Because the format is so simple,
  the parser is a little silly, admittedly,
  but it illustrates the interpretive responsibilities
  that parsers are required to carry out.
  Parsing in general is a very interesting subject
  and many books have been written
  either partially or completely dedicated to it.
  In a system like Sphinx,
  the parser must understand the many markup tokens,
  directives, and commands defined by the system,
  transforming the input text into something
  the rest of the system can work with.

  Notice the connection point between
  ``parse()`` and ``read()``:
  the first task in parsing is to pass the filename it has been given
  to ``read()``, which finds and returns the contents of that file.

* The ``title_of()`` task, given a source file name,
  returns the document's title:

    >>> @task
    ... def title_of(filename):
    ...     title, body = parse(filename)
    ...     return title

  .. TODO: something is wrong with the next paragraph; looks like a bad
  .. edit

  This task nicely illustrates the
  separation of responsibilities between
  the parts of a document processing system:
  ``title_of()`` works from an in-memory representation of a document,
  in this case a tuple,
  rather than *itself* having to sift the chaos of bits
  in the input file looking for a document title.
  Its job is to produce the in-memory representation,
  in accordance with the contract of the system specification,
  that the rest of the blog builder processing functions
  like ``title_of()``
  will expect to be able to use.

  If you are coming from an orthodox object-oriented tradition,
  this function-oriented design may look a little weird.
  In an OO solution,
  ``parse()`` would return some sort of ``Document`` object
  that has ``title_of()`` as a method or property.
  In fact, Sphinx works exactly this way:
  its ``Parser`` subsystem produces a “Docutils document tree” object
  for the other parts of the system to use.

  Contingent is not opinionated
  with regard to these differing design paradigms
  and supports either approach equally well.
  For this chapter we are keeping things simple.

* The final task,
  ``render()``,
  turns the in-memory representation of a document
  into an output form.
  It is, in effect, the inverse of ``parse()``.
  Whereas ``parse()`` takes an input document
  conforming to a specification
  and converts it to an in-memory representation,
  ``render()`` takes an in-memory representation
  and produces an output document
  conforming to some specification.

    >>> import re
    >>>
    >>> LINK = '<a href="{}">{}</a>'
    >>> PAGE = '<h1>{}</h1>\n<p>\n{}\n<p>'
    >>>
    >>> def make_link(match):
    ...     filename = match.group(1)
    ...     return LINK.format(filename, title_of(filename))
    ...
    >>> @task
    ... def render(filename):
    ...     title, body = parse(filename)
    ...     body = re.sub(r'`([^`]+)`', make_link, body)
    ...     return PAGE.format(title, body)

Here is an example run
that will invoke every stage of the above logic —
rendering ``tutorial.txt`` to produce its output:

>>> print(render('tutorial.txt'))
<h1>Beginners Tutorial</h1>
<p>
Welcome to the tutorial!
We hope you enjoy it.
<p>

Figure 3 illustrates the task graph
that transitively connects all the tasks
required to produce the output,
from reading the input file,
parsing and transforming the document,
and rendering the result:

..
 >>> open('figure3.dot', 'w').write(as_graphviz(project._graph)) and None

.. image:: figure3.png

It turns out that Figure 3 was not hand-drawn for this chapter,
but has been generated directly from Contingent!
Building this graph is possible for the ``Project`` object
because it maintains its own call stack,
similar to the stack of live execution frames
that Python maintains to remember which function to continue running
when the current one returns.

Every time that a new task is invoked,
Contingent can assume that it has been called —
and that its output will be used —
by the task currently at the top of the stack.
Maintaining the stack will require that several extra steps
surround the invocation of a task T:

1. Push T onto the stack.
2. Execute T, letting it call any other tasks it needs.
3. Pop T off the stack.
4. Return its result.

To intercept task calls,
the ``Project`` leverages a key Python feature: *function decorators*.
A decorator is allowed to process or transform a function
at the moment that it is being defined.
The ``Project.task`` decorator uses this opportunity
to package every task inside another function, a *wrapper*,
allowing a clean separation of responsibilities.
The wrapper worries about graph and stack management —
of which the task function remains blissfully ignorant —
while each task function can focus on
the work needed to be done to perform the task.
Here is what the ``task`` decorator boilerplate looks like:

.. code-block:: python

        from functools import wraps

        def task(function):
            @wraps(function)
            def wrapper(*args):
                # wrapper body, that will call function()
            return wrapper

This is an entirely typical Python decorator declaration.
It can then be applied to a function
by naming it after a ``@`` character atop the ``def``
that creates the function:

.. code-block:: python

    @task
    def title_of(filename):
        title, body = parse(filename)
        return title

Python assigns the wrapper function to the name ``title_of``.
The wrapper which can access the original version of the function
via the name ``function``,
calling it at the appropriate time.
The body of the Contingent wrapper
runs something like this:

.. code-block:: python

    task = Task(wrapper, args)

    if self.task_stack:
        self._graph.add_edge(task, self.task_stack[-1])

    self._graph.clear_inputs_of(task)
    self._task_stack.append(task)
    try:
        value = function(*args)
    finally:
        self._task_stack.pop()

    return value

This wrapper performs several crucial maintenance steps:

1. Packages the task —
   a function plus its arguments —
   into a small object for convenience.
   The ``wrapper`` here names the wrapped version of the task function.

2. If this task has been invoked
   by a current task that is already underway,
   add an edge capturing the fact that
   this task is an input to the already-running task.

3. Forget whatever we might have learned last time about the task,
   since it might make new decisions this time —
   if the source text of the API guide no longer mentions the Tutorial,
   for example, then its ``render()`` will no longer ask
   for the ``title_of()`` the Tutorial document.

4. Push this task onto the top of the task stack
   in case it should, in its turn, invoke further tasks
   in the course of doing its work.

5. Invoke the task
   inside of a ``try...finally`` block
   that ensures we correctly remove the finished task from the stack
   even if a task dies by raising an exception.

6. Return the task’s return value,
   so that callers of this wrapper
   will not be able to tell that they have not simply invoked
   the plain task function itself.

Steps 4 and 5 maintain the task stack itself,
which is then used by step 2 to perform the consequences tracking
that is our whole reason for building a task stack in the first place.

Since each task gets surrounded by its own copy of the wrapper function,
the mere invocation and execution of the normal stack of tasks
will produce a graph of relationships as an invisible side effect.
That is why we were careful to use the wrapper
around every one of the processing steps we defined:

.. code-block:: python

    @task
    def read(filename):
        # body of read

    @task
    def parse(filename):
        # body of parse

    @task
    def title_of(filename):
        # body of title_of

    @task
    def render(filename):
        # body of render

Thanks to these wrappers,
when we called ``parse('tutorial.txt')``
the decorator learned
the connection between ``parse`` and ``read``.
We can ask about the relationship by building another ``Task`` tuple
and asking what the consequences would be
if its output value changed:

>>> task = Task(read, ('tutorial.txt',))
>>> print(task)
read('tutorial.txt')
>>> project._graph.immediate_consequences_of(task)
[parse('tutorial.txt')]

The consequence of re-reading the ``tutorial.txt`` file
and finding its contents have changed
is that we need to re-execute the ``parse()`` routine for that document.
What happens if we render the entire set of documents?
Will Contingent be able to learn the entire build process
with its interrelationships?

>>> for filename in 'index.txt', 'tutorial.txt', 'api.txt':
...     print(render(filename))
...     print('=' * 30)
...
<h1>Table of Contents</h1>
<p>
* <a href="tutorial.txt">Beginners Tutorial</a>
* <a href="api.txt">API Reference</a>
<p>
==============================
<h1>Beginners Tutorial</h1>
<p>
Welcome to the tutorial!
We hope you enjoy it.
<p>
==============================
<h1>API Reference</h1>
<p>
You might want to read
the <a href="tutorial.txt">Beginners Tutorial</a> first.
<p>
==============================

It worked!
From the output, we can see that
our transform substited the docuent titles
for the directives in our source docuents,
indicating that Contingent was able to
discover the connections between the various tasks
needed to build our documents.

..
 >>> open('figure4.dot', 'w').write(as_graphviz(project._graph)) and None

.. figure:: figure4.png

   **Figure 4.** The complete set of relationships
   between our input files and our HTML outputs.

By watching one task invoke another
through the ``task`` wrapper machinery,
``Project`` has automatically learned
the graph of inputs and consequences.
Since it has a complete consequences graph
at its disposal,
Contingent knows all the things to rebuild
if the inputs to any tasks change.

Chasing Consequences
====================

Once the initial build has run to completion,
Contingent needs to monitor the input files for changes.
When the user finishes a new edit and runs “Save,”
both the ``read()`` method and its consequences need to be invoked.

This will require us to walk the graph in the opposite order
from the one in which it was created.
It was built, you will recall, by calling
``render()`` for the API Reference and having that call ``parse()``
which finally invoked the ``read()`` task.
Now we go in the other direction:
we know that ``read()`` will now return new content,
and we need to figure out what consequences lie downstream.

The process of compiling consequences is a recursive one,
as each consequence can itself have further tasks that depended on it.
We could perform this recursion manually
through repeated calls to the graph:

>>> task = Task(read, ('api.txt',))
>>> project._graph.immediate_consequences_of(task)
[parse('api.txt')]
>>> t1, = _
>>> project._graph.immediate_consequences_of(t1)
[render('api.txt'), title_of('api.txt')]
>>> t2, t3 = _
>>> project._graph.immediate_consequences_of(t2)
[]
>>> project._graph.immediate_consequences_of(t3)
[render('index.txt')]
>>> t4, = _
>>> project._graph.immediate_consequences_of(t4)
[]

Note that we are here taking advantage
of the fact that the Python prompt saves the last value displayed
under the name ``_`` for use in the subsequent expression.

This recursive task of looking repeatedly for immediate consequences
and only stopping when we arrive at tasks with no further consequences
is a basic enough graph operation that it is supported directly
by a method on the ``Graph`` class:

>>> project._graph.recursive_consequences_of([task])
[parse('api.txt'), render('api.txt'), title_of('api.txt'), render('index.txt')]

In fact, ``recursive_consequences_of()`` tries to be a bit clever.
If a particular task appears repeatedly as a downstream consequence
of several other tasks,
then it is careful to only mention it only once in the output list
while moving it close to the end
so that it appears only after the tasks that are its inputs.
This intelligence is powered by the classic depth-first implementation
of a topological sort,
an algorithm which winds up being fairly easy to write in Python
through a hidden a recursive helper function.
Check out the ``graphlib.py`` source code for the details
if graph algorithms interest you!

If upon detecting a change
we are careful to re-run every task in the recursive consequences,
then Contingent will be able to avoid rebuilding too little.
Our second challenge, however,
was to avoid rebuilding too much.
We want to avoid rebuilding all three documents
every time that ``tutorial.txt`` is changed,
since most edits will probably not affect its title but only its body.
How can this be accomplished?

The solution is to make graph recomputation dependent on caching.
When stepping forward through the recursive consequences of a change,
we will only invoke tasks whose inputs are different than last time.

This optimization will involve a final data structure:

* We will give the ``Project`` a ``_todo_list`` of tasks
  that have had at least one input change
  and that therefore require re-execution.

* If we reach a task in the recursive consequences
  but do not find it listed in the ``_todo_list``
  then we will run the task to generate a new value.

* After running the task we will compare its new output value
  to the one that it returned last time.
  If they are different, then every immediate consequence
  of the task gets added to the ``_todo_list``.

* Once the ``_todo_list`` is empty,
  Contingent knows that all task outputs are now up to date.

Again, Python’s convenient and unified design
makes these features very easy to code.
Because task objects are hashable,
the ``_todo_list`` can simply be a set
that remembers task items by identity —
guaranteeing that a task never appears twice —
and the ``_cache`` of return values from previous runs
can be a dict with tasks as keys.

Suppose you edit ``tutorial.txt``
and change both the title and the body content.
We can simulate this by modifying the value
in our ``filesystem`` dict:

>>> filesystem['tutorial.txt'] = """
... The Coder Tutorial
... ------------------
... This is a new and improved
... introductory paragraph.
... """

Since the contents have changed,
we need to tell the Project that the task needs to be re-run:

>>> task = Task(read, ('tutorial.txt',))
>>> project.invalidate(task)

The ``Project`` class supports a simple tracing facility
that will tell us which tasks are executed in the course
of a rebuild.
Since the above change to ``tutorial.txt``
affects both its body and its title,
everything downstream will need to be re-computed:

>>> project.start_tracing()
>>> project.rebuild()
>>> print(project.stop_tracing())
calling read('tutorial.txt')
calling parse('tutorial.txt')
calling render('tutorial.txt')
calling title_of('tutorial.txt')
calling render('api.txt')
calling render('index.txt')

Looking back at Figure 4,
you can see that, as expected,
this is every task that is an immediate or downstream consequence
of ``read('tutorial.txt')``.

But what if we edit it again,
but this time leave the title the same?

>>> filesystem['tutorial.txt'] = """
... The Coder Tutorial
... ------------------
... Welcome to the coder tutorial!
... It should be read top to bottom.
... """
>>> project.invalidate(task)

This small, limited change
should have no effect on the other documents.

>>> project.start_tracing()
>>> project.rebuild()
>>> print(project.stop_tracing())
calling read('tutorial.txt')
calling parse('tutorial.txt')
calling render('tutorial.txt')
calling title_of('tutorial.txt')

Success!
Only one document got rebuilt.
The fact that ``title_of()``, given a new input document,
nevertheless returned the same value means that all further
downstream tasks were insulated from the change
and did not get re-invoked.

Conclusion
==========

There exist languages and programming methodologies
under which Contingent would be a suffocating forest of tiny classes
giving useless and verbose names to every concept in the problem domain.

When programming Contingent in Python, however,
we skipped the creation of a dozen classes that could have existed,
like ``TaskArgument`` and ``CachedResult`` and ``ConsequenceList``.
We instead drew upon Python’s strong tradition
of solving generic problems with generic data structures,
resulting in code that repeatedly uses a small set of ideas
from the core data structures tuple, list, set, and dict.

But does this not cause a problem?

Generic data structures are also, by their nature, anonymous.
Our ``project._cache`` is a set.
So is every collection of upstream and downstream nodes
inside the ``Graph``.
Are we in danger of seeing generic ``set`` error messages
and not knowing whether to look in the project or the graph
implementation for the error?

In fact, we are not in danger!

Thanks to the careful discipline of encapsulation —
of only allowing ``Graph`` code to touch the graph’s sets,
and ``Project`` code to touch the project’s set —
there will never be ambiguity if a set operation
returns an error during a later phase of the project.
The name of the innermost executing method at the moment of the error
will necessarily direct us to exactly the class, and set,
involved in the mistake.
There is no need to create a subclass of ``set``
for every possible application of the data type,
so long as we put that conventional underscore in front of data
structure attributes and then are careful not to touch them
from code outside of the class.

Contingent demonstrates how crucial the Facade pattern,
from the epochal *Design Patterns* book,
is for a well-designed Python program.
Not every data structure and fragment of data in a Python program
gets to be its own class.
Instead, classes are used sparingly,
at conceptual pivots in the code where a big idea —
like the idea of a dependency graph —
can be wrapped up into a Facade
that hides the details of the simple generic data structures
that lie beneath it.

Code outside of the Facade
names the big concepts that it needs
and the operations that it wants to perform.
Inside of the Facade,
the programmer manipulates the small and convenient moving parts
of the Python programming language to make the operations happen.

.. Links
.. _Sphinx: http://sphinx-doc.org/
