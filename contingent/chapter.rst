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
with the venerable “make” utility,
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
to form a graph:

.. image:: figure1.png

.. * Then add_edge() then remove_edge()
.. * Contingent will sometimes need to rebuild, in case ``api.rst``
..   is edited to add or remove a cross reference: clear_inputs_of()
.. * (leave the rest out for now?)
.. * Finally, step back: our API only asks that nodes be represented
..   with hashable types. Discuss that tuples are fine too, not just
..   strings like we are using in this section.
.. * Another step back: note that our API hides the data structures
..   COMPLETELY! We could change to a stupid Node class and Edge class
..   any time we wanted, because only task IDs pass our API border.

How could we represent such a graph in Python?

The language gives priority to four generic data structures
by giving them direct support in the language syntax.
You can create new instances of the big-four data structures
by simply typing them into your source code.

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

You may have been surprised
by the absence of classes in the above discussion
of Python data structures.
After all, they are a frequent mechanism for organizing data
and get defined through a dedicated Python syntax.

But it turns out that classes are often
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
as part of the internal invisible machinery of the class.

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
    :start-line: 65
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



Adding an edge means updating both the
inputs and consequences structures;
here, adding a consequence of ``'tutorial.rst'``
also adds an input of ``'tutorial-title'``:

>>> g.add_edge('tutorial.rst', 'tutorial-title')
>>> g.inputs_of('tutorial-title')
['tutorial.rst']

The implementation of ``remove_edge()`` is,
unsurprisingly, the inverse of ``add_edge()``:

.. include:: contingent/graphlib.py
    :code: python
    :start-line: 40
    :end-line: 45

Figure 1 shows a simplified representation of our example
that ignores document titles and the table of contents.
Let's fill in some of the details:
a moment ago we added an edge
from ``tutorial.rst`` to ``tutorial-title``

>>> 'tutorial-title' in g.immediate_consequences_of('tutorial.rst')
True

and we also need to show that ``api.rst`` has ``api-title`` as
a consequence and that both of these are inputs to ``index.html``:

>>> g.add_edge('api.rst', 'api-title')
>>> g.add_edge('tutorial-title', 'index.html')
>>> g.add_edge('api-title', 'index.html')

This manual walk-through illustrates what we
will eventually have Contingent do for us:
the graph ``g`` captures the inputs and consequences
for the various artifacts in our project's documentation.
Figure 2 depicts the result.

..
 >>> open('figure2.dot', 'w').write(as_graphviz(g)) and None

.. image:: figure2.png

.. ----

.. [I wonder if we should defer talking about clear_inputs_of() until
..  later, rather than trying to conjure up a motivation for it here.]

.. >>> g.add_edge('tutorial-title', 'api.html')

.. .. include:: contingent/graphlib.py
..     :code: python
..     :start-line: 50
..     :end-line: 55

.. FINALLY -

.. is it time here to describe and justify the consequences methods?
.. Maybe?


.. >>> g.immediate_consequences_of('index.rst')
.. ['index.html']

.. That is simple.  But this is a several-step cascade,
.. we have to follow to the bottom:

.. >>> sorted(g.immediate_consequences_of('api.rst'))
.. ['api-title', 'api.html']
.. >>> g.immediate_consequences_of('api-title')
.. ['index.html']
.. >>> g.immediate_consequences_of('index.html')
.. []
.. >>> g.immediate_consequences_of('api.html')
.. []

.. Whenever things change we want to do that, but to be careful of the
.. order.  [Ugh - should we even explain?  Maybe just mention for the
.. advanced people: to avoid rerunning a task several times, we need a
.. topological sort.]

.. >>> g.recursive_consequences_of(['api.rst'])
.. ['api-title', 'index.html', 'api.html']

.. Wow look it did what we did manually above!  It's great!


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
so we'll need to be more precise about what we want.
What are tasks?
How are they defined and executed?
And how can Contingent know the connections between them?

In Contingent, tasks are modeled as functions plus arguments,
where the functions define actions the particular project
understands how to perform with the arguments providing
the specifics: *which* source document should be read,
*which* blog title is needed.
As they are running,
these functions may in turn invoke *other* task functions,
passing whatever arguments they need answers for.

To see how this works we'll continue with our blog building example.
In order to prevent ourselves from wallowing around in a bog of details,
for this illustration we will work with
simplified input and output document formats.
Our input documents will consist of a title on the first line,
with the remainder of the text forming the body.
Cross references are simply source file names
enclosed in back ticks (`````),
which are replaced with the title
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

This simplified build system involves five basic steps:
*reading* the raw contents of a file,
*parsing* the contents to produce an in-memory document representation,
*extracting* information from our document representation,
*transforming* the document representation,
and *rendering* the output document.

* ``read()`` pretends to read the files from disk;
  since we defined the source text in variables,
  all it actually needs to do is convert from a filename
  to the corresponding text:

    >>> filesystem = {'index.txt': index, 'tutorial.txt': tutorial,
    ...               'api.txt': api}
    >>> @task
    ... def read(filename):
    ...     return filesystem[filename]

* ``parse()`` interprets the raw text of the file contents
  according to the specification of our document format.
  Our format is very simple:
  the title of the document appears on the first line,
  and the rest of the content is considered the document's body.

    >>> @task
    ... def parse(filename):
    ...     text = read(filename).strip('\n')
    ...     title, body = text.split('\n', 1)
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
  ``parse()``'s first task is to pass the filename it has been given
  to ``read()``, which finds and returns the contents
  of that file.

* ``title_of()``, given a source file name,
  returns the document's title:

    >>> @task
    ... def title_of(filename):
    ...     title, body = parse(filename)
    ...     return title

  This task nicely illustrates the
  separation of responsibilities between
  the parts of a document processing system:
  ``title_of()`` works from an in-memory representation of a document,
  in this case a tuple,
  rather than *itself* having to sift the chaos of bits
  in the input file looking for a document title.
  ``parse()``'s job is to produce the in-memory representation,
  in accordance with the contract of the system specification,
  that the rest of the blog builder processing functions
  like ``title_of()``
  expect to be able to use.

  If you are coming from an orthodox object-oriented tradition,
  this function-oriented design may look a little weird.
  In an OO solution,
  ``parse()`` would return some sort of ``Document`` object
  that has ``title_of()`` as a method or property.
  In fact, Sphinx works exactly this way,
  its ``Parser`` subsystem producing a “Docutils document tree” object
  for the other parts of the system.
  Contingent is not opinionated
  with regard to these differing design paradigms
  and supports either approach equally well.

* A document processing system like Sphinx
  defines many useful document *transforms*
  that implement features like cross referencing,
  formatting, and document assembly.
  Our simplified document specification
  includes only simple cross-referencing
  implemented in this ``transform()`` function

    >>> import re
    >>> @task
    ... def transform(filename):
    ...     title, raw_body = parse(filename)
    ...     body = re.sub(r'`([^`]+)`',
    ...         lambda match: repr(title_of(match.group(1))),
    ...         raw_body)
    ...     return title, body

  which looks up
  document titles enclosed in backticks (```tutorial.txt```)
  and replaces them with the corresponding document's title.

    .. TODO: how much explanation do we need here? If you don't already
        know how this works, this implementation is pretty inscrutable.

* ``render()``'s job is to turn the in-memory representation of a document
  into an output form; it is, in effect, the inverse of ``parse()``.
  ``parse()`` takes an input document
  conforming to a specification
  and converts it to an in-memory representation;
  ``render()`` takes an in-memory representation
  and produces an output document
  conforming to some specification.
  Both functions could, in fact,
  work with the same document specification,
  which isn't as weird as it might sound at first:
  just because we are working from the same specification
  doesn't mean we have to produce the same *document*.
  Besides, in-memory representations can sometimes be easier to work with
  than raw text if the document specification is complicated (think HTML).

    >>> @task
    ... def render(filename):
    ...     title, body = transform(filename)
    ...     return title + '\n' + body


Here's an example:
rendering ``tutorial.txt``
produces its output

>>> print(render('tutorial.txt'))
Beginners Tutorial
------------------
Welcome to the tutorial!
We hope you enjoy it.

Figure 3 illustrates the task graph
that transitively connects all the tasks
required to produce the output,
from reading the input file,
parsing and transforming the document,
and rendering the result:

..
 >>> open('figure3.dot', 'w').write(as_graphviz(project.graph)) and None

.. image:: figure3.png


Wait, where did Figure 3 come from?
It turns out that,
like your nosy next-door neighbor,
``Project`` spies on all of the interactions
between its tasks.
To see how this works,
we need to look at what happens when
one function calls another.
When ``parse('tutorial.txt')`` calls ``read('tutorial.txt')``,
Python can't simply jump
from ``parse``'s code to ``read``'s,
since that would lose track of what we were doing
in ``parse`` when the call was made,
leaving our program wandering around forgetting everything
like Leonard from *Memento*,
with equally disastrous consequences.
In order to keep track of things like
*who* called ``read``,
where we were in the code when the call was made,
and what local variables were in play at the time,
Python maintains a *call stack*
with the information for the currently-executing function
on top.
Calling a function pushes its information to the top of the stack;
returning from a function pops it off.
This elegant structure allows each function
to work within its own local context,
with the stack maintaining the call chain from one function to another.
For example,
when ``read()`` returns and its information is popped off the stack,
``parse()``, which called it,
is now the top function,
and control returns to it at the point the call was made.

*But wait*, you're thinking,
*we need to track* **tasks**,
*not just any old function that comes along.*
I like where your head's at,
particularly if it's still attached to you.
While Python provides introspection facilities
that will let a program examine the real function call stack,
it's true that these facilities don't quite match our need:
we care when one *task* invokes another,
which will be difficult to tease out from
the myriad function calls flying about as our program runs.
Instead, we can maintain our own *task* stack,
so when ``parse('tutorial.txt')`` calls ``read('tutorial.txt')``,
``Project`` can jot that fact down in its task graph.

Let's explore this idea by looking at
expanded versions of ``parse`` and ``read``
to see how they would work with a task stack.
To maintain the stack as we run our tasks,
we need to ensure that tasks are pushed on as they begin
and popped off as they complete,
just like the call stack does for function calls.
Each time a task runs,
we need to

1. push the task onto the stack
2. execute the task, which may involve calling other tasks
3. pop the task off the stack
4. return its result

For example, here is an expanded version of the ``read`` task function,
cleverly called ``read_task``,
which handles both ``read``'s actual responsibilities
and task stack management
using a simple Python list as the task stack:

>>> task_stack = []
>>> def read_task(filename):
...     task = Task(read_task, (filename,))
...     task_stack.append(task)
...     print(task_stack)
...     result = 'Witty Title\nEngaging content'
...     task_stack.pop()
...     return result


Calling ``read_task`` prints out the task stack as it runs:

>>> read_task('api.rst') and None
[read_task('api.rst')]

More interesting is to see what happens
when we call ``parse_task``,
the task function version of ``parse``.
Notice that if one task invokes another task
while it is doing its work (step 2 above),
the calling task will be the one at the top of the stack
when the call is made.
Since ``parse_task`` will call ``read_task``
to get the contents of the file it needs to parse,
our stack will be two tasks deep
when ``read_task`` is called:

>>> def parse_task(filename):
...     task = Task(parse_task, (filename,))
...     task_stack.append(task)
...     print(task_stack)
...     text = read_task(filename).strip('\n')
...     title, body = text.split('\n', 1)
...     task_stack.pop()
...     return title, body

>>> parse_task('api.rst') and None
[parse_task('api.rst')]
[parse_task('api.rst'), read_task('api.rst')]

Contingent's ``Project`` class,
which we looked at briefly above,
exists primarily to maintain a task stack like this,
and, as one task calls another,
to record the interaction in its consequences graph.
Of course writing all that task stack management code
in each task function would be
tedious, repetitive, error-prone, and repetitive,
so ``Project`` leverages another Python feature
to help out: *function decorators*.
A function decorator packages a function
inside another function, a *wrapper*,
allowing clean separation of responsibilities:
the wrapper worries about graph and task stack management —
of which the task function remains blissfully ignorant —
while each task function can focus on
the work needed to be done to perform the task.
Here is what the ``task`` decorator boilerplate looks like:

.. code-block:: python

        def task(self, task_function):
            @wraps(task_function)
            def wrapper(*args):
                # wrapper body

            return wrapper

which is a fairly typical Python decorator declaration.
When the decorator is used, like this:

.. code-block:: python

    @task
    def title_of(filename):
        title, body = parse(filename)
        return title

Python replaces ``title_of`` with the wrapper function,
which can access the original version of the function
via the name ``task_function``,
calling it at the appropriate time.
The body of the wrapper function

.. code-block:: python

    task = Task(wrapper, args)

    if self.task_stack:
        self.graph.add_edge(task, self.task_stack[-1])

    self.task_stack.append(task)
    try:
        value = task_function(*args)
    finally:
        self.task_stack.pop()

    return value


performs
these maintenance steps:

1. package the task — which Contingent models
   as a task function plus its arguments —
   into a task tuple for convenience.
   ``wrapper`` here means the wrapped version
   of the task function.

2. if this task was invoked by a previous task,
   add an edge capturing the fact that
   this task has the previous one as a consequence

3. push this task to the top of the task stack

4. invoke the task, capturing its return value.
   The ``try...finally`` block ensures
   we correctly maintain the task stack
   even if a task raises an exception

5. pop this task off the top of the task stack

6. return the result of executing the task

Notice that steps 3–6 correspond
to the pattern we set out above
for managing a task stack,
and step 2 adds the consequences tracking
that was our whole reason for
endeavoring to build a task stack in the first place.
Since each task gets its own copy of the wrapper function,
we can do this as many times as we need to,
allowing each task to automatically maintain
the task stack and consequences graph
without itself having to know anything about them.
With this task decorator in place,
we can now see that our original task definitions

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
    def transform(filename):
        # body of transform

    @task
    def render(filename):
        # body of render

are in fact wrapped task functions that,
as each task is called,
maintain the task stack and
dynamically learn the connections between the tasks
as one invokes another.
So when we called ``parse('tutorial.txt')`` earlier,
``Project``'s ``task`` decorator learned
the connection between ``parse`` and ``read``:

>>> task = Task(read, ('tutorial.txt',))
>>> print(task)
read('tutorial.txt')
>>> project.graph.immediate_consequences_of(task)
[parse('tutorial.txt')]

What happens if we render the entire set of documents?

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
From the output, we can see that
our transform substited the docuent titles
for the directives in our source docuents,
indicating that Contingent was able to
discover the connections between the various tasks
needed to build our documents,
as shown by Figure 4:

..
 >>> open('figure4.dot', 'w').write(as_graphviz(project.graph)) and None

.. image:: figure4.png

By watching one task invoke another
through the ``task`` wrapper machinery,
``Project`` has automatically learned
the graph of inputs and consequences.
Since it has a complete consequences graph
at its disposal,
Contingent knows all the things to rebuild
if the inputs to any tasks change.

But can it avoid rebuilding them?
Look at all the things that MIGHT need to be rebuilt
if the tutorial source text is touched.

>>> task = read, ('tutorial.txt',)
>>> pprint(project.graph.recursive_consequences_of([task]))
[parse('tutorial.txt'),
 title_of('tutorial.txt'),
 transform('api.txt'),
 render('api.txt'),
 transform('index.txt'),
 render('index.txt'),
 transform('tutorial.txt'),
 render('tutorial.txt')]

But what if the tutorial's title did not change?
As you can see in Figure 4,
changes to the tutorial that do not affect the title
should not need to touch the other documents.

What can we do?

Caching Consequences
====================

So far, we have built a system that avoids rebuilding too little:
by maintaining a complete consequences graph,
Contingent knows every task that is a downstream consequence
of the tutorial's source text
and can force each affected task to be recomputed.
Our second challenge, however,
is to avoid rebuilding too *much*:
we want to avoid rebuilding all the documents
if ``tutorial.txt`` is touched but its title has not changed,
since we know that the tutorial's body
has no effect on the other documents.

How? Caching, my boy, caching!

First, though, we need to be more precise about what we mean.
That is, we need to express the idea
“``tutorial.txt`` is touched but its title has not changed”
in terms of Contingent's model concepts of
tasks, values, and consequences.
What we need is a way to compare a task's output
with the value produced by the previous run:
we say that a task has changed if
its current value is different from the value it produced last time.
For example, the current value of this task is:

>>> title_of('tutorial.txt')
'Beginners Tutorial'

Suppose you edit ``tutorial.txt``,
change both the title and the body content,
and save your work to the file system:

>>> filesystem['tutorial.txt'] = """
... The Coder Tutorial
... ------------------
... This is a new and improved
... introductory paragraph.
... """

Since the contents have changed,
the task responsible for reading the file in is now invalid:

>>> task = read, ('tutorial.txt',)
>>> project.invalidate(task)

which means that everything gets rebuilt,
because every document needs to change.

>>> project.start_tracing()
>>> project.rebuild()
>>> print(project.stop_tracing())
calling read('tutorial.txt')
calling parse('tutorial.txt')
calling title_of('tutorial.txt')
calling transform('api.txt')
calling render('api.txt')
calling transform('index.txt')
calling render('index.txt')
calling transform('tutorial.txt')
calling render('tutorial.txt')

Looking back at Figure 4,
you can see that, as expected,
this is every task that is an immediate or downstream consequence
of ``read('tutorial.txt')``; also unsurprisingly,
the tutorial title task exhibits the updated value:

>>> title_of('tutorial.txt')
'The Coder Tutorial'

But what if we edit it again,
but this time leave the title the same?

>>> filesystem['tutorial.txt'] = """
... The Coder Tutorial
... ------------------
... Welcome to the coder tutorial!
... It should be read top to bottom.
... """
>>> project.invalidate(task)



.. include:: contingent/projectlib.py
    :code: python
    :start-line: 78
    :end-line: 96


.. include:: contingent/graphlib.py
    :code: python
    :start-line: 50
    :end-line: 55


.. include:: contingent/projectlib.py
    :code: python
    :start-line: 110
    :end-line: 123



So show rest of stuff from the listing?

Show how awesome Python is:
again, because functions are both 1st class objects
and are also hashable, we can use them as part of keys:
(f, args) is a completely natural key.





This should have no effect on the other documents.

>>> project.start_tracing()
>>> project.rebuild()
>>> print(project.stop_tracing())
calling read('tutorial.txt')
calling parse('tutorial.txt')
calling title_of('tutorial.txt')
calling transform('tutorial.txt')
calling render('tutorial.txt')

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

..    x...original `Learning About Pandas`_ blog post from last year.

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
