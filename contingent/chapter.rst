
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
that Sphinx has included the attractive human-readable title
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

In the sections that follow we will construct such a tool in Python,
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

.. OKAY, DAN!

.. Time to go to town!  Everything from here down is (a) great code that I
.. have just edited, with (b) rough notes for you to work from.  Turn on
.. your CS prof superpowers and explain everything clearly, concisely, and
.. whimsically.

.. Also, edit any of my stuff above that you'd like.  And I can edit you
.. when you have turned the notes that follow into text, and after our
.. mutual edits hopefully it will read fairly clearly.

.. I have adjusted the code below so that it shows what Graph() does more
.. simply than before, so we can work on explaining the implementation.
.. Put words around these code samples, talking about the following points
.. (based on Debo's email to us) (these do not need to be long parts each,
.. by the way, you just need to make each point clearly, along with other
.. points that I'm sure will occur to you as you quote sections of
.. graphlib.py and discuss them (try to include them with RST file-line
.. inclusion, not by cutting and pasting):

.. * A graph needs to store edges.
.. * You might need to do lookup either way: what edges arrive here?  What
..   edges go away from this node?  And for each question, outline a
..   situation in which Contingent will need to answer a question.
.. * Talk about how we solve problems in Python: not with a Node class and an
..   Edge class and an OutgoingEdgesCollection class, but with simple
..   generic data structures.
.. * Do four bullet points with a sentence each: tuple, list, dict, set
.. * Discover that we should use a dict-of-sets pointing in each direction.
.. * Show __init__() and talk about defaultdict
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

.. There's probably more great points we can make about Python and our
.. wonderful API, but those are the ones that come to mind right now that I
.. wanted to get down before I forgot them. :)


How could we represent such a graph in Contingent?
Neither the core Python language nor the standard library
provide a direct equivalent to a graph data structure,
which isn't really surprising:
while a language like Python provides many useful data structures like
lists, sets, and dictionaries,
when solving problems we inevitably need structures or functions that
the language authors didn't put in.
Rather than trying to anticipate every possible need a user might
have — which is impossible anyway — good language designers try to
provide two things:

1. flexible, general purpose data structures and functions, and
2. language features that let users build entirely *new* functions and
   data structures that fit the problem at hand.

This is a lot of the fun and challenge of language design,
since different users have different opinions about what constitutes a
“flexible” system,
what features merit being called “general purpose,”
and what kinds of facilities the language should offer for extension.

Since Python doesn't contain exactly what we need,
we'll have to construct something ourselves.
At the core of Contingent is a ``Graph`` library,
which is just such an extension to Python
that gives us a convenient way to keep
all the nodes and edges of a graph organized.

>>> from contingent import graphlib
>>> g = graphlib.Graph()

A class like ``Graph`` defines a useful *abstraction*
that gives up some generality
in exchange for closer proximity to whatever problem we're working on.
``Graph`` allows us to write our program using
names drawn from the language of *graphs*,
rather than having to express everything
in terms of lists and dicts.
While they don't really matter to the computer,
good names are vitally important to the *programmer*,
and the ability to work with names that closely match the problem is an
important feature of modern programming languages.

When we create a new ``Graph`` object,
it won't have any nodes or edges:

>>> g.edges()
[]

Let's see if we can build the graph for our Sphinx example by hand.
First,
we need a way to add edges between nodes:

>>> g.add_edge('index.rst', 'index.html')
>>> g.add_edge('tutorial.rst', 'tutorial.html')
>>> g.add_edge('api.rst', 'api.html')

And just like that,
``g`` represents the same graph that we saw in Figure 1,

>>> from pprint import pprint
>>> pprint(g.edges())
[('api.rst', 'api.html'),
 ('index.rst', 'index.html'),
 ('tutorial.rst', 'tutorial.html')]

but, since it is a real live Python object and not just a pretty picture,
we can ask it interesting questions!
For example, when Contingent is building a blog from source files,
it will need to know things like “What depends on ``api.rst``?” when
the content of ``api.rst`` changes:

>>> g.immediate_consequences_of('api.rst')
['api.html']

``Graph`` is telling Contingent that,
when ``api.rst`` changes,
``api.html`` is now stale and must be rebuilt.
How about ``index.html``?

>>> g.immediate_consequences_of('index.html')
[]

Since ``index.html`` is at the end of the graph ­— it is what
graph folks call a *leaf node* — nothing depends on it and so nothing
needs to be rebuilt if it changes.

How can we implement a method like ``add_edges``?
Careful readers will have noticed that we added edges to our graph
without explicitly creating node and edge objects,
and that the nodes themselves are simply strings.
Coming from other languages and traditions,
one might have expected to see something more like::

    Graph g = new ConcreteGraph();
    Node indexRstNode = new StringNode("index.rst");
    Node indexHtmlNode = new StringNode("index.html");
    Edge indexEdge = new DirectedEdge(indexRstNode, indexHtmlNode);
    g.addEdge(indexEdge);

with user-defined classes and interfaces for everything in the system.
The Python language and community explicitly and intentionally emphasize
using simple, generic data structures to solve problems,
rather than creating custom classes for every minute detail
of the problem we want to tackle.
This is one facet of the notion of “Pythonic” solutions that you may
have read about: Pythonic solutions try to
minimize syntactic overhead
and leverage Python's powerful built-in tools
and extensive standard library.

What does this say about how should we build the ``Graph`` class?
The main purpose of ``Graph`` is to keep track of the relationships
between all the various nodes by maintaining a collection of all the
edges between them.
This will allow Contingent,
when looking at a node,
to determine which nodes are consequences of it.
We also need to be sure that two edges
pointing to something called ``index.html``
actually point to the same node.
But, since we want to push as much work as possible to Python itself,
we need to examine the facilitites that it offers
that might be of use to solve this problem.

Python provides a number of data structures that might work:

* ``tuples`` are immutable sequences of objects:
  duplicate object references are allowed but,
  once created,
  tuple instances cannot be changed.

* ``lists`` are mutable sequences of objects: new objects can be added
  and existing objects removed or rearranged.
  As with tuples,
  lists can have duplicate references to the same object.

* ``dicts`` are mutable mappings from one object (the “key”)
  to another object (the “value”).
  A typical use for dicts is to track collections of complex objects
  by a name or other convenient identifier.
  dict values can be duplicated but keys must be unique.

* ``sets`` are mutable, unordered collections of *unique* objects.

Looking at our requirements,
it seems like we need bits from both ``dict`` and ``set``:
tracking the consequences of a node in a ``set`` would make sure we
never duplicate edges,
while ``dict`` would give us a way to track the *adjacent* nodes
to each node in our graph.
This leads us to a dict-of-sets approach:
the dict's keys are the nodes in the graph,
while the values are the set of nodes adjacent to each key node:

>>> consequences_of = {
...     'index.rst': set(['index.html']),
...     'api.rst': set(['api.html']),
... }
>>> 'index.html' in consequences_of['index.rst']
True

If we happen to record a connection between two nodes more than once,
``set`` ensures that we remember that we've already seen
this edge and therefore don't need to record it again:

>>> consequences_of['api.rst'].add('api.html')
>>> consequences_of['api.rst'] == set(['api.html'])
True

``Graph`` maintains two of these dict-of-sets structures,
one recording the consequences of each node
and a second recording its inputs —
the nodes *it* depends on.
This duplication is a tradeoff of space for convenience:
while either data structure represents the entire graph unambiguously,
having both makes asking questions in either direction
a symmetric operation.

Here is ``Graph``\ 's constructor:

.. include:: contingent/graphlib.py
    :code: python
    :start-line: 15
    :end-line: 18

Whoa now, what's this ``defaultdict``?
It turns out that handling the “key doesn't exist yet” case is so common
that Python includes a special utility to make dealing with it easier.
With a normal ``dict``,
if you try to retrieve a key that does not exist you get a ``KeyError``:

>>> consequences_of = {}
>>> consequences_of['index.rst'].add('index.html')
Traceback (most recent call last):
     ...
KeyError: 'index.rst'

Rather than having to gum up your code with ugly special cases all over
the place, like this:

.. code:: python

    # ugly special case to handle “we haven't seen this task yet”
    if input_task not in self._consequences_of:
        self._consequences_of[input_task] = set()

    self._consequences_of[input_task].add(consequence_task)

``defaultdict`` simplifies things by allowing you to provide a
function that returns a value for absent keys.
In this case,
when we ask about an edge that the ``Graph`` hasn't yet seen,
we want to get back an empty ``set`` rather than an exception:

>>> from collections import defaultdict
>>> consequences_of = defaultdict(set)
>>> consequences_of['api.rst']
set()

Structuring our implementation this way means that,
for operations that need to work with this data structure,
the first-time case is identical to
the second-and-subsequent-times case:

>>> consequences_of['index.rst'].add('index.html')
>>> 'index.html' in consequences_of['index.rst']
True

..
 >>> from contingent.rendering import as_graphviz
 >>> open('figure1.dot', 'w').write(as_graphviz(g)) and None

And now we're ready to look at ``add_edge()``:

.. include:: contingent/graphlib.py
    :code: python
    :start-line: 36
    :end-line: 41

Adding an edge means updating both the
inputs and consequences structures;
here, adding a consequence of ``'tutorial.rst'``
also adds an input of ``'tutorial-title'``:

>>> g.add_edge('tutorial.rst', 'tutorial-title')
>>> g.inputs_of('tutorial-title')
['tutorial.rst']

Since we chose to represent the graph in both directions
we need to update ``_inputs_of`` and ``_consequences_of``
whenever we add a new edge.
This makes it easy to ask about either
a node's inputs or its consequences
but requires ``Graph`` to keep the two structures
carefully synchronized.
An alternative implementation might choose
to represent one side directly
and derive the other via computation,
thereby choosing to reduce space consumption by
increasing code complexity.
Unless your graph is enormous or your hardware peculiar, however,
you are unlikely to notice the performance difference;
you certainly *will* notice
if your code is awkward or complicated,
particularly if you stare at it at 2 in the morning.

Speaking of uncomplicated code,
notice how ``add_edge()`` doesn't know or care
whether either node has been seen before:
because the inputs and consequences data structures
are defined as ``defaultdict(set)``,
``add_edge()`` remains blissfully ignorant
as to the novelty of a node,
while ``defaultdict`` takes care of the difference
by creating new ``set`` objects on the fly as needed.
As we saw above, ``add_edge()`` would be
3 times as long without ``defaultdict``;
more importantly, it would be much more difficult
to read its purpose and behavior from the resulting code.
This implementation demonstrates a Pythonic
approach to problems: simple, direct, concise.

At this point you might be wondering:
“what's up with the ‘``_``’ at the beginning of the ``Graph``
attribute names ``_inputs_of`` and ``_consequences_of``?”
This is a Python convention —
a rule generally followed by the community
but not enforced by the language itself.
You can read it as a marker on the attribute indicating
“this attribute is an implementation detail; don't depend on it.”
Python's open philosophy and powerful introspection
mean that anyone will be able to see and manipulate these attributes:

>>> '_inputs_of' in dir(g)
True

This convention is one way the community has developed
to allow programmers to pass messages and warnings
through spacetime to each other.
Recognizing the need to signal differences among
public-facing, internal, and other types of object attributes,
the community adopted single leading underscores
as a way to provide
a concise and fairly consistent indicator
to other programmers,
including our future selves,
that the original authors intended these attributes
to be viewed as part of the internal machinery of the class.

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

>>> from contingent.projectlib import Project, pack_task
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

    >>> @task
    ... def read(filename):
    ...     return {'index.txt': index,
    ...             'tutorial.txt': tutorial,
    ...             'api.txt': api}[filename]

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

>>> project.start_tracing()
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
Python can't simply transfer control
from ``parse``'s code to ``read``'s,
since that would lose track of what we were doing
in ``parse`` when the call was made,
leaving our program wandering around forgetting everything
like Leonard from *Memento*.
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
that will let a program examine the stack,
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
To maintain the stack of tasks,
running a task needs to follow the sequence

1. push the task onto the stack
2. do its work, possibly calling other tasks
3. pop the task off the stack
4. return its result

every time a task is invoked.
Notice that if the task invokes another task at step 2,
the calling task will be the one at the top of the stack
when the call is made.

Here is the expanded ``read_task``,
using a simple Python list as the task stack:

>>> task_stack = []
>>> def read_task(filename):
...     task = pack_task(read_task, (filename,))
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
since it will in turn call ``read_task``
in the course of doing its work:

>>> def parse_task(filename):
...     task = pack_task(parse_task, (filename,))
...     task_stack.append(task)
...     print(task_stack)
...     text = read_task(filename).strip('\n')
...     title, body = text.split('\n', 1)
...     task_stack.pop()
...     return title, body

This time, invoking the task shows the stack at two points,
once inside ``parse_task`` and once in ``read_task``:

>>> parse_task('api.rst') and None
[parse_task('api.rst')]
[parse_task('api.rst'), read_task('api.rst')]

``Project``'s main purpose is to maintain a task stack like this,
and, as one task calls another,
to record the transaction in its consequences graph:

>>> task = read, ('tutorial.txt',)
>>> project.graph.immediate_consequences_of(task)
[parse('tutorial.txt')]

Of course writing all that task stack management code
in each task function would be
tedious, repetitive, error-prone, and repetitive,
so ``Project`` leverages another Python feature
to help out: *function decorators*.
A function decorator packages a function
inside another *wrapper* function,
allowing clean separation of responsibilities.
For our task decorator,
the wrapper worries about graph and task stack management —
of which the task function remains blissfully ignorant —
while each task function can focus on
the work needed to be done to perform the task.
Here is what the ``task`` decorator looks like:

.. code-block:: python

    class Project:
        # other stuff…

        def task(self, task_function):
            @wraps(task_function)
            def wrapper(*args):
                task = pack_task(wrapper, args)

                if self.task_stack:
                    self.graph.add_edge(task, self.task_stack[-1])

                self.task_stack.append(task)
                try:
                    value = task_function(*args)
                finally:
                    self.task_stack.pop()

                return value


Notice that it follows the pattern set out above:





Invoking each task could involve invoking other tasks,

>>> print(project.stop_tracing())
calling render('tutorial.txt')
. calling transform('tutorial.txt')
. . calling parse('tutorial.txt')
. . . calling read('tutorial.txt')

leading to a chain of consequences for each:

>>> project.graph.recursive_consequences_of([task])
[parse('tutorial.txt'), transform('tutorial.txt'), render('tutorial.txt')]










Okay: if we keep our edges up to date,
we will never again have the problem of rebuilding too little.

But how can the edges be kept up to date?

We can use wrappers plus a stack.

Yay!  Fundamental computer science like Debo wanted,
with a great chance to show how easily these are implemented in Python.
Explain how the stack we bulid and tear down
reflects exactly the way the real stack is growing and shrinking.

Time to illustrate.


So we have this decorator, which adds a wrapper.



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

>>> pprint(project.graph.tasks())
[parse('api.txt'),
 parse('index.txt'),
 parse('tutorial.txt'),
 read('api.txt'),
 read('index.txt'),
 read('tutorial.txt'),
 render('api.txt'),
 render('index.txt'),
 render('tutorial.txt'),
 title_of('api.txt'),
 title_of('tutorial.txt'),
 transform('api.txt'),
 transform('index.txt'),
 transform('tutorial.txt')]

..
 >>> open('figure4.dot', 'w').write(as_graphviz(project.graph)) and None


Again, with tracing!


.. image:: figure4.png

So as you can see by Figure 4, it has things figured out.
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
[parse('tutorial.txt'),
 title_of('tutorial.txt'),
 transform('api.txt'),
 render('api.txt'),
 transform('index.txt'),
 render('index.txt'),
 transform('tutorial.txt'),
 render('tutorial.txt')]

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
calling title_of('tutorial.txt')
calling transform('api.txt')
calling render('api.txt')
calling transform('index.txt')
calling render('index.txt')
calling transform('tutorial.txt')
calling render('tutorial.txt')


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
