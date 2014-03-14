
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

>>> g.consequences_of('B.body')
['B.html']

Because a target might itself be the dependency for yet further targets,
``consequences_of`` needs to be a recursive operation that computes a
transitive closure over our dependency graph.  If the body of each blog
post is in fact imported from a Restructured Text file, for example,
then our graph will be two stories high:

>>> g.add_edge('A.rst', 'A.body')
>>> g.add_edge('B.rst', 'B.body')
>>> g.add_edge('C.rst', 'C.body')
>>> g.consequences_of('B.rst')
['B.body', 'B.html']

But when dealing with document collections like a Sphinx project or
statically generated blog, the dependency graph tends to be more
complicated.  For example, each blog post’s navigation might point the
way to the previous post in the blog’s history.  In real life, the
navigation would probably need at least the previous post’s title, URL,
and date.  But to keep our example simple, we will only consider the
title:

>>> g = Graph()

Since targets can themselves be the dependencies of yet further targets,
the ``consequences_of()`` operation in fact needs to be recursive.

