
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
and date.  But to keep our example simple, we will only consider that
the title of each blog post will appear on the page of the next one.
Like body, the title will need to appear on each post’s own page:

>>> g.add_edge('A.rst', 'A.title')
>>> g.add_edge('B.rst', 'B.title')
>>> g.add_edge('C.rst', 'C.title')
>>> g.add_edge('A.title', 'A.html')
>>> g.add_edge('B.title', 'B.html')
>>> g.add_edge('C.title', 'C.html')

But as part of the navigation each title will also appear on the HTML
page for the subsequent post:

>>> g.add_edge('A.title', 'B.html')
>>> g.add_edge('B.title', 'C.html')

Editing the markup source for any blog post but the last will now force
us to regenerate both that blog post and also the subsequent one, to
make sure that an edited title is correctly reflected in the next post’s
navigation:

>>> g.consequences_of('B.rst')
['B.body', 'B.title', 'B.html', 'C.html']

