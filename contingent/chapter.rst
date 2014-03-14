
Build systems maintain a directed graph.

>>> from graphlib import Graph
>>> g = Graph()

A system like ``make`` rebuilds an output file when one of its source
files appears to have been modified more recently.

>>> g.add_edge('A.md', 'A.html')
>>> g.add_edge('B.md', 'B.html')
>>> g.add_edge('C.md', 'C.html')

This makes it easy, if ``B.md`` is modified, to determine which output
file needs to be rebuilt.

>>> g.consequences_of('B.md')
{'B.html'}


