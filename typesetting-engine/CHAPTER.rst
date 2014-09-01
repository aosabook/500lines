Typesetting engine
******************

A typesetting engine deals with the composition of text for visual display or printing. In the digital era we live in, the main challenge for achieving an aesthetically pleasant result is to figure out the best possible line breaks in a stream of text representing paragraphs.

To illustrate the need of a good algorithm for breaking paragraphs into lines, I will let the god speak:
http://jill-jenn.net/metasentence.png

Our contribution takes a stream of paragraphs with titles and creates PostScript slides.

PostScript Slides
=================

For the rendering part, we use the page description language PostScript because of its simplicity. The main command that suits our needs is `x y moveto (c) show`: it puts the character `c` at the position `x` `y` of the current slide.

Line-Breaking Algorithm
=======================

This algorithm was described by Donald E. Knuth and Michael F. Plass in a 1981 paper entitled "Breaking Paragraphs into Lines".

Boxes, Glue and Penalties
-------------------------

Our text is composed of blocks of several types:
- characters of a certain size that can't be altered, such as letters and punctuation (boxes);
- spaces that can be stretched or shrinked of a certain amount (glue);
- and special characters such as hyphens or forced breaks (penalties).

Among these blocks, we need to identify at which positions we will insert *breakpoints*, within glues or after hyphens. A choice of breakpoints is thus a list of increasing positions within the list of blocks.

Between two consecutive breakpoints, if we know the size of each box, it is easy to compute the stretching/shrinking ratio of the spaces in order to justify the text. Basically, we want to minimize the deformation of the glue and use as few hyphens as possible: with low shrink/stretch ratio comes great readability. Each breakpoint will have a badness score called total of *demerits*. We thus want to solve the following optimisation problem: how to determine efficiently the breakpoint sequence achieving the fewest demerits?

Directed Acyclic Graph
----------------------

If we ask ourselves the question: “From a certain breakpoint, what are the next feasible breakpoints?”, we can imagine a graph where the nodes are the breakpoints and there is an edge between nodes A and B if B is a feasible breakpoint right after the breakpoint A. Please note that this graph is acyclic, because the nodes along a path have increasing positions.

Knowing this graph, it is easy to compute efficiently the shortest path, if we regard the demerits as distances. But here, the algorithm really is original because we compute the best path *while* constructing the DAG. To be precise, we maintain a linked list of breakpoints that are part of the DAG.

Bonus Features
--------------

- Dealing with lines of different lengths (triangle-shaped or circle-shaped text)
- Fitness classes: tight / normal / loose / very loose, with extra demerits for switching between classes from one line to the next
- Possible adjustment of the final number of lines

Our Implementation
==================

Block
-----

TODO explain namedtuple

Breakpoint
----------

- previous: the best previous node
- link: the next node in the linked list

Computing the ratio
-------------------

TODO math formula

Maintaining the linked list of breakpoints
------------------------------------------

Painting
--------

Troubleshooting
===============

It was hard to find how to get the width of all characters of a given font. There were several ways to estimate them but we wanted to get the true value. Our final solution used the output of `ttf2tfm` (TeX font metric) as stated in `compute_font_metrics.py`.

Further Extensions
==================

- Allow bold and italics (requires extra character width)
- Allow inclusion of code or graphics

References
==========

* Knuth-Plass - "Breaking Paragraphs into Lines"
* (Possibly other links related to EPUB readers or layout engines such as WebKit.)
