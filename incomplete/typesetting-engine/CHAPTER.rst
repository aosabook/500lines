Typesetting engine
******************

A typesetting engine deals with the composition of text for visual display or printing. In the digital era we live in, the main challenge for achieving an aesthetically pleasant result is to figure out the best possible line breaks in a stream of text representing paragraphs.

To illustrate the need of a good algorithm for breaking paragraphs into lines, I will let the god speak:

.. image:: http://jill-jenn.net/metasentence.png
   :align: center

Our contribution takes a stream of paragraphs with titles and creates PostScript slides.

PostScript Slides
=================

For the rendering part, we use the page description language PostScript because of its simplicity. The main command that suits our needs is ``x y moveto (c) show``: it puts the character ``c`` at the position ``x`` ``y`` of the current slide.

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

Knowing this graph, it is easy to compute efficiently the shortest path, if we regard the demerits as distances. But here, the algorithm really is original because we compute the best path *while* constructing the directed acyclic graph (DAG). To be precise, we maintain a linked list of breakpoints that are part of the DAG.

Bonus Features
--------------

- Dealing with lines of different lengths (triangle-shaped or circle-shaped text)
- Fitness classes: tight / normal / loose / very loose, with extra demerits for switching between classes from one line to the next
- Possible adjustment of the final number of lines

Our Implementation
==================

The **raw text** becomes a sequence of **blocks** for which we have to figure out the best position of the **breakpoints** (typesetting part) so that we can **paint** all blocks on a slide (rendering part).

The *best* position is chosen according to a certain “score of badness” called **demerits**.

Block
-----

To represent blocks, the ``Block`` object needs the following attributes:

- the *type*: box, glue, penalty (represented by a ``Enum``);
- the corresponding *character* with its *width* according to the font metrics;
- the *stretch* and *shrink* parameters: 0 if it is a block or a penalty, a certain integer if it is a glue (there, ``SPACE_STRETCH`` and ``SPACE_SHRINK``);
- a ``penalty`` value if the block is a penalty: an amount of badness. For example, putting a breakpoint after a block corresponding to an hyphen is considered worse than putting it in place of a glue;
- a ``flag`` boolean: “Is this a flagged penalty?” Two consecutive breakpoints at flagged penalties get extra demerits. In Knuth's paper, all penalties are flagged ones. As of tradition, we kept it in the implementation.

All those attributes are defined at the beginning of the typesetting step and are not supposed to be modified. This is why we chose a ``namedtuple``, which is not mutable, instead of a new class.

Breakpoint
----------

Breakpoints are at the core of our main data structure: a linked list of nodes that are part of a DAG. The ``Breakpoint`` class has the following attributes:

- the *position* of this breakpoint in the sequence of blocks;
- the corresponding *line*: indeed, imagine a really narrow text where according to the choice of breakpoints, a certain word could be at the end of the first line or at the end of the second line, we need to take both scenarios into account;
- a *fitness* class which plays a role in the computation of demerits.
- values *total_width*, *total_stretch*, *total_shrink*, *total_demerits*, which contains the sum of those values for the blocks encountered so far (CHECK: is it right after, if it's a glue?);
- *previous*: the best previous breakpoint that leads to this breakpoint;
- *link*: the next node in the linked list.

Please keep in mind that *previous* and *link* are not symmetric. In the following graph, there is an arrow from ``A`` to ``B`` if ``B.previous = A`` but for example, if those breakpoints are part of the linked list, we have ``so.link = seen`` and ``seen.link = dark``.

.. image:: http://i.imgur.com/8YUCMeM.png
   :align: center

Computing the ratio
-------------------

If we want to justify the line between two breakpoints, we must compute the ratio of its spaces. In the best possible world, spaces need not be modified (``ratio = 0``). If the line is too short, spaces need to be stretched, all of the same amount ``ratio * SPACE_STRETCH`` (``ratio > 0``). If the line is too long, spaces need to be shrinked, so we need to add to each of them the negative value ``ratio * SPACE_SHRINK`` (``ratio < 0``).

Thus, knowing the value ``current_line_length - width`` and the number of spaces within the current line (actually, the sum of their shrink/stretch parameters), we can compute the ratio using a simple division. In order to get a pleasant result, ``abs(ratio)`` needs to be as low as possible. This is why the demerits depend of the ``ratio`` parameter.

Maintaining the linked list of breakpoints
------------------------------------------

Using the ratio and a few other parameters (fitness class), we can compute the increase of demerits between any two breakpoints. Now our goal is to find the best sequence of breakpoints i.e. the one achieving the lowest demerits. If we knew the complete list of breakpoints, we could compute that value by dynamic programming:::

    For each line
        For each breakpoint registered at this line
            Find the best previous breakpoint (the one achieving the lowest demerits)

But we don't. As we compute the demerits while adding breakpoints to our data structure, we need to use cunning.::

    For each block
        If this block is a possible breakpoint
            For each line
                For each breakpoint registered at this line in my linked list
                    Check if this breakpoint is the best previous breakpoint for the current block
                Add into the linked list a breakpoint for my current block registered at the next line, knowing its best previous breakpoint

Well, that's clever.

Those steps ensure that the linked list is sorted by line number, which is essential for our dynamic programming approach. (Those consecutive nested for loops are represented by nested while loops in the ``find_best_previous_breakpoints`` method.)

More optimizations are made so that the linked list has no obsolete breakpoints (i.e. achieving a ratio less than -1, which corresponds to an ugly shrink). The first nodes of the linked list are progressively pruned.

Painting
--------

As we know the width of each box, if we get the optimal sequence of breakpoints and the corresponding ratios, we can compute the position of each glue as well, thus paint all blocks at their right positions, so that the text can be justified.

Troubleshooting
===============

It was hard to find how to get the width of all characters of a given font. There were several ways to estimate them but we wanted to get the true value. Our final solution used the output of ``ttf2tfm`` (TeX font metric) as stated in ``compute_font_metrics.py``.

Further Extensions
==================

- Allow bold and italics (requires extra character width)
- Allow inclusion of code or graphics

References
==========

* Knuth-Plass - "Breaking Paragraphs into Lines"
* (Possibly other links related to EPUB readers or layout engines such as WebKit.)
