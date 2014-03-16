Author: Carl Friedrich Bolz
Project: Object Models
Requirements: Python 2.7 or 3.3

This directory holds a simple imperative object-oriented object model
implementation, which is built up in stages. It is code as it could appear in
an interpreter for the language (except in Python and not in, say, C, to make
it more approachable). It is not meant to mirror any real language's model
directly, but some of the stages take up a lot of inspiration from Smalltalk
and Python.

The subdirectory hold increasingly complex versions. The chapter will explain
how the stages have to be changed to achieve various features and properties.
The goal is not to fully explain the entire model of a single language, but to
understand the design space of object-oriented imperative language design
better.

The stages are:

1 - a simple message-based model
2 - changing from message-based to attribute-based by introducing bound methods
3 - allowing more powerful customization: __get__, __getattr__, __setattr__
4 - introducing maps to store instances more efficiently

A note about size: just counting the lines of all the code gives a too large
result. However, there is a lot of shared code between the stages (particularly
in the tests) which would not have to be shown in the actual chapter.
There is a script countlines.py that shows an approximation of the correct
lines by counting the lines of the diffs.

There's another script diff.py that shows the diffs between the stages.
