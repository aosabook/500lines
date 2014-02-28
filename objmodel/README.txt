Author: Carl Friedrich Bolz
Project: Object Models
Requirements: Python

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
