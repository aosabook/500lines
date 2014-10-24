Author: Darius Bacon
Project: AST to bytecode compiler
Requirements: Python

This will compile from Python 3 AST to Python 3.4 bytecode.

These seem the most relevant docs and helpers:

* http://docs.python.org/3.4/library/ast.html
* http://docs.python.org/3.4/library/dis.html
* http://alexleone.blogspot.com.ar/2010/01/python-ast-pretty-printer.html

We intend to connect these:

* http://blog.nullspace.io/obvious-python-parser.html
* https://github.com/akaptur/byterun

Priorities: in descending order, I want it to be

* Correct or fail-stop
* Self-hosting
* Clear
* Practical
* Complete

That is, it's more important to implement a subset of Python well,
supporting practical refinements like line-number info, than to
implement every feature of Python; that what's done be done clearly is
still more important; but the subset must be powerful enough to code
this compiler itself in; and input programs using any feature not
supported must provoke a clear error.

Why these priorities: There are many instructive toy compilers; the
first four qualities together would make this be more than a
toy. (Other goals could also work, but these are mine.) Subject to
these, more complete is better, but I expect to miss quite a
bit. Compiling a strong subset with good line number info, and such,
makes less of a toy than would leaving out that sort of thing while
filling in more corners of the language. Python is big.

That the data structure and methods used extend without change to
supporting the full language, I'm not going to worry
about. Something's gotta give.

To run:

    $ make
    $ python3.4 compiler.py your_source_filename.py
