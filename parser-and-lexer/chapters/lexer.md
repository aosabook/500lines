Lexing Python 3.1 
=================
A Python 3 program begins its life as a string of characters. Perhaps it is a single line, or perhaps it is distributed across thousands of files.

At some point, this string (the "source code" of your Python 3 program) is fed to another program whose job is to turn that string into instructions that a computer can run directly. There is some debate about what to call such programs. We will call them *compilers*.

In this chapter, we will implement the first phase of a Python 3 compiler. This phase is called *lexing*.

What is lexing?
---------------
To a computer, the source code of a Python 3 program is simply a string of characters. A human programmer sees things differently. Consider the following valid Python 3 program.

```python
42
```

A computer sees only two characters. A human sees a number.

Considering a more complicated example:

```python
cow = make_cow()
```

A computer sees sixteen normal characters. A human sees an identifier `cow`, some punctuation `=`, another identifier `make_cow`, and finally, the punctuation `(` and `)`.

In other words, there is a mismatch &mdash; where a computer sees the "atoms" of a Python 3 program as individual characters, a human will want to think of the program source as *meaningful groups of characters* &mdash; things like numbers, keywords, and punctuation.

These "meaningful groups of characters" are called *tokens*.

A lexer, in short, is a program that *takes the raw program source as input*, and *attempts to turn it into a stream of tokens*.

This idea of thinking of programs as a stream of tokens rather than a stream of characters is so important to programmers that every major editor allows you to define a color scheme for the source code. Such programs are called *syntax highlighters*, and it makes sense that they are usually implemented using lexers.


Why lex the program source at all?
----------------------------------
One application of lexical analysis is syntax highlighting. 