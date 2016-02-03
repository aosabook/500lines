Lexing Python 3.1 
=================
A Python 3 program begins its life as a string of characters. Perhaps it is a single line, or perhaps it is distributed across thousands of files.

At some point, this string (the "source code" of your Python 3 program) is fed to another program whose job is to turn that string into instructions that a computer can run directly. There is some debate about what to call such programs. We will call them *compilers*.

In this chapter, we will implement the first phase of a Python 3 compiler. This phase is called *lexing*.

What is a lexer?
---------------
To a computer, the source of a Python 3 program is an unstructured string of characters.

To a human, individual characters are a technicality. We see code as *meaningful groups of characters*:

<img src="../images/lexer-1.png" alt="Drawing" style="width: 400px;"/>

Humans prefer this because it abstracts away the mundane details of individual characters and frees us to think about program source as consisting of things like *keywords* and *identifiers*. We are trading the low-level abstraction that is hard to think about and manipulate for a higher level abstraction that is easier to think about and manipulate.

Because this abstraction is so natural, the first step of compilation is usually to transform the raw text of the source code into a stream of these "meaningful groups" characters, which are formally called **tokens.** A program that "tokenizes" the program source is called a **lexer**, or sometimes a **tokenizer.** More particularly, a lexer:

* Takes the program source as input
* Consumes every character of the source, and either throws it away, or groups it into a token
* Outputs all the tokens it finds in the source , or an error if the source can't be broken down into known tokens

As an example, here is the lexed output of the example program in the figure above:

```
(KEYWORD def)
(IDENTIFIER "print_cow")
(PUNCTUATION "(")
(PUNCTUATION ")")
(PUNCTUATION ":")
(IDENTIFIER "print")
(PUNCTUATION "(")
(LITERAL "cow")
(PUNCTUATION ")")
(NEWLINE)
```

Each token is tagged with its type, like `KEYWORD` or `IDENTIFIER`. We can see that the tokens end up being very much like what we'd imagined before, but with a couple of extra tokens added in, like `PUNCTUATION`.

An example of a program that can't be lexed is a program that has an unterminated string. For example:

```python
print "This string is unterminated! It has no close quote!
```


The lexical structure of Python 3
--------------



How do we do lexing?
--------------------




