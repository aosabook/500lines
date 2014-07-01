
A Simple Object Model
======================


Introduction
----------------

Object-oriented programming is one of the major programming paradigms in use
today, a lot of languages provide some form of object-orientation. While on the
surface the mechanisms that different object-oriented programming languages
provide the programmer with are very similar, the details can vary a lot.
Commonalities of most languages are the presence of objects and some kind of
inheritance mechanism. But already whether the language supports classes depends
on the precise language. For example in prototype based languages like Self or
Javascript the concept of class does not exist and instances instead inherit
directly from each other.

Understanding the differences between different object models can be
interesting. It often reveals the family resemblance between different
languages. It can be useful to quickly understand the model of a new language by
putting it into the context of the models of other languages and it can give a
better feeling of the programming language design space.

This chapter explores the implementation of a series of very simple object
models. It starts out with just having simple instances and classes and the
possibilities to send messages to instances. This is the "classical"
object-oriented approach that was started by early OO languages such as
Simula-67 and Smalltalk. This model is then extend step by step, the first three
steps exploring different language design choices, the last step to improve the
efficiency of the object model. The final model is not that of a real language,
but an idealized, simplified version of that of Python.

The object models presented in this chapter will also be implemented in Python.
To understand the behavior and the design choice better the chapter will also
present tests for the object model. The choice of Python as an implementation
language is quite unrealistic. A "real" VM is typically implemented in a
low-level language like C/C++ and needs a lot of additional attention to
engineering detail to make it efficient. However, the simpler implementation
language makes it easier to focus on actual behaviour differences instead of
getting bogged down by implementation details.



Message based model
----------------------

meta: test driven, tests show both "normal" Python behavior and same behavior
using the object model API.
one-to-one mapping, but using internal APIs
mapping would be done by eg the interpreter or a compiler
application level vs interpreter level

decomposition: a base class Base with the methods that all objects implement
a helper class BaseWithDict that is responsible for the dictionary (not so
useful now, but important in the last chapter)
two concrete classes Instance and Class
classes are objects too (Class inherits from Base)
base class of application level type hierarchy: OBJECT (an instance of Class on
the implementation level)

send messages to objects
single inheritance, walk up inheritance chain to find method that implements message

note: application inheritance not modeled by implementation inheritance
implementation inheritance is purely an implementation shortcut

attributes stored freely in dictionaries

base classes OBJECT and TYPE are intertwined
class of most classes: TYPE
TYPE inherits from OBJECT



Attribute based model
----------------------

- Big difference between Python and Ruby
- Primitive operation is reading an attribute, not sending a method 
- need a way to "bind" a method to an object
- binding is just done with a closure for now
- rest does not change much


Meta-object protocol
----------------------

- common approach in more dynamic languages
- pioneered in Lisp, Smalltalk, but common in most modern dynamically typed
  languages (Python, Ruby, Javascript, Lua, ...)
- Primitive operations overridable by user code
- hooks to modify how exactly the object machinery does things
- Often use normal inheritance of the meta-hooks to get the base behavior (ie
  OBJECT has a __setattr__ with the default behavior)


- concretely: override what reading and writing an attribute means (__getattr__
  and __setatttr__)
- override what binding a "method" means with __get__


Instance optimization
----------------------

- Small optimization: maps
- no behavior difference
- In chapter only memory size optimization
- In practice also huge speed optimization
- Observation: many instances share dictionary layout, thus very inefficient
- Better: explicitly share layout 
- Core technique in all high-performance dynamic language implementations such
  as Self, V8, PyPy


Potential Extensions
----------------------

- Distinction between implementation code and user code
- More meta methods
- Multiple inheritance (easy!)
- switch to prototype model


Conclusions
--------------

Writing an object model is easy and fun
one can embed this into another language (eg gobject for C, many others) and use
it there
can experiment with different language design without the "boring" parts of
language implementation (parsing and execution)
