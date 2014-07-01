
A Simple Object Model
======================


Introduction
----------------



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
