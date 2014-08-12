
A Simple Object Model
======================


Introduction
----------------

Object-oriented programming is one of the major programming paradigms in use
today, a lot of languages provide some form of object-orientation. While on the
surface the mechanisms that different object-oriented programming languages
provide to the programmer are very similar, the details can vary a lot.
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

The object model we will start out with is a majorly simplified version of that
of Smalltalk. It will have classes and instances of them, the ability to read
and write attributes into objects, the ability to send messages to objects and
the ability for a class to be a subclass of another classes.

The interface to these features is represented by a shared base class ``Base``
that defines a number of methods:

````python
class Base(object):
    """ The base class that all of the object model classes inherit from. """

    def __init__(self, cls):
        """ Every object has a class. """
        self.cls = cls

    def read_attr(self, fieldname):
        """ read field 'fieldname' out of the object """
        return self._read_dict(fieldname)

    def write_attr(self, fieldname, value):
        """ write field 'fieldname' into the object """
        self._write_dict(fieldname, value)

    def isinstance(self, cls):
        """ return True if the object is an instance of class cls """
        return self.cls.issubclass(cls)

    def send(self, methname, *args):
        """ send message 'methname' with arguments 'args' to object """
        meth = self.cls._read_from_class(methname)
        return meth(self, *args)

    def _read_dict(self, fieldname):
        """ read an field 'fieldname' out of the object's dict """
        return MISSING

    def _write_dict(self, fieldname, value):
        """ write a field 'fieldname' into the object's dict """
        raise AttributeError
````

The methods ``Base._read_dict`` and ``Base._write_dict`` are default
implementations that do not implement interesting behaviour. They need to be
overridden in the subclasses of ``Base``.

To start the implementation a good approach is to write a test to think about
what the to-be-implemented behaviour should be. All tests presented in this
chapter will always consist of two parts: A bit of completely regular Python
code defining and using a few classes, making use of increasingly advanced
features of the Python object model. The second half of each test is the
corresponding test using the object model we will implement in this chapter,
instead of normal Python classes.

XXX
one-to-one mapping, but using internal APIs
mapping would be done by eg the interpreter or a compiler
application level vs interpreter level

Let us start with a simple test for reading and writing object fields.

````python
def test_read_write_field():
    # Python code
    class A(object):
        pass
    obj = A()
    obj.a = 1
    assert obj.a == 1

    obj.b = 5
    assert obj.a == 1
    assert obj.b == 5

    obj.a = 2
    assert obj.a == 2
    assert obj.b == 5

    # Object model code
    A = Class("A", OBJECT, {}, TYPE)
    obj = Instance(A)
    obj.write_attr("a", 1)
    assert obj.read_attr("a") == 1

    obj.write_attr("b", 5)
    assert obj.read_attr("a") == 1
    assert obj.read_attr("b") == 5

    obj.write_attr("a", 2)
    assert obj.read_attr("a") == 2
    assert obj.read_attr("b") == 5
````

The test shows that we need two classes ``Class`` and ``Instance`` to represent
classes and instances in the object model respectively. Both of them should
inherit from ``Base``. The constructor of ``Instance`` just takes the class to
be instantiated, while the constructor of ``Class`` takes the name of the class,
the base class or None, the dictionary of the class and the metaclass (what the
metaclass is we will discuss a bit later).

Both instances and classes will use a dictionary to store their fields.
Therefore it makes sense to introduce a shared helper class for that part of
their behaviour (in theory that behaviour could also be part of ``Base``, but in
a later section it will become important that this is not the case):

````python
MISSING = object()

class BaseWithDict(Base):
    def __init__(self, cls, fields):
        Base.__init__(self, cls)
        self._fields = fields

    def _read_dict(self, fieldname):
        return self._fields.get(fieldname, MISSING)

    def _write_dict(self, fieldname, value):
        self._fields[fieldname] = value
````

Now we have enough scaffolding to define ``Instance`` and ``Class``. Both are
simply subclasses of ``BaseWithDict``. When constructing an ``Instance``, the
field dict is initialized as an empty dictionary. For classes, the fields are
passed into the constructor by the user of the object model.

````python
class Instance(BaseWithDict):
    """Instance of a user-defined class. """

    def __init__(self, cls):
        assert isinstance(cls, Class)
        BaseWithDict.__init__(self, cls, {})


class Class(BaseWithDict):
    """ A User-defined class. """

    def __init__(self, name, base_class, fields, metaclass):
        BaseWithDict.__init__(self, metaclass, fields)
        self.name = name
        self.base_class = base_class
````

Now we can also see what a metaclass is. Since classes are also a kind of
object, they inherit from ``Base``. Thus the class needs to be an instance of
another class, its metaclass.

Now our first test above almost passes. The only missing bit is the definition
of the base classes ``TYPE`` and ``OBJECT``, which are both instances of
``Class``. XXX describe that stuff

````python
# set up the base hierarchy like in Python (the ObjVLisp model)
# the ultimate base class is OBJECT
OBJECT = Class("object", None, {}, None)
# TYPE is a subclass of OBJECT
TYPE = Class("type", OBJECT, {}, None)
# TYPE is an instance of itself
TYPE.cls = TYPE
# OBJECT is an instance of TYPE
OBJECT.cls = TYPE
````

XXX
base class of application level type hierarchy: OBJECT (an instance of Class on
the implementation level)
base classes OBJECT and TYPE are intertwined
class of most classes: TYPE
TYPE inherits from OBJECT


Now the first test passes. A second test that is easy to write and immediately
passes is the following. It checks that reading and writing attributes works on
classes as well.

````python
def test_read_write_field_class():
    # classes are objects too
    # Python code
    class A(object):
        pass
    A.a = 1
    assert A.a == 1
    A.a = 6
    assert A.a == 6

    # Object model code
    A = Class("A", OBJECT, {}, TYPE)
    A.write_attr("a", 1)
    assert A.read_attr("a") == 1
    A.write_attr("a", 5)
    assert A.read_attr("a") == 5
````

Isinstance checking
++++++++++++++++++++++

So far the fact that objects have classes is not really made use of. So the next
test we are writing implements the ``isinstance`` machinery. The test for that
looks as follows:

````python
def test_isinstance():
    # Python code
    class A(object):
        pass
    class B(A):
        pass
    b = B()
    assert isinstance(b, B)
    assert isinstance(b, A)
    assert isinstance(b, object)
    assert not isinstance(b, type)

    # Object model code
    A = Class("A", OBJECT, {}, TYPE)
    B = Class("B", A, {}, TYPE)
    b = Instance(B)
    assert b.isinstance(B)
    assert b.isinstance(A)
    assert b.isinstance(OBJECT)
    assert not b.isinstance(TYPE)
````

To check whether an object ``obj`` is an instance of a certain class ``cls`` it
is enough to check whether ``cls`` is a superclass of the class of ``obj``.
Checking whether a class is a superclass of another class the chain of
superclasses of that class is walked. If and only if the other class is found in
that chain it is a superclass. The chain of superclasses of a class is also
called the "method resolution order" (mro) of that class. It can easily be
computed recursively:


````python
class Class(BaseWithDict):
    ...

    def mro(self):
        """ compute the mro (method resolution order) of the class """
        if self.base_class is None:
            return [self]
        else:
            return [self] + self.base_class.mro()

    def issubclass(self, cls):
        """ is self a subclass of cls? """
        return cls in self.mro()
````

With that code the test passes.


Message sending
++++++++++++++++++++

The remaining missing feature for this first version of the object model is the
ability to send messages to objects.

XXX polymorphism

````python
def test_send_simple():
    # Python code
    class A(object):
        def f(self):
            return self.x + 1
    obj = A()
    obj.x = 1
    assert obj.f() == 2

    class B(A):
        pass
    obj = B()
    obj.x = 1
    assert obj.f() == 2 # works on subclass too

    # Object model code
    def f(self):
        return self.read_attr("x") + 1
    A = Class("A", OBJECT, {"f": f}, TYPE)
    obj = Instance(A)
    obj.write_attr("x", 1)
    assert obj.send("f") == 2

    B = Class("B", A, {}, TYPE)
    obj = Instance(B)
    obj.write_attr("x", 2)
    assert obj.send("f") == 3
````

To find the correct implementation of a method that is sent to an object we walk
the method resolution order of the class of the object. The first method that is
found in the dictionary of one of the classes in the method resolution order is
then called. The code for that looks as follows:

````python
class Class(BaseWithDict):
    ...

    def _read_from_class(self, methname):
        for cls in self.mro():
            if methname in cls._fields:
                return cls._fields[methname]
        return MISSING

````

Together with the code for ``send`` in the ``Base`` implementation, this passes
the test.

To make sure that methods with arguments work as well, and that overriding of
methods is implemented correctly, we can use the following slightly more complex
test:

````python
def test_send_subclassing_and_arguments():
    # Python code
    class A(object):
        def g(self, arg):
            return self.x + arg
    obj = A()
    obj.x = 1
    assert obj.g(4) == 5

    class B(A):
        def g(self, arg):
            return self.x + arg * 2
    obj = B()
    obj.x = 4
    assert obj.g(4) == 12

    # Object model code
    def g_A(self, arg):
        return self.read_attr("x") + arg
    A = Class("A", OBJECT, {"g": g_A}, TYPE)
    obj = Instance(A)
    obj.write_attr("x", 1)
    assert obj.send("g", 4) == 5

    def g_B(self, arg):
        return self.read_attr("x") + arg * 2
    B = Class("B", A, {"g": g_B}, TYPE)
    obj = Instance(B)
    obj.write_attr("x", 4)
    assert obj.send("g", 4) == 12
````

XXX
send messages to objects
single inheritance, walk up inheritance chain to find method that implements message

note: application inheritance not modeled by implementation inheritance
implementation inheritance is purely an implementation shortcut

attributes stored freely in dictionaries




Attribute based model
----------------------

Now that we have the simplest version of our object model we can now think of
ways to change it. The first change that this section will introduce is the
distinction between a message-based model and an attribute-based model. This is
one of the core differences between Smalltalk, Ruby, Javascript on the one hand
and Python, Lua on the other hand. The message-based model has the sending of
messages as the primitive operation of program execution. The attribute-based
model splits up the sending of a message into two parts, looking up an attribute
and then calling the result. This difference can be shown in the following test:

````python
def test_bound_method():
    # Python code
    class A(object):
        def f(self, a):
            return self.x + a + 1
    obj = A()
    obj.x = 2
    m = obj.f
    assert m(4) == 7

    class B(A):
        pass
    obj = B()
    obj.x = 1
    m = obj.f
    assert m(10) == 12 # works on subclass too

    # Object model code
    def f(self, a):
        return self.read_attr("x") + a + 1
    A = Class("A", OBJECT, {"f": f}, TYPE)
    obj = Instance(A)
    obj.write_attr("x", 2)
    m = obj.read_attr("f")
    assert m(4) == 7

    B = Class("B", A, {}, TYPE)
    obj = Instance(B)
    obj.write_attr("x", 1)
    m = obj.read_attr("f")
    assert m(10) == 12
````

While the set up of the classes is the same as the corresponding test for
message sends, the way that the methods are called is different. First, the
attribute with the name of the method is looked up on the object. The result of
that lookup operation is a *bound method*, an object that encapsulates both the
object as well as the function found in the class. Then that bound method is
called with a call operation.

To implement this behaviour, we need to change the ``Base.read_attr``
implementation. If the attribute is not found in the dictionary, it is looked
for in the class. If it is found in the class, and the attribute is a callable,
it needs to be turned into a bound method. To emulate a bound method we simply
use a closure. In addition to changing ``Base.read_attr`` we can also change
``Base.send`` to use the new approach to message sending to make sure the
previous tests still pass.

````python
class Base(object):
    ...
    def read_attr(self, fieldname):
        """ read field 'fieldname' out of the object """
        result = self._read_dict(fieldname)
        if result is not MISSING:
            return result
        result = self.cls._read_from_class(fieldname)
        if callable(result):
            return _make_boundmethod(result, self)
        if result is MISSING:
            raise AttributeError(fieldname)
        return result

    def send(self, methname, *args):
        """ send message 'methname' with arguments 'args' to object """
        meth = self.read_attr(methname)
        return meth(*args)

def _make_boundmethod(meth, self):
    def bound(*args):
        return meth(self, *args)
    return bound

````

The rest of the code does not need to be changed at all.


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

- support for constructors and __new__
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
