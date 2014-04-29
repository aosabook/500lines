# A Templating Engine

An important phase in any web application is generating HTML to be served to
the browser. These days, very few HTML pages are completely static, they
involve at least a small amount of dynamic data, such as the user's name.
Usually, they contain a great deal of dynamic data: product listings, friends'
news updates, and so on.

But at the same time, every HTML page contains large swaths of static text. And
these pages are large, containing tens of thousands of bytes of text. The
web application developer has a problem to solve: how best to generate a large
string containing a mix of static and dynamic data?  To add to the problem, the
static text is actually HTML markup that is authored by another member of the
team, the front-end designer, who wants to be able to work with it in familiar
ways.

One simple way to make the HTML would be to have large string constants in our
code, and join them together to produce the page.  Dynamic data would be
inserted with string substitution of some sort.  Some of our dynamic data is
repetitive, for example, lists of products, so we can't predict ahead of time
how many items will be included.  This means we'll have chunks that repeat, so
those would have to be handled separately and combined with the rest of the
page.

If we built our page this way, we'd have a mess on our hands.  The HTML would
be in multiple string constants embedded in our application code.  The logic
of how to loop over the dynamic data and combine it into the page would be
intermixed with other application logic.  Our front-end designer would need to
be able to edit your program in order to make HTML changes.  It quickly becomes
unworkable.

## Templating

The preferred way to produce HTML pages is templating.  The HTML page is
authored as a template, meaning that the file is mostly static HTML, with
dynamic pieces embedded in it with special notation.  

This is the opposite of how most programs work.  With most programming
languages, like Python, most of the source file is executable code, and if you
need literal static text, you embed it in a string literal:

```
def hello():
    print("Hello, world!")

hello()
```

When Python reads this source file, its initial state is that text like "def"
is meant to be interpreted as Python.  The double quote character indicates
that the following text is meant literally, until the closing double quote.
This is how most programming languages work, but with variations in the
specifics of the syntax.

A template language flips this around: the template file is mostly static
literal text, with notation to indicate the executable dynamic parts:

```
<html>
<body>
<p>Welcome, {{user_name}}!</p>
```

Here the text is meant to appear literally in the resulting HTML page, but the
{{ notation indicates a switch into dynamic mode, where the user_name variable
will be substituted into the output.

String formatting functions such as Python's `"foo = {foo}!".format(foo=17)`
are examples of mini-languages used to create strings from a literal and data
to be inserted.  Templates extend this idea to include logic constructs like
conditionals and loops, but are really just a difference of degree.

These files are called templates because they are used to produce many
different pages with similar structure but differing details.

To use HTML templates in our programs, we need a templating engine: a function
that will take a template and data to plug into the template, and will produce
a complete string of HTML.  This chapter describes a simple implementation of
a templating engine.

By the way, there's often nothing particular about HTML in a templating engine,
it could be used to produce any textual result.  But they are usually used for
HTML, and occasionally have HTML-specific features, such as escaping.


## Supported Syntax

Template engines vary in the syntax they support.  We'll implement a subset of
the Django syntax.  Since we are implementing our engine in Python, some Python
concepts will appear in our syntax.  

Data is inserted using double curly braces:

```
<p>Welcome, {{user_name}}!</p>
```

Templating engines usually provide access to elements within data using a
simplified and relaxed syntax.  You can use a dot to access object attributes,
or container elements, and if the resulting value is callable, it's
automatically called:

```
<p>The price is: {{product.price}}, with a {{product.discount}}% discount.</p>
```

Dots can be used multiple times on a single value to navigate down an attribute
or element chain.

You can also use helper functions, called filters, to modify values.  Filters
are invoked with a pipe character:

```
<p>Short name: {{story.subject|slugify|lower}}</p>
```

Building interesting pages usually requires at least a small amount of logic,
so conditionals are available:

```
{% if user.is_logged_in %}
    <p>Welcome, {{ user.name }}!</p>
{% else %}
    <p><a href="/login">Log in </a></p>
{% endif %}
```

Looping lets us include collections of data in our pages:

```
<p>Products:</p>
<ul>
{% for product in product_list %}
    <li>{{ product.name }}: {{ product.price|format_price }}</li>
{% endfor %}
</ul>
```

Conditionals and loops can of course be nested to build more complex pages.

Lastly, so that we can document our templates, comments appear between
brace-hashes:

```
{# This is the best template ever! #}
```

## What's Left Out

Full-featured template engines provide more features, of course.  To keep this
code small, we're leaving out interesting ideas like:

* Template inheritance and inclusion
* Custom tags
* Automatic escaping
* Arguments to filters
* Complex logic like elif and for/else
* Loops with more than one loop variable
* Whitespace control

Even so, our simple template engine is useful.  In fact, it is the template
engine used in coverage.py to produce its HTML reports.


## Implementation Approaches

In broad strokes, the template engine will have two main phases:

* Parse the template
* Execute the template to assemble the string result, which involves:
  - Manage the dynamic context
  - Execute the logic elements
  - Implement dot access and filter execution

A key decision in the implementation is what will be passed from the parsing
phase to the execution phase.  What does parsing produce that can be executed?
There are two main options here.  We'll call them interpretation and
compilation, using the terms loosely from other language implementations.

In an interpretation model, parsing produces a data structure representing the
structure of the template. The execution phase walks that data structure,
assembling the result string based on the instructions it found.

In a compilation model, parsing produces some form of executable code.  The
execution phases executes that code, producing the result.

Our implementation of the engine uses the compilation model.  We compile the
template into Python code.  When run, the Python code assembles the result.
If you are interested in seeing an implementation of the interpretation model,
an earlier version of this same code used interpretation, and is in the history
of the coverage.py repository on Bitbucket.  

For coverage.py's use case, there are only a few templates, and they are used
over and over to produce many files from the same template.  Overall, the
program ran faster if the templates were compiled to Python code, because even
though the compilation process was a bit more complicated, it only had to run
once, while the execution of the compiled code ran many times, and was faster
than interpreting a data structure many times.


## Compiling to Python

Before we get to the code of the template engine, let's look at what it
produces.  The parsing phase will convert a template into a Python function.
Here is a small template:

```
<p>Welcome, {{user_name}}!</p>
<p>Products:</p>
<ul>
{% for product in product_list %}
    <li>{{ product.name }}:
        {{ product.price|format_price }}</li>
{% endfor %}
</ul>
```

Our engine will compile this template to this Python code (slightly reformatted
for readability):

```
def render(ctx, dot):
    c_user_name = ctx['user_name']
    c_product_list = ctx['product_list']
    c_format_price = ctx['format_price']

    result = []
    a = result.append
    e = result.extend
    s = str

    e(['<p>Welcome, ', s(c_user_name), '!</p>\n<p>Products:</p>\n<ul>\n'])
    for c_product in c_product_list:
        e([
            '\n    <li>',
            s(dot(c_product, 'name')),
            ':\n        ',
            s(c_format_price(dot(c_product, 'price'))),
            '</li>\n'
        ])
    a('\n</ul>\n')
    return ''.join(result)
```

This Python code looks unusual, because we've chosen some shortcuts that
produce slightly faster code.  Each template is converted into a `render`
function that takes a dictionary of data called the context (abbreviated to
`ctx`). The body of the function starts by unpacking the data from the context
into local names, because they are faster for repeated use.  We use locals with
a `c_` prefix so that we can use other local names without fear of collisions.

The result of the template will be a string, but the fastest way to build a
string from parts is to create a list of strings, and join them together at the
end.  `result` will be the list of strings.  Because we're going to add strings
to this list, we capture its `append` and `extend` methods in the local names
`a` and `e`.  [[ Reviewers: should I explain more about this unusual use of
Python methods??]] The last local we create is a shorthand for the `str`
built-in.

With those preliminaries out of the way, we're ready for the Python lines
created from our particular template. Strings will be added to the result list
using the `a` or `e` shorthands, depending on whether we have one string to add
or more than one.  Literal text in the template becomes a simple string
literal.

Expressions in `{{ ... }}` are computed, converted to strings, and added to the
result.  Dots in the expression are handled by the `dot` function passed into
our function, because the meaning of the dotted expressions depends on the data
in the context: it could be attribute access or item access, and it could be a
callable.

The logical structures `{{ if ... }}` and `{{ for ... }}` are converted into
Python conditionals and loops in a relatively straightforward way.


<!-- [[[cog from cogutil import include ]]] -->
<!-- [[[end]]] -->

## Writing the Engine

Now that we understand what the engine will do, let's walk through the
implementation.

### CodeBuilder

The bulk of the work in our engine is parsing the template and producing the
necessary Python code.  To help with that, we have the `CodeBuilder` class,
which handles the bookkeeping for us as we add lines of code, manage
indentation, and finally get a dict of global values from the compiled Python.

A CodeBuilder object keeps a list of strings that will together be the final
Python code.  The only other state it needs is the current indentation level:

<!-- [[[cog include("templite.py", first="class CodeBuilder", numblanks=2) ]]] -->
```
class CodeBuilder(object):
    """Build source code conveniently."""

    def __init__(self, indent=0):
        self.code = []
        self.ident_level = indent
```
<!-- [[[end]]] -->

CodeBuilder is quite simple, it has:

* a method to add a new line of code, which automatically indents the text to
  the current indentation level, and supplies a newline:

<!-- [[[cog include("templite.py", first="def add_line", numblanks=3, dedent=False) ]]] -->
```
    def add_line(self, line):
        """Add a line of source to the code.

        Indentation and newline will be added for you, don't provide them.

        """
        self.code.extend([" " * self.ident_level, line, "\n"])
```
<!-- [[[end]]] -->

* two methods to increase or decrease the indentation level:

<!-- [[[cog include("templite.py", first="INDENT_STEP = 4", numblanks=3, dedent=False) ]]] -->
```
    INDENT_STEP = 4      # PEP8 says so!

    def indent(self):
        """Increase the current indent for following lines."""
        self.ident_level += self.INDENT_STEP

    def dedent(self):
        """Decrease the current indent for following lines."""
        self.ident_level -= self.INDENT_STEP
```
<!-- [[[end]]] -->

* a method to add a sub-builder.  This lets us keep a reference to a place in
  the code, and add text to it later.  The self.code list is mostly a list of
  strings, but will also hold references to these sub-builders:

<!-- [[[cog include("templite.py", first="def add_subbuilder", numblanks=1, dedent=False) ]]] -->
```
    def add_subbuilder(self):
        """Add a section, a sub-CodeBuilder."""
        sect = CodeBuilder(self.ident_level)
        self.code.append(sect)
        return sect
```
<!-- [[[end]]] -->

* a `__str__` method for producing a single string with all the code. This
  simply joins together all the strings in `self.code`.  Note that because
  `self.code` can contain sub-builders, this might call other `CodeBuilder`
  objects recursively:

<!-- [[[cog include("templite.py", first="def __str__", numblanks=1, dedent=False) ]]] -->
```
    def __str__(self):
        return "".join(str(c) for c in self.code)
```
<!-- [[[end]]] -->

* a method to produce the final vaues by executing the code.  This stringifies
  the object, executes it in a new globals namespace, and returns the resulting
  values:

<!-- [[[cog include("templite.py", first="def get_globals", numblanks=1, dedent=False) ]]] -->
```
    def get_globals(self):
        """Compile the code, and return a dict of globals it defines."""
        # A check that the caller really finished all the blocks they started.
        assert self.ident_level == 0
        # Get the Python source as a single string.
        python_source = str(self)
        # Execute the source, defining globals, and return them.
        global_namespace = {}
        exec(python_source, global_namespace)
        return global_namespace
```
<!-- [[[end]]] -->

Although we only use this class to produce one function, there's nothing here
that limits it to that use.  This makes the class simpler to implement, and
easier to understand.

[[much more to come...]]
