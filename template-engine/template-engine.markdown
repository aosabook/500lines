title: A Template Engine
author: Ned Batchelder
<markdown>
_Ned Batchelder is a software engineer with a long career, currently working at
edX to build open source software to educate the world.  He's the maintainer of
coverage.py, an organizer of Boston Python, and has spoken at many PyCons.  He
blogs at [http://nedbatchelder.com](http://nedbatchelder.com). He once had
dinner at the White House._
</markdown>
## Introduction

Most programs contain a lot of logic, and a little bit of literal textual data.
Programming languages are designed to be good for this sort of programming.
But some programming tasks involve only a little bit of logic, and a great deal
of textual data.  For these tasks, we'd like to have a tool better suited to
these text-heavy problems.  A template engine is such a tool.  In this chapter,
we build a simple template engine.

The most common example of one of these text-heavy tasks is in web applications.
An important phase in any web application is generating HTML to be served to
the browser. Very few HTML pages are completely static: they
involve at least a small amount of dynamic data, such as the user's name.
Usually, they contain a great deal of dynamic data: product listings, friends'
news updates, and so on.

At the same time, every HTML page contains large swaths of static text. And
these pages are large, containing tens of thousands of bytes of text. The
web application developer has a problem to solve: how best to generate a large
string containing a mix of static and dynamic data?  To add to the problem, the
static text is actually HTML markup that is authored by another member of the
team, the front-end designer, who wants to be able to work with it in familiar
ways.

For purposes of illustration, let's imagine we want to produce this toy HTML:

```html
<p>Welcome, Charlie!</p>
<p>Products:</p>
<ul>
    <li>Apple: $1.00</li>
    <li>Fig: $1.50</li>
    <li>Pomegranate: $3.25</li>
</ul>
```

Here, the user's name will be dynamic, as will the names and prices of
the products.  Even the number of products isn't fixed: at another moment, there
could be more or fewer products to display.

One way to make this HTML would be to have string constants in our code,
and join them together to produce the page.  Dynamic data would be inserted
with string substitution of some sort.  Some of our dynamic data is repetitive,
like our lists of products.  This means we'll have chunks of HTML that repeat,
so those will have to be handled separately and combined with the rest of the
page.

Producing our toy page in this way might look like this:

```python
# The main HTML for the whole page.
PAGE_HTML = """
<p>Welcome, {name}!</p>
<p>Products:</p>
<ul>
{products}
</ul>
"""

# The HTML for each product displayed.
PRODUCT_HTML = "<li>{prodname}: {price}</li>\n"

def make_page(username, products):
    product_html = ""
    for prodname, price in products:
        product_html += PRODUCT_HTML.format(
            prodname=prodname, price=format_price(price))
    html = PAGE_HTML.format(name=username, products=product_html)
    return html
```

This works, but we have a mess on our hands.  The HTML is in multiple string
constants embedded in our application code.  The logic of the page is hard to
see because the static text is broken into separate pieces. The details of
how data is formatted is lost in the Python code.  In order to modify the HTML
page, our front-end designer would need to be able to edit Python code to make
HTML changes.  Imagine what the code would look like if the page were ten (or
one hundred) times more complicated; it would quickly become unworkable.


## Templates

The better way to produce HTML pages is with *templates*.  The HTML page is
authored as a template, meaning that the file is mostly static HTML, with
dynamic pieces embedded in it using special notation.  Our toy page above could
look like this as a template:

```html
<p>Welcome, {{user_name}}!</p>
<p>Products:</p>
<ul>
{% for product in product_list %}
    <li>{{ product.name }}:
        {{ product.price|format_price }}</li>
{% endfor %}
</ul>
```

Here the focus is on the HTML text, with logic embedded in the HTML.  Contrast
this document-centric approach with our logic-centric code above.
Our earlier program was mostly Python code, with HTML embedded in
the Python logic.  Here our program is mostly static HTML markup.

The mostly-static style used in templates is the opposite of how most
programming languages work.  For example, with Python, most of
the source file is executable code, and if you need literal static text, you
embed it in a string literal:

```python
def hello():
    print("Hello, world!")

hello()
```

When Python reads this source file, it interprets text like `def hello():` as
instructions to be executed.  The double quote character in
`print("Hello, world!")` indicates that the following text is meant literally,
until the closing double quote.  This is how most programming languages work:
mostly dynamic, with some static pieces embedded in the instructions.  The
static pieces are indicated by the double-quote notation.

A template language flips this around: the template file is mostly static
literal text, with special notation to indicate the executable dynamic parts.

```html
<p>Welcome, {{user_name}}!</p>
```

Here the text is meant to appear literally in the resulting HTML page, until the
'`{{`' indicates a switch into dynamic mode, where the `user_name` variable
will be substituted into the output.

String formatting functions such as Python's `"foo = {foo}!".format(foo=17)`
are examples of mini-languages used to create text from a string literal and the data
to be inserted.  Templates extend this idea to include constructs like
conditionals and loops, but the difference is only of degree.

These files are called templates because they are used to produce many
pages with similar structure but differing details.

To use HTML templates in our programs, we need a *template engine*: a function
that takes a static template describing the structure and static content of
the page, and a dynamic *context* that provides the dynamic data to plug into
the template.  The template engine combines the template and the context to
produce a complete string of HTML.  The job of a template engine is to
interpret the template, replacing the dynamic pieces with real data.

By the way, there's often nothing particular about HTML in a template engine,
it could be used to produce any textual result.  For example, they are also
used to produce plain-text email messages.  But usually they are used for
HTML, and occasionally have HTML-specific features, such as escaping, which
makes it possible to insert values into the HTML without worrying about which
characters are special in HTML.


## Supported Syntax

Template engines vary in the syntax they support.  Our template syntax is based
on Django, a popular web framework.  Since we are implementing our engine in
Python, some Python concepts will appear in our syntax. We've already seen some
of this syntax in our toy example at the top of the chapter, but this is a quick
summary of all of the syntax we'll implement.

Data from the context is inserted using double curly braces:

```html
<p>Welcome, {{user_name}}!</p>
```

The data available to the template is provided in the context when the template
is rendered. More on that later.

Template engines usually provide access to elements within data using a
simplified and relaxed syntax. In Python, these expressions all have different
effects:

```python
dict["key"]
obj.attr
obj.method()
```

In our template syntax, all of these operations are expressed with a dot:

```
dict.key
obj.attr
obj.method
```

The dot will access object attributes or dictionary values, and
if the resulting value is callable, it's automatically called.  This is
different than the Python code, where you need to use different syntax for
those operations. This results in simpler template syntax:

```html
<p>The price is: {{product.price}}, with a {{product.discount}}% discount.</p>
```

You can use functions called _filters_ to modify values.  Filters
are invoked with a pipe character:

```html
<p>Short name: {{story.subject|slugify|lower}}</p>
```

Building interesting pages usually requires at least a small amount of decision-making,
so conditionals are available:

```html
{% if user.is_logged_in %}
    <p>Welcome, {{ user.name }}!</p>
{% endif %}
```

Looping lets us include collections of data in our pages:

```html
<p>Products:</p>
<ul>
{% for product in product_list %}
    <li>{{ product.name }}: {{ product.price|format_price }}</li>
{% endfor %}
</ul>
```

As with other programming languages, conditionals and loops can be nested to
build complex logical structures.

Lastly, so that we can document our templates, comments appear between
brace-hashes:

```html
{# This is the best template ever! #}
```


## Implementation Approaches

In broad strokes, the template engine will have two main phases: _parsing_ the template, and then _rendering_ the template.

Rendering the template specifically involves:

* Managing the dynamic context, the source of the data
* Executing the logic elements
* Implementing dot access and filter execution

The question of what to pass from the parsing
phase to the rendering phase is key.  What does parsing produce that can be rendered?
There are two main options; we'll call them *interpretation* and
*compilation*, using the terms loosely from other language implementations.

In an interpretation model, parsing produces a data structure representing the
structure of the template. The rendering phase walks that data structure,
assembling the result text based on the instructions it finds.  For a
real-world example, the Django template engine uses this approach.

In a compilation model, parsing produces some form of directly executable code.
The rendering phase executes that code, producing the result.  Jinja2 and Mako
are two examples of template engines that use the compilation approach.

Our implementation of the engine uses compilation: we compile the template
into Python code.  When run, the Python code assembles the result.

The template engine described here was originally written as part of
coverage.py, to produce HTML reports.  In coverage.py, there are only a few
templates, and they are used over and over to produce many files from the same
template.  Overall, the program ran faster if the templates were compiled to
Python code, because even though the compilation process was a bit more
complicated, it only had to run once, while the execution of the compiled code
ran many times, and was faster than interpreting a data structure many times.

It's a bit more complicated to compile the template to Python, but it's not as
bad as you might think. And besides, as any developer can tell you,
it's more fun to write a program to write a program than it is to write a
program!

Our template compiler is a small example of a general technique called code
generation.  Code generation underlies many powerful and flexible tools,
including programming language compilers.  Code generation can get
complex, but is a useful technique to have in your toolbox.

Another application of templates might prefer the interpreted approach, if
templates will be used only a few times each.  Then the effort to compile to
Python won't pay off in the long run, and a simpler interpretation process
might perform better overall.


## Compiling to Python

Before we get to the code of the template engine, let's look at the code it
produces.  The parsing phase will convert a template into a Python function.
Here is our small template again:

```html
<p>Welcome, {{user_name}}!</p>
<p>Products:</p>
<ul>
{% for product in product_list %}
    <li>{{ product.name }}:
        {{ product.price|format_price }}</li>
{% endfor %}
</ul>
```

Our engine will compile this template to Python code.  The resulting Python
code looks unusual, because we've chosen some shortcuts that produce slightly
faster code.  Here is the Python (slightly reformatted for readability):

```python
def render_function(context, do_dots):
    c_user_name = context['user_name']
    c_product_list = context['product_list']
    c_format_price = context['format_price']

    result = []
    append_result = result.append
    extend_result = result.extend
    to_str = str

    extend_result([
        '<p>Welcome, ',
        to_str(c_user_name),
        '!</p>\n<p>Products:</p>\n<ul>\n'
    ])
    for c_product in c_product_list:
        extend_result([
            '\n    <li>',
            to_str(do_dots(c_product, 'name')),
            ':\n        ',
            to_str(c_format_price(do_dots(c_product, 'price'))),
            '</li>\n'
        ])
    append_result('\n</ul>\n')
    return ''.join(result)
```

Each template is converted into a `render_function` function that takes a
dictionary of data called the context.  The body of the function starts by
unpacking the data from the context into local names, because they are faster
for repeated use.  All the context data goes into locals with a `c_` prefix so
that we can use other local names without fear of collisions.

The result of the template will be a string. The fastest way to build a
string from parts is to create a list of strings, and join them together at the
end.  `result` will be the list of strings.  Because we're going to add strings
to this list, we capture its `append` and `extend` methods in the local names
`result_append` and `result_extend`.  The last local we create is a `to_str`
shorthand for the `str` built-in.

These kinds of shortcuts are unusual. Let's look at them more closely.  In
Python, a method call on an object like `result.append("hello")` is executed in
two steps.  First, the append attribute is fetched from the result object:
`result.append`.  Then the value fetched is invoked as a function, passing it
the argument `"hello"`.  Although we're used to seeing those steps performed
together, they really are separate. If you save the result of the first step,
you can perform the second step on the saved value.  So these two Python
snippets do the same thing:

```python
# The way we're used to seeing it:
result.append("hello")

# But this works the same:
append_result = result.append
append_result("hello")
```

In the template engine code, we've split it out this way so that we only do the
first step once, no matter how many times we do the second step.  This saves us
a small amount of time, because we avoid taking the time to look up the append attribute.

This is an example of a micro-optimization: an unusual coding technique that
gains us tiny improvements in speed.  Micro-optimizations can be less readable, or more
confusing, so they are only justified for code that is a proven performance
bottleneck.  Developers disagree on how much micro-optimization is justified,
and some beginners overdo it.  The optimizations here were added only after
timing experiments showed that they improved performance, even if only a little
bit.  Micro-optimizations can be instructive, as they make use of some exotic
aspects of Python, but don't over-use them in your own code.

The shortcut for `str` is also a micro-optimization. Names in Python can be
local to a function, global to a module, or built-in to Python.  Looking up a
local name is faster than looking up a global or a built-in.  We're used to the
fact that `str` is a builtin that is always available, but Python still has to
look up the name `str` each time it is used.  Putting it in a local saves us
another small slice of time because locals are faster than builtins.

Once those shortcuts are defined, we're ready for the Python lines created from
our particular template. Strings will be added to the result list using the
`append_result` or `extend_result` shorthands, depending on whether we have one
string to add, or more than one.  Literal text in the template becomes a simple
string literal.

Having both append and extend adds complexity, but remember we're aiming
for the fastest execution of the template, and using extend for one item means
making a new list of one item so that we can pass it to extend.

Expressions in `{{ ... }}` are computed, converted to strings, and added to the
result.  Dots in the expression are handled by the `do_dots` function passed
into our function, because the meaning of the dotted expressions depends on the
data in the context: it could be attribute access or item access, and it could
be a callable. 

The logical structures `{% if ... %}` and `{% for ... %}` are converted into
Python conditionals and loops.  The expression in the `{% if/for ... %}` tag
will become the expression in the `if` or `for` statement, and the contents up
until the `{% end... %}` tag will become the body of the statement.


<!-- [[[cog from cogutil import include ]]] -->
<!-- [[[end]]] -->


## Writing the Engine

Now that we understand what the engine will do, let's walk through the
implementation.


### The Templite class

The heart of the template engine is the Templite class.  (Get it? It's a
template, but it's lite!)

The Templite class has a small interface.  You construct a Templite object with the
text of the template, then later you can use the `render` method on it to
render a particular context, the dictionary of data, through the template:

```python
# Make a Templite object.
templite = Templite('''
    <h1>Hello {{name|upper}}!</h1>
    {% for topic in topics %}
        <p>You are interested in {{topic}}.</p>
    {% endfor %}
    ''',
    {'upper': str.upper},
)

# Later, use it to render some data.
text = templite.render({
    'name': "Ned",
    'topics': ['Python', 'Geometry', 'Juggling'],
})
```

We pass the text of the template when the object is created so that we can
do the compile step just once, and later call `render` many times to reuse the
compiled results.

The constructor also accepts a dictionary of values, an initial context. These
are stored in the Templite object, and will be available when the template is
later rendered.  These are good for defining functions or constants we want to
be available everywhere, like `upper` in the previous example.

Before we discuss the implementation of Templite, we have a helper to define
first: CodeBuilder.


### CodeBuilder

The bulk of the work in our engine is parsing the template and producing the
necessary Python code.  To help with producing the Python, we have the
CodeBuilder class, which handles the bookkeeping for us as we construct 
the Python code.  It adds lines of code, manages indentation, and
finally gives us values from the compiled Python.

One CodeBuilder object is responsible for a complete chunk of Python code. As
used by our template engine, the chunk of Python is always a single complete
function definition. But the CodeBuilder class makes no assumption that it will
only be one function.  This keeps the CodeBuilder code more general, and less
coupled to the rest of the template engine code.

As we'll see, we also use nested CodeBuilders to make it possible to put code
at the beginning of the function even though we don't know what it will be
until we are nearly done.

A CodeBuilder object keeps a list of strings that will together be the final
Python code.  The only other state it needs is the current indentation level:

<!-- [[[cog include("templite.py", first="class CodeBuilder", numblanks=2) ]]] -->
```python
class CodeBuilder(object):
    """Build source code conveniently."""

    def __init__(self, indent=0):
        self.code = []
        self.indent_level = indent
```
<!-- [[[end]]] -->

CodeBuilder doesn't do much. `add_line` adds a new line of code, which
automatically indents the text to the current indentation level, and supplies a
newline:

<!-- [[[cog include("templite.py", first="def add_line", numblanks=3, dedent=False) ]]] -->
```python
    def add_line(self, line):
        """Add a line of source to the code.

        Indentation and newline will be added for you, don't provide them.

        """
        self.code.extend([" " * self.indent_level, line, "\n"])
```
<!-- [[[end]]] -->

`indent` and `dedent` increase or decrease the indentation level:

<!-- [[[cog include("templite.py", first="INDENT_STEP = 4", numblanks=3, dedent=False) ]]] -->
```python
    INDENT_STEP = 4      # PEP8 says so!

    def indent(self):
        """Increase the current indent for following lines."""
        self.indent_level += self.INDENT_STEP

    def dedent(self):
        """Decrease the current indent for following lines."""
        self.indent_level -= self.INDENT_STEP
```
<!-- [[[end]]] -->

`add_section` is managed by another CodeBuilder object.  This lets us
keep a reference to a place in the code, and add text to it later. The
`self.code` list is mostly a list of strings, but will also hold references to
these sections:

<!-- [[[cog include("templite.py", first="def add_section", numblanks=1, dedent=False) ]]] -->
```python
    def add_section(self):
        """Add a section, a sub-CodeBuilder."""
        section = CodeBuilder(self.indent_level)
        self.code.append(section)
        return section
```
<!-- [[[end]]] -->

`__str__` produces a single string with all the code. This
simply joins together all the strings in `self.code`.  Note that because
`self.code` can contain sections, this might call other `CodeBuilder`
objects recursively:

<!-- [[[cog include("templite.py", first="def __str__", numblanks=1, dedent=False) ]]] -->
```python
    def __str__(self):
        return "".join(str(c) for c in self.code)
```
<!-- [[[end]]] -->

`get_globals` yields the final values by executing the code.  This stringifies
the object, executes it to get its definitions, and returns the resulting
values:

<!-- [[[cog include("templite.py", first="def get_globals", numblanks=1, dedent=False) ]]] -->
```python
    def get_globals(self):
        """Execute the code, and return a dict of globals it defines."""
        # A check that the caller really finished all the blocks they started.
        assert self.indent_level == 0
        # Get the Python source as a single string.
        python_source = str(self)
        # Execute the source, defining globals, and return them.
        global_namespace = {}
        exec(python_source, global_namespace)
        return global_namespace
```
<!-- [[[end]]] -->

This last method uses some exotic features of Python.  The `exec` function
executes a string containing Python code.  The second argument to `exec` is
a dictionary that will collect up the globals defined by the code.  So for
example, if we do this:

```python
python_source = """\
SEVENTEEN = 17

def three():
    return 3
"""
global_namespace = {}
exec(python_source, global_namespace)
```

then `global_namespace['SEVENTEEN']` is 17, and `global_namespace['three']` is
an actual function named `three`.

Although we only use CodeBuilder to produce one function, there's nothing here
that limits it to that use.  This makes the class simpler to implement, and
easier to understand.

CodeBuilder lets us create a chunk of Python source code, and has no specific
knowledge about our template engine at all.  We could use it in such a way that
three different functions would be defined in the Python, and then `get_globals`
would return a dict of three values, the three functions.  As it happens, our
template engine only needs to define one function.  But it's better software
design to keep that implementation detail in the template engine code, and out
of our CodeBuilder class.

Even as we're actually using it&mdash;to define a single function&mdash;having `get_globals`
return the dictionary keeps the code more modular because it doesn't need to
know the name of the function we've defined.  Whatever function name we define
in our Python source, we can retrieve that name from the dict returned by
`get_globals`.

Now we can get into the implementation of the Templite class itself, and see
how and where CodeBuilder is used.


### The Templite class implementation

Most of our code is in the Templite class.  As we've discussed, it has both a compilation and a rendering phase.


#### Compiling

All of the work to compile the template into a Python function happens in the
Templite constructor.  First the contexts are saved away:

<!-- [[[cog include("templite.py", first="def __init__(self, text, ", numblanks=3, dedent=False) ]]] -->
```python
    def __init__(self, text, *contexts):
        """Construct a Templite with the given `text`.

        `contexts` are dictionaries of values to use for future renderings.
        These are good for filters and global values.

        """
        self.context = {}
        for context in contexts:
            self.context.update(context)
```
<!-- [[[end]]] -->

Notice we used `*contexts` as the parameter. The asterisk denotes that any
number of positional arguments will be packed into a tuple and passed in as
`contexts`.  This is called argument unpacking, and means that the caller can
provide a number of different context dictionaries.  Now any of these calls are
valid:

```python
t = Templite(template_text)
t = Templite(template_text, context1)
t = Templite(template_text, context1, context2)
```

The context arguments (if any) are supplied to the constructor as a tuple of
contexts.  We can then iterate over the `contexts` tuple, dealing with each of
them in turn.  We simply create one combined dictionary called `self.context`
which has the contents of all of the supplied contexts.  If duplicate names are
provided in the contexts, the last one wins.

To make our compiled function as fast as possible, we extract context variables
into Python locals.  We'll get those names by keeping a set of variable names
we encounter, but we also need to track the names of variables defined in the
template, the loop variables:

<!-- [[[cog include("templite.py", first="self.all_vars", numblanks=1, dedent=False) ]]] -->
```python
        self.all_vars = set()
        self.loop_vars = set()
```
<!-- [[[end]]] -->

Later we'll see how these get used to help construct the prologue of our
function. First, we'll use the CodeBuilder class we wrote earlier to start to
build our compiled function:

<!-- [[[cog include("templite.py", first="code = CodeBuilder", numblanks=2, dedent=False) ]]] -->
```python
        code = CodeBuilder()

        code.add_line("def render_function(context, do_dots):")
        code.indent()
        vars_code = code.add_section()
        code.add_line("result = []")
        code.add_line("append_result = result.append")
        code.add_line("extend_result = result.extend")
        code.add_line("to_str = str")
```
<!-- [[[end]]] -->

Here we construct our CodeBuilder object, and start writing lines into it. Our
Python function will be called `render_function`, and will take two arguments:
`context` is the data dictionary it should use, and `do_dots` is a function
implementing dot attribute access.

The context here is the combination of the data context passed to the Templite
constructor, and the data context passed to the render function.  It's the complete
set of data available to the template that we made in the Templite constructor.

Notice that CodeBuilder is very simple: it doesn't "know" about function
definitions, just lines of code.  This keeps CodeBuilder simple, both in its
implementation, and in its use.  We can read our generated code here without
having to mentally interpolate too many specialized CodeBuilder.

We create a section called `vars_code`.  Later we'll write the variable
extraction lines into that section.  The `vars_code` object lets us save a
place in the function that can be filled in later when we have the information
we need.

Then four fixed lines are written, defining a result list, shortcuts for the
methods to append to or extend that list, and a shortcut for the `str()`
builtin.  As we discussed earlier, this odd step squeezes just a little bit
more performance out of our rendering function.

The reason we have both the `append` and the `extend` shortcut is so we can
use the most effective method, depending on whether we have one line to add to
our result, or more than one.

Next we define an inner function to help us with buffering output strings:

<!-- [[[cog include("templite.py", first="buffered =", numblanks=1, dedent=False) ]]] -->
```python
        buffered = []
        def flush_output():
            """Force `buffered` to the code builder."""
            if len(buffered) == 1:
                code.add_line("append_result(%s)" % buffered[0])
            elif len(buffered) > 1:
                code.add_line("extend_result([%s])" % ", ".join(buffered))
            del buffered[:]
```
<!-- [[[end]]] -->

As we create chunks of output that need to go into our compiled function, we need
to turn them into function calls that append to our result.  We'd like to
combine repeated append calls into one extend call.  This is another micro-optimization.
To make this possible, we buffer the chunks.

The `buffered` list holds strings that are yet to be written to our function
source code.  As our template compilation proceeds, we'll append strings to
`buffered`, and flush them to the function source when we reach control flow
points, like if statements, or the beginning or ends of loops.

The `flush_output` function is a *closure*, which is a fancy word for a function
that refers to variables outside of itself. Here `flush_output` refers to
`buffered` and `code`. This simplifies our calls to the function:  we don't
have to tell `flush_output` what buffer to flush, or where to flush it; it
knows all that implicitly.

If only one string has been buffered, then the `append_result` shortcut is used
to append it to the result. If more than one is buffered, then the 
`extend_result` shortcut is used, with all of them,
to add them to the result.
Then the buffered list is cleared so more strings can be buffered.

The rest of the compiling code will add lines to the function by appending them
to `buffered`, and eventually call `flush_output` to write them to the
CodeBuilder.

With this function in place, we can have a line of code in our compiler like
this:

```python
buffered.append("'hello'")
```

\noindent which will mean that our compiled Python function will have this line:

```python
append_result('hello')
```

\noindent which will add the string `hello` to the rendered output of the template. We have multiple levels of abstraction here which can be difficult to keep straight. The compiler uses \newline `buffered.append("'hello'")`, which creates `append_result('hello')` in the compiled Python function, which when run, appends `hello` to the template result.

Back to our Templite class. As we parse control structures, we want to check
that they are properly nested.  The `ops_stack` list is a stack of strings:

<!-- [[[cog include("templite.py", first="ops_stack", numblanks=1, dedent=False) ]]] -->
```python
        ops_stack = []
```
<!-- [[[end]]] -->

When we encounter an `{% if .. %}` tag (for example), we'll push `'if'` onto
the stack.  When we find an `{% endif %}` tag, we can pop the stack and report
an error if there was no `'if'` at the top of the stack.

Now the real parsing begins.  We split the template text into a number of
tokens using a regular expression, or *regex*.  Regexes can be
daunting: they are a very compact notation for complex pattern matching.  They
are also very efficient, since the complexity of matching the pattern is
implemented in C in the regular expression engine, rather than in your own
Python code.  Here's our regex:

<!-- [[[cog include("templite.py", first="tokens =", numblanks=1, dedent=False) ]]] -->
```python
        tokens = re.split(r"(?s)({{.*?}}|{%.*?%}|{#.*?#})", text)
```
<!-- [[[end]]] -->

This looks complicated; let's break it down.  

The `re.split` function will
split a string using a regex.  Our pattern is parenthesized,
so the matches will be used to split the string, and will also be returned as
pieces in the split list.  Our pattern will match our tag syntaxes, but we've
parenthesized it so that the string will be split at the tags, and the tags
will also be returned.

The `(?s)` flag in the regex means that a dot should match even a newline. Next
we have our parenthesized group of three alternatives: `{{.*?}}` matches an
expression, `{%.*?%}` matches a tag, and `{#.*?#}` matches a comment.  In all
of these, we use `.*?` to match any number of characters, but the shortest
sequence that matches.

The result of `re.split` is a list of strings.  For example, this template text:

```html
<p>Topics for {{name}}: {% for t in topics %}{{t}}, {% endfor %}</p>
```

would be split into these pieces:

```python
[
    '<p>Topics for ',               # literal
    '{{name}}',                     # expression
    ': ',                           # literal
    '{% for t in topics %}',        # tag
    '',                             # literal (empty)
    '{{t}}',                        # expression
    ', ',                           # literal
    '{% endfor %}',                 # tag
    '</p>'                          # literal
]
```

Once the text is split into tokens like this, we can loop over the tokens, and
deal with each in turn.  By splitting them according to their type, we can
handle each type separately.

The compilation code is a loop over these tokens:

<!-- [[[cog include("templite.py", first="for token", numlines=1, dedent=False) ]]] -->
```python
        for token in tokens:
```
<!-- [[[end]]] -->

Each token is examined to see which of the four cases it is.  Just looking at
the first two characters is enough.  The first case is a comment, which is easy
to handle: just ignore it and move on to the next token:

<!-- [[[cog include("templite.py", first="if token.", numlines=3, dedent=False) ]]] -->
```python
            if token.startswith('{#'):
                # Comment: ignore it and move on.
                continue
```
<!-- [[[end]]] -->

For the case of `{{...}}` expressions, we cut off the two braces at the front
and back, strip off the white space, and pass the entire expression to
`_expr_code`:

<!-- [[[cog include("templite.py", first="elif token.startswith('{{')", numlines=4, dedent=False) ]]] -->
```python
            elif token.startswith('{{'):
                # An expression to evaluate.
                expr = self._expr_code(token[2:-2].strip())
                buffered.append("to_str(%s)" % expr)
```
<!-- [[[end]]] -->

The `_expr_code` method will compile the template expression into a Python
expression.  We'll see that function later.  We use the `to_str` function to
force the expression's value to be a string, and add that to our result.

The third case is the big one: `{% ... %}` tags.  These are control structures
that will become Python control structures.  First we have to flush our
buffered output lines, then we extract a list of words from the tag:

<!-- [[[cog include("templite.py", first="elif token.startswith('{%')", numlines=4, dedent=False) ]]] -->
```python
            elif token.startswith('{%'):
                # Action tag: split into words and parse further.
                flush_output()
                words = token[2:-2].strip().split()
```
<!-- [[[end]]] -->

Now we have three sub-cases, based on the first word in the tag: `if`, `for`,
or `end`.  The `if` case shows our simple error handling and code generation:

<!-- [[[cog include("templite.py", first="if words[0] == 'if'", numlines=7, dedent=False) ]]] -->
```python
                if words[0] == 'if':
                    # An if statement: evaluate the expression to determine if.
                    if len(words) != 2:
                        self._syntax_error("Don't understand if", token)
                    ops_stack.append('if')
                    code.add_line("if %s:" % self._expr_code(words[1]))
                    code.indent()
```
<!-- [[[end]]] -->

The `if` tag should have a single expression, so the `words` list should have
only two elements in it.  If it doesn't, we use the `_syntax_error` helper
method to raise a syntax error exception.  We push `'if'` onto `ops_stack` so
that we can check the `endif` tag.  The expression part of the `if` tag is compiled
to a Python expression with `_expr_code`, and is used as the conditional
expression in a Python `if` statement.

The second tag type is `for`, which will be compiled to a Python `for` statement:

<!-- [[[cog include("templite.py", first="elif words[0] == 'for'", numlines=13, dedent=False) ]]] -->
```python
                elif words[0] == 'for':
                    # A loop: iterate over expression result.
                    if len(words) != 4 or words[2] != 'in':
                        self._syntax_error("Don't understand for", token)
                    ops_stack.append('for')
                    self._variable(words[1], self.loop_vars)
                    code.add_line(
                        "for c_%s in %s:" % (
                            words[1],
                            self._expr_code(words[3])
                        )
                    )
                    code.indent()
```
<!-- [[[end]]] -->

We do a check of the syntax and push `'for'` onto the stack.  The
`_variable` method checks the syntax of the variable, and adds it to the set
we provide.  This is how we collect up the names of all the variables
during compilation. Later we'll need to write the prologue of our function,
where we'll unpack all the variable names we get from the context.  To do that
correctly, we need to know the names of all the variables we encountered,
`self.all_vars`, and the names of all the variables defined by loops, `self.loop_vars`.

We add one line to our function source, a `for` statement.  All of our template
variables are turned into Python variables by prepending `c_` to them, so that
we know they won't collide with other names we're using in our Python function.
We use `_expr_code` to compile the iteration expression from the template into
an iteration expression in Python.

The last kind of tag we handle is an `end` tag; either `{% endif %}` or
`{% endfor %}`.  The effect on our compiled function source is the same: simply
unindent to end the `if` or `for` statement that was started earlier:

<!-- [[[cog include("templite.py", first="elif words[0].startswith('end')", numlines=11, dedent=False) ]]] -->
```python
                elif words[0].startswith('end'):
                    # Endsomething.  Pop the ops stack.
                    if len(words) != 1:
                        self._syntax_error("Don't understand end", token)
                    end_what = words[0][3:]
                    if not ops_stack:
                        self._syntax_error("Too many ends", token)
                    start_what = ops_stack.pop()
                    if start_what != end_what:
                        self._syntax_error("Mismatched end tag", end_what)
                    code.dedent()
```
<!-- [[[end]]] -->

Notice here that the actual work needed for the end tag is one line: unindent the
function source.  The rest of this clause is all error checking to make sure
that the template is properly formed.  This isn't unusual in program
translation code.

Speaking of error handling, if the tag isn't an `if`, a `for`, or an `end`, then
we don't know what it is, so raise a syntax error:

<!-- [[[cog include("templite.py", first="else:", numlines=2, dedent=False) ]]] -->
```python
                else:
                    self._syntax_error("Don't understand tag", words[0])
```
<!-- [[[end]]] -->

We're done with the three different special syntaxes (`{{...}}`, `{#...#}`, and
`{%...%}`). What's left is literal content.  We'll add the literal string to
the buffered output, using the `repr` built-in function to produce a Python
string literal for the token:

<!-- [[[cog include("templite.py", first="else:", after="Don't understand tag", numblanks=1, dedent=False) ]]] -->
```python
            else:
                # Literal content.  If it isn't empty, output it.
                if token:
                    buffered.append(repr(token))
```
<!-- [[[end]]] -->

If we didn't use `repr`, then we'd end up with lines like this in our compiled
function:

```python
append_result(abc)      # Error! abc isn't defined
```

We need the value to be quoted like this:

```python
append_result('abc')
```

The `repr` function supplies the quotes around the string for us, and also
provides backslashes where needed:

```python
append_result('"Don\'t you like my hat?" he asked.')
```

Notice that we first check if the token is an empty string with `if token:`,
since there's no point adding an empty string to the output. Because our regex
is splitting on tag syntax, adjacent tags will have an empty token between
them.  The check here is an easy way to avoid putting useless
`append_result("")` statements into our compiled function.

That completes the loop over all the tokens in the template.  When the loop is
done, all of the template has been processed.  We have one last check to make:
if `ops_stack` isn't empty, then we must be missing an end tag.  Then we flush
the buffered output to the function source:

<!-- [[[cog include("templite.py", first="if ops_stack:", numblanks=2, dedent=False) ]]] -->
```python
        if ops_stack:
            self._syntax_error("Unmatched action tag", ops_stack[-1])

        flush_output()
```
<!-- [[[end]]] -->

We had created a section at the beginning of the function.  Its role was to
unpack template variables from the context into Python locals.  Now that we've
processed the entire template, we know the names of all the variables, so we
can write the lines in this prologue.

We have to do a little work to know what names we need to define.  Looking at our sample template:

```html
<p>Welcome, {{user_name}}!</p>
<p>Products:</p>
<ul>
{% for product in product_list %}
    <li>{{ product.name }}:
        {{ product.price|format_price }}</li>
{% endfor %}
</ul>
```

There are two variables used here, `user_name` and `product`.  The `all_vars`
set will have both of those names, because both are used in `{{...}}`
expressions.  But only `user_name` needs to be extracted from the context in
the prologue, because `product` is defined by the loop.

All the variables used in the template are in the set `all_vars`, and all the
variables defined in the template are in `loop_vars`.  All of the names in
`loop_vars` have already been defined in the code because they are used in
loops.  So we need to unpack any name in `all_vars` that isn't in `loop_vars`:

<!-- [[[cog include("templite.py", first="for var_name", numblanks=1, dedent=False) ]]] -->
```python
        for var_name in self.all_vars - self.loop_vars:
            vars_code.add_line("c_%s = context[%r]" % (var_name, var_name))
```
<!-- [[[end]]] -->

Each name becomes a line in the function's prologue, unpacking the context
variable into a suitably named local variable.

We're almost done compiling the template into a Python function.  Our function
has been appending strings to `result`, so the last line of the function is
simply to join them all together and return them:

<!-- [[[cog include("templite.py", first='code.add_line("return', numlines=2, dedent=False) ]]] -->
```python
        code.add_line("return ''.join(result)")
        code.dedent()
```
<!-- [[[end]]] -->

Now that we've finished writing the source for our compiled Python function,
we need to get the function itself from our CodeBuilder object.  The
`get_globals` method executes the Python code we've been assembling.  Remember
that our code is a function definition (starting with `def render_function(..):`),
so executing the code will define `render_function`, but not execute the body
of `render_function`.

The result of `get_globals` is the dictionary of values defined in the code.
We grab the `render_function` value from it, and save it as an attribute in our
Templite object:

<!-- [[[cog include("templite.py", first="self._render_function =", numlines=1, dedent=False) ]]] -->
```python
        self._render_function = code.get_globals()['render_function']
```
<!-- [[[end]]] -->

Now `self._render_function` is a callable Python function. We'll use it later,
during the rendering phase.


#### Compiling Expressions

We haven't yet seen a significant piece of the compiling process: the
`_expr_code` method that compiles a template expression into a Python
expression.  Our template expressions can be as simple as a single name:

```
{{user_name}}
```

\noindent or can be a complex sequence of attribute accesses and filters:

```
{{user.name.localized|upper|escape}}
```

Our `_expr_code` method will handle all of these possibilities.  As with
expressions in any language, ours are built recursively: big expressions are
composed of smaller expressions.  A full expression is pipe-separated, where
the first piece is dot-separated, and so on.  So our function naturally takes a
recursive form:

<!-- [[[cog include("templite.py", first="def _expr_code", numlines=2, dedent=False) ]]] -->
```python
    def _expr_code(self, expr):
        """Generate a Python expression for `expr`."""
```
<!-- [[[end]]] -->

The first case to consider is that our expression has pipes in it.  If it does,
then we split it into a list of pipe-pieces.  The first pipe-piece is passed
recursively to `_expr_code` to convert it into a Python expression.

<!-- [[[cog include("templite.py", first="if ", after="def _expr_code", numlines=6, dedent=False) ]]] -->
```python
        if "|" in expr:
            pipes = expr.split("|")
            code = self._expr_code(pipes[0])
            for func in pipes[1:]:
                self._variable(func, self.all_vars)
                code = "c_%s(%s)" % (func, code)
```
<!-- [[[end]]] -->

Each of the remaining pipe pieces is the name of a function.  The value is
passed through the function to produce the final value.  Each function name is
a variable that gets added to `all_vars` so that we can extract it properly in
the prologue.

If there were no pipes, there might be dots.  If so, split on the dots.  The
first part is passed recursively to `_expr_code` to turn it into a Python
expression, then each dot name is handled in turn:

<!-- [[[cog include("templite.py", first="elif ", after="def _expr_code", numlines=5, dedent=False) ]]] -->
```python
        elif "." in expr:
            dots = expr.split(".")
            code = self._expr_code(dots[0])
            args = ", ".join(repr(d) for d in dots[1:])
            code = "do_dots(%s, %s)" % (code, args)
```
<!-- [[[end]]] -->

To understand how dots get compiled, remember that `x.y` in the template could
mean either `x['y']` or `x.y` in Python, depending on which works;  if the
result is callable, it's called.  This uncertainty means that we have to try
those possibilities at run time, not compile time.  So we compile `x.y.z` into
a function call, `do_dots(x, 'y', 'z')`.  The dot function will try the
various access methods and return the value that succeeded.

The `do_dots` function is passed into our compiled Python function at run time.
We'll see its implementation in just a bit.

The last clause in the `_expr_code` function handles the case that there was no
pipe or dot in the input expression.  In that case, it's just a name. We
record it in `all_vars`, and access the variable using its prefixed Python
name:

<!-- [[[cog include("templite.py", first="else:", after="def _expr_code", numlines=4, dedent=False) ]]] -->
```python
        else:
            self._variable(expr, self.all_vars)
            code = "c_%s" % expr
        return code
```
<!-- [[[end]]] -->


#### Helper Functions

During compilation, we used a few helper functions.  The `_syntax_error` method
simply puts together a nice error message and raises the exception:

<!-- [[[cog include("templite.py", first="def _syntax_error", numblanks=1, dedent=False) ]]] -->
```python
    def _syntax_error(self, msg, thing):
        """Raise a syntax error using `msg`, and showing `thing`."""
        raise TempliteSyntaxError("%s: %r" % (msg, thing))
```
<!-- [[[end]]] -->

The `_variable` method helps us with validating variable names and adding them
to the sets of names we collected during compilation.  We use a
regex to check that the name is a valid Python identifier, then add the name to
the set:

<!-- [[[cog include("templite.py", first="def _variable", numblanks=4, dedent=False) ]]] -->
```python
    def _variable(self, name, vars_set):
        """Track that `name` is used as a variable.

        Adds the name to `vars_set`, a set of variable names.

        Raises an syntax error if `name` is not a valid name.

        """
        if not re.match(r"[_a-zA-Z][_a-zA-Z0-9]*$", name):
            self._syntax_error("Not a valid name", name)
        vars_set.add(name)
```
<!-- [[[end]]] -->

With that, the compilation code is done!


#### Rendering

All that's left is to write the rendering code.  Since we've compiled our
template to a Python function, the rendering code doesn't have much to do.  It
has to get the data context ready, and then call the compiled Python code:

<!-- [[[cog include("templite.py", first="def render(", numblanks=3, dedent=False) ]]] -->
```python
    def render(self, context=None):
        """Render this template by applying it to `context`.

        `context` is a dictionary of values to use in this rendering.

        """
        # Make the complete context we'll use.
        render_context = dict(self.context)
        if context:
            render_context.update(context)
        return self._render_function(render_context, self._do_dots)
```
<!-- [[[end]]] -->

Remember that when we constructed the `Templite` object, we started with a data
context.  Here we copy it, and merge in whatever data has been passed in for
this rendering.  The copying is so that successive rendering calls won't see
each others' data, and the merging is so that we have a single dictionary to
use for data lookups.  This is how we build one unified data context from the
contexts provided when the template was constructed, with the data provided now
at render time.

Notice that the data passed to `render` could overwrite data passed to the
Templite constructor.  That tends not to happen, because the context passed to
the constructor has global-ish things like filter definitions and
constants, and the context passed to `render` has specific data for that one
rendering.

Then we simply call our compiled `render_function`.  The first argument is the
complete data context, and the second argument is the function that will implement
the dot semantics.  We use the same implementation every time: our own
`_do_dots` method.

<!-- [[[cog include("templite.py", first="def _do_dots", numblanks=1, dedent=False) ]]] -->
```python
    def _do_dots(self, value, *dots):
        """Evaluate dotted expressions at runtime."""
        for dot in dots:
            try:
                value = getattr(value, dot)
            except AttributeError:
                value = value[dot]
            if callable(value):
                value = value()
        return value
```
<!-- [[[end]]] -->

During compilation, a template expression like `x.y.z` gets turned into
`do_dots(x, 'y', 'z')`.  This function loops over the dot-names, and for each
one tries it as an attribute, and if that fails, tries it as a key.  This is
what gives our single template syntax the flexibility to act as either `x.y` or
`x['y']`.  At each step, we also check if the new value is callable, and if it
is, we call it.  Once we're done with all the dot-names, the value in hand is
the value we want.

Here we used Python argument unpacking again (`*dots`) so that `_do_dots` could
take any number of dot names.  This gives us a flexible function that will work
for any dotted expression we encounter in the template.

Note that when calling `self._render_function`, we pass in a function to use
for evaluating dot expressions, but we always pass in the same one.  We could
have made that code part of the compiled template, but it's the same eight
lines for every template, and those eight lines are part of the definition of
how templates work, not part of the details of a particular template.  It feels
cleaner to implement it like this than to have that code be part of the
compiled template.


## Testing

Provided with the template engine is a suite of tests that cover all of the
behavior and edge cases.  I'm actually a little bit over my 500-line limit:
the template engine is 252 lines, and the tests are 275 lines.  This is typical
of well-tested code: you have more code in your tests than in your product.


## What's Left Out

Full-featured template engines provide much more than we've implemented
here.  To keep this code small, we're leaving out interesting ideas like:

* Template inheritance and inclusion
* Custom tags
* Automatic escaping
* Arguments to filters
* Complex conditional logic like else and elif
* Loops with more than one loop variable
* Whitespace control

Even so, our simple template engine is useful.  In fact, it is the template
engine used in coverage.py to produce its HTML reports.


## Summing up

In 252 lines, we've got a simple yet capable template engine.  Real template
engines have many more features, but this code lays out the basic ideas of the
process: compile the template to a Python function, then execute the function
to produce the text result.
