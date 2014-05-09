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
def render_function(ctx, do_dots):
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
            s(do_dots(c_product, 'name')),
            ':\n        ',
            s(c_format_price(do_dots(c_product, 'price'))),
            '</li>\n'
        ])
    a('\n</ul>\n')
    return ''.join(result)
```

This Python code looks unusual, because we've chosen some shortcuts that
produce slightly faster code.  Each template is converted into a
`render_function` function that takes a dictionary of data called the context
(abbreviated to `ctx`). The body of the function starts by unpacking the data
from the context into local names, because they are faster for repeated use.
We use locals with a `c_` prefix so that we can use other local names without
fear of collisions.

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
result.  Dots in the expression are handled by the `do_dots` function passed
into our function, because the meaning of the dotted expressions depends on the
data in the context: it could be attribute access or item access, and it could
be a callable.

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

* a method to produce the final values by executing the code.  This stringifies
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


### The Templite class

The heart of the template engine is the Templite class.  (Get it? It's a
template, but it's lite!)

The Templite class has a very simple interface.  You construct one with the
text of the template, then later you can use the `.render` method on it to
render a particular dictionary of data through the template:

```
templite = Templite('''
    <h1>Hello {{name|upper}}!</h1>
    {% for topic in topics %}
        <p>You are interested in {{topic}}.</p>
    {% endif %}
    ''',
    {'upper': str.upper},
)
text = templite.render({
    'name': "Ned",
    'topics': ['Python', 'Geometry', 'Juggling'],
})
```

The constructor also accepts a dictionary of values, these are stored in the
Templite object, and will also be available when the template is later
rendered.  These are good for defining global functions, for example.


#### Compiling

All of the work to compile the template into a Python function happens in the
Templite constructor.  First the contexts are saved away:

<!-- [[[cog include("templite.py", first="def __init__(self, text, ", numblanks=3, dedent=False) ]]] -->
```
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

Notice we used `*contexts` as the argument, using Python argument packing, so
that the caller can provide a number of different context dictionaries.

To make our compiled function as fast as possible, we extract context variables
into Python locals.  We'll get the set of all those names by keeping a set of
variable names we encounter, but we also need to track the names of variables
defined in the template, the loop variables:

<!-- [[[cog include("templite.py", first="self.all_vars", numblanks=1, dedent=False) ]]] -->
```
        self.all_vars = set()
        self.loop_vars = set()
```
<!-- [[[end]]] -->

Later we'll see how these get used to help contruct the preface of our
function.

Now we use the CodeBuilder class we wrote earlier to start to build our
function:

<!-- [[[cog include("templite.py", first="code = CodeBuilder", numblanks=2, dedent=False) ]]] -->
```
        code = CodeBuilder()

        code.add_line("def render_function(ctx, do_dots):")
        code.indent()
        vars_code = code.add_subbuilder()
        code.add_line("result = []")
        code.add_line("a = result.append")
        code.add_line("e = result.extend")
        code.add_line("s = str")
```
<!-- [[[end]]] -->

Here we construct our CodeBuilder object, and start writing lines into it. Our
Python function will be called `render_function`, and will take two arguments:
`ctx` is the data dictionary it should use, and `do_dots` is a function it can
use to implement dot attribute access.

We create a sub-builder called `vars_code`.  Later we'll write the variable
extraction lines into that sub-builder.  This lets us save a place in the 
function that can be filled in later when we have the information we need.

Then four fixed lines are written, defining a result list, shortcuts for the
methods to append to or extend that list, and a shortcut for the `str()`
builtin.  This odd step is done to squeeze just a little bit more performance
out of our rendering function.  CPython looks up local names a little faster
than builtin names.

The reason we have both the `append` and the `extend` shortcut is so we can
use the most effective method, depending on whether we have one line to add to
our result, or more than one.

Next we define an inner function to help us with buffering output strings:

<!-- [[[cog include("templite.py", first="buffered =", numblanks=1, dedent=False) ]]] -->
```
        buffered = []
        def flush_output():
            """Force `buffered` to the code builder."""
            if len(buffered) == 1:
                code.add_line("a(%s)" % buffered[0])
            elif len(buffered) > 1:
                code.add_line("e([%s])" % ", ".join(buffered))
            del buffered[:]
```
<!-- [[[end]]] -->

The `buffered` list are strings that are yet to be written to our function
source code.  As our template compilation proceeds, we'll append strings to
`buffered`, and flush them to the function source when we reach control flow
points, like if statements, or the beginning or ends of loops.

The `flush_output` function is a closure, it refers to `buffered` and `code` 
implicitly, which simplifies our calls to the function.  If only one string has
been buffered, then the `a` shortcut is used to append it to the result. If
more than one is buffered, then all of them are used with the `e` shortcut (for
extend) to add them to the result.  Then the buffered list is cleared so more
strings can be buffered.

The rest of the compiling code will add lines to the function by appending them
to `buffered`, and eventually calling `flush_output` to write them to the
CodeBuilder.

As we parse control structures, we want to check that they are properly nested.
The `ops_stack` list is a simple stack of strings:

<!-- [[[cog include("templite.py", first="ops_stack", numblanks=1, dedent=False) ]]] -->
```
        ops_stack = []
```
<!-- [[[end]]] -->

When we encounter an `{% if .. %}` tag, we'll push `'if'` onto the stack.  When
we find an `{% endif %}` tag, we can pop the stack and report an error if it
wasn't `'if'` at the top of the stack.

Now the real parsing begins.  We split the template text into a number of tokens
using a regular expression:

<!-- [[[cog include("templite.py", first="tokens =", numblanks=1, dedent=False) ]]] -->
```
        tokens = re.split(r"(?s)({{.*?}}|{%.*?%}|{#.*?#})", text)
```
<!-- [[[end]]] -->

This looks complicated, let's break it down.  The `re.split` function will
split a string using a regular expression.  If the pattern is parenthesized,
then the matches will be used to split the string, but will also be returned as
pieces in the split list.  Our pattern will match our tag syntaxes, but we've
parenthesized it so that the string will be split at the tags, and the tags
will also be returned.

The `(?s)` flag in the regex means that dot should match even a newline. Then
we have our parenthesized group of three alternatives: `{{.*?}}` matches an 
expression, `{%.*?%}` matches a tag, and `{#.*?#}` matches a comment.  In all
of these, we use `.*?` to match any number of characters, but the shortest
sequence that matches.  With `.*`, we'd match the longest, which match from the
beginning of the first tag to the end of the last tag.

The result of `re.split` is a list of strings.  For example, this template text:

```
<p>Topics for {{name}}: {% for t in topics %}{{t}}, {% endfor %}</p>
```

is split into these pieces:

```
[
    '<p>Topics for ',
    '{{name}}',
    ': ',
    '{% for t in topics %}',
    '',
    '{{t}}',
    ', ',
    '{% endfor %}',
    '</p>'
]
```

The compilation code is a loop over these tokens:

<!-- [[[cog include("templite.py", first="for token", numlines=1, dedent=False) ]]] -->
```
        for token in tokens:
```
<!-- [[[end]]] -->

Each token is examined to see which of the four cases it is.  Just looking at
the first two characters is enough.  The first case is a comment, it's easy to
handle: just ignore it and move on to the next token:

<!-- [[[cog include("templite.py", first="if token.", numlines=3, dedent=False) ]]] -->
```
            if token.startswith('{#'):
                # Comment: ignore it and move on.
                continue
```
<!-- [[[end]]] -->

For the case of `{{...}}` expressions, we cut off the two braces at the front
and back, strip off the white space, and pass the entire expression to
`_expr_code`:

<!-- [[[cog include("templite.py", first="elif token.startswith('{{')", numlines=3, dedent=False) ]]] -->
```
            elif token.startswith('{{'):
                # An expression to evaluate.
                buffered.append("s(%s)" % self._expr_code(token[2:-2].strip()))
```
<!-- [[[end]]] -->

The `_expr_code` method will compile the template expression into Python code.
We'll see that function later.  The result goes into our function, with
the `s` function (shorthand for the `str` builtin).

The third case is the big one: `{% ... %}` tags.  These are control structures
that will become Python control structures.  First we have to flush our
buffered output lines, then we extract a list of words from the tag:

<!-- [[[cog include("templite.py", first="elif token.startswith('{%')", numlines=4, dedent=False) ]]] -->
```
            elif token.startswith('{%'):
                # Action tag: split into words and parse further.
                flush_output()
                words = token[2:-2].strip().split()
```
<!-- [[[end]]] -->

Now we have three sub-cases, based on the first word in the tag: if, for, or
end.  The if case shows our simple error handling and code generation:

<!-- [[[cog include("templite.py", first="if words[0] == 'if'", numlines=7, dedent=False) ]]] -->
```
                if words[0] == 'if':
                    # An if statement: evaluate the expression to determine if.
                    if len(words) != 2:
                        self._syntax_error("Don't understand if", token)
                    ops_stack.append('if')
                    code.add_line("if %s:" % self._expr_code(words[1]))
                    code.indent()
```
<!-- [[[end]]] -->

The if tag should have a single expression, so the `words` list should have
only two elements in it.  If it doesn't, we use the `_syntax_error` helper
method to raise a syntax error exception.  We push `'if'` onto `ops_stack` so
that we can check the endif tag.  The expression part of the if tag is compiled
to a Python expression with `_expr_code`, and is used as the conditional
expression in a Python if statement.

The second tag type is "for", which will of course be compiled to a Python for
statement:

<!-- [[[cog include("templite.py", first="elif words[0] == 'for'", numlines=13, dedent=False) ]]] -->
```
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

We do a simple check of the syntax and push `'for'` onto the stack.  The
`_variable` method checks the syntax of the variable, and adds it to the set
we provide.  This is how we collect up the names of all the loop variables
during the compilation so that we can later create the prologue of the function.

We add one line to our function source, a for statement.  All of our template
variables are turned into Python variables by prepending `c_` to them so that
we know they won't collide with other names we're using in our Python function.
We use `_expr_code` to compile the iteration expression from the template into
an iteration expression in Python.

The last kind of tag we handle is and end tag, either `{% endif %}` or
`{% endfor %}`.  The effect on our compiled function source is the same: simply
unindent to end the if statement or for statement that was started earlier:

<!-- [[[cog include("templite.py", first="elif words[0].startswith('end')", numlines=11, dedent=False) ]]] -->
```
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

Speaking of error handling, if the tag isn't an if, a for, or an end, then we
don't know what it is, so raise a syntax error:

<!-- [[[cog include("templite.py", first="else:", numlines=2, dedent=False) ]]] -->
```
                else:
                    self._syntax_error("Don't understand tag", words[0])
```
<!-- [[[end]]] -->

We're done with the three different special syntaxes (`{{...}}`, `{#...#}`, and
`{%...%}`), what's left is literal content.  We'll add the literal string to
the buffered output, using the `repr` built-in function to produce a Python
string literal for the token.  There's no point adding an empty string to the
output, so only add the token if it's non-empty:

<!-- [[[cog include("templite.py", first="else:", after="Don't understand tag", numblanks=1, dedent=False) ]]] -->
```
            else:
                # Literal content.  If it isn't empty, output it.
                if token:
                    buffered.append(repr(token))
```
<!-- [[[end]]] -->

That completes the loop over all the tokens in the template.  When the loop is
done, all of the template has been processed.  We have one last check to make:
if `ops_stack` isn't empty, then we must be missing an end-tag.  Then we flush
the buffered output to the function source:

<!-- [[[cog include("templite.py", first="if ops_stack:", numblanks=2, dedent=False) ]]] -->
```
        if ops_stack:
            self._syntax_error("Unmatched action tag", ops_stack[-1])

        flush_output()
```
<!-- [[[end]]] -->

We had created a sub-builder at the beginning of the function.  Its role was to
unpack template variables from the context into Python locals.  Now that we've
processed the entire template, we know the names of all the variables, so we
can write the lines in this prologue.  The variables used are in the set
`self.all_vars`, and all the variables defined in the template are in
`self.loop_vars`.  We need to unpack any name in `all_vars` that isn't in
`loop_vars`:

<!-- [[[cog include("templite.py", first="for var_name", numblanks=1, dedent=False) ]]] -->
```
        for var_name in self.all_vars - self.loop_vars:
            vars_code.add_line("c_%s = ctx[%r]" % (var_name, var_name))
```
<!-- [[[end]]] -->

Each name becomes a line in the function's prologue unpacking the context
variable into a suitably-named local variable.

We're almost done compiling the template into a Python function.  Our function
has been appending strings to `result`, so the last line of the function is
simply to join them all together and return them:

<!-- [[[cog include("templite.py", first='code.add_line("return', numlines=2, dedent=False) ]]] -->
```
        code.add_line("return ''.join(result)")
        code.dedent()
```
<!-- [[[end]]] -->

Finally, we get the function itself from our CodeBuilder object.  This line
executes the Python code we've been assembling.  The dictionary of globals is
returned, we grab the `render_function` value from it, and save it as an
attribute in our Templite object:

<!-- [[[cog include("templite.py", first="self._render_function =", numlines=1, dedent=False) ]]] -->
```
        self._render_function = code.get_globals()['render_function']
```
<!-- [[[end]]] -->

Now `self._render_function` is a callable Python function, we'll use it later
during the rendering phase.


#### Compiling Expressions

We haven't yet seen a significant piece of the compiling process: the
`_expr_code` method that compiles a template expression into a Python
expression.  Our template expressions can be as simple as a single name:

```
{{user_name}}
```

or can be a complex accretion of attribute access and filters:

```
{{user.name.localized|upper|escape}}
```

Our `_expr_code` method will handle of these possibilities.  As with
expressions in any language, ours are built recursively: a full expression is
pipe-separated, where the first piece is dot-separated, and so on.  So our
function naturally takes a recursive form:

<!-- [[[cog include("templite.py", first="def _expr_code", numlines=2, dedent=False) ]]] -->
```
    def _expr_code(self, expr):
        """Generate a Python expression for `expr`."""
```
<!-- [[[end]]] -->

The first case to consider is that our expression has pipes in it.  If it does,
then we split it into a list of pipe-pieces.  The first pipe-piece is passed
recursively to `_expr_code` to turn it into a Python expression.

<!-- [[[cog include("templite.py", first="if ", after="def _expr_code", numlines=6, dedent=False) ]]] -->
```
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
```
        elif "." in expr:
            dots = expr.split(".")
            code = self._expr_code(dots[0])
            args = ", ".join(repr(d) for d in dots[1:])
            code = "do_dots(%s, %s)" % (code, args)
```
<!-- [[[end]]] -->

To understand how dots get compiled, remember that `x.y` in the template could
mean either `x['y']` or `x.y` in Python, depending on which works, and if the
result is callable, it's called.  This uncertainty means that we have to try
those possibilities at run time, not compile time.  So we compile `x.y.z` into
a function call, `do_dots(x, 'y', 'z')`.  The dot function will try the
various access methods to and return the value that succeeded.

The `do_dots` function is passed into our compiled Python function at run time,
we'll see how it is implemented in just a bit.

The last clause in the `_expr_code` function handles the case that there was no
pipe or dot in the input expression.  In that case, it's just a simple name. We
record it in `all_vars`, and access the variable using its prefixed Python
name:

<!-- [[[cog include("templite.py", first="else:", after="def _expr_code", numlines=4, dedent=False) ]]] -->
```
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
```
    def _syntax_error(self, msg, thing):
        """Raise a syntax error using `msg`, and showing `thing`."""
        raise TempliteSyntaxError("%s: %r" % (msg, thing))
```
<!-- [[[end]]] -->

The `_variable` method helped us with validating variable names and adding them
to the sets of names we collected during compilation.  It's simple: we use a
regex to check that the name is a valid Python identifier, then add the name to
the set:

<!-- [[[cog include("templite.py", first="def _variable", numblanks=4, dedent=False) ]]] -->
```
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
template to a Python function, the rendering code is very simple.  It has to
get the data context ready, and then call the compiled Python code:

<!-- [[[cog include("templite.py", first="def render", numblanks=3, dedent=False) ]]] -->
```
        code.add_line("def render_function(ctx, do_dots):")
        code.indent()
        vars_code = code.add_subbuilder()
        code.add_line("result = []")
        code.add_line("a = result.append")
        code.add_line("e = result.extend")
        code.add_line("s = str")

        buffered = []
        def flush_output():
            """Force `buffered` to the code builder."""
            if len(buffered) == 1:
                code.add_line("a(%s)" % buffered[0])
            elif len(buffered) > 1:
                code.add_line("e([%s])" % ", ".join(buffered))
            del buffered[:]

        ops_stack = []
```
<!-- [[[end]]] -->

Remember that when we constructed the `Templite` object, we started with a data
context.  Here we copy it, and add in whatever data has been passed in for this
rendering.  Then we simply call our compiled `render_function`.  The first
argument is the data context, the second argument is the function that will
implement the dot semantics.  We use the same implementation every time, our
own `_do_dots` method, which is the last piece of code to look at:

<!-- [[[cog include("templite.py", first="def _do_dots", numblanks=1, dedent=False) ]]] -->
```
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


## Testing

Provided with the template engine is a suite of tests that cover all of the
behavior and edge cases.  I'm actually a little bit over my 500-line limit:
the template engine is 251 lines, and the tests are 275 lines.  This is typical
of well-tested code: you have more code in your tests than in your product.


## Summing up

In 251 lines, we've got a simple yet capable templating engine.  Real template
engines have many more features, but this code lays out the basic ideas of the
process: compile the template to a Python function, then execute the function
to produce the text result.
