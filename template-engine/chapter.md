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
inserted with string substitution of some sort.  Some of our dynamica data is
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
We'll call the two main options interpretation and compilation, using the terms
loosely from other languages.

In an interpretation model, the parsing phase produces a data structure
representing the structure of the template. The execution phase walks
that data structure, assembling the result string based on the instructions it
found.

In a compilation model, the parsing phase produces some form of executable
code.  The execution phases executes that code, producing the result.

Our implementation of the engine uses the compilation model.  We compile the
template into Python code.  When run, the Python code assembles the result.

[[much more to come...]]
