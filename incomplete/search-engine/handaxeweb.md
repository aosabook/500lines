handaxeweb: a minimalist literate-programming system
====================================================

**Note for 500lines project**: I’m adding handaxeweb to this project
not because I think it should appear in the book, but just because I
want to be able to use it to avoid source code duplication.

> Let us change our traditional attitude to the construction
> of programs: Instead of imagining that our main task is to
> instruct a computer what to do, let us concentrate rather
> on explaining to humans what we want the computer to do. 

> > — Donald E. Knuth, "Literate Programming", in The Computer
> > Journal, 1984, p.99

Literate-programming systems are systems for writing programs
that are optimized for readability. This is a very simple
literate-programming system called “handaxeweb”
that supports multiple versions of a program in the same
HTML or Markdown document.

What literate programming is, and how handaxeweb is related to other such systems
---------------------------------------------------------------------------------

Traditionally a literate-programming system contains two 
programs: one called `tangle`, to feed the program to the compiler,
and one to
produce a printable version called `weave` (related to a
famous couplet alluding to webs).

Following noweb, handaxeweb doesn’t make any attempt to produce a
“woven” output for human consumption; it only tangles.
The idea is that you
write your literate program either as a plain ASCII text
document, or in Markdown or something, as long as it permits
you to write segments of your program indented by four
spaces.

### Phil Bewig’s “The Essence of Literate Programming”: the inspiration ###

handaxeweb is more directly inspired by Phil Bewig’s “The
Essence of Literate Programming”, a post on
comp.programming.literate on 1996-05-27, message-id
`<pbewigDs1Ewq.G03@netcom.com>`, containing the following
noweb-like literate-programming system written in awk:

    # in The Essence of Literate Programming:
    /^<<.+>>=$/ {
        name = substr($0, 3, length($0) - 5)
        while (getline > 0) {
            if (length($0) == 0) next
            chunk[name, ++count[name]] = $0 } }
    END { tangle("*", ""); printf "\n" }
    function tangle(name, prefix,    i, tag, suffix) {
        for (i = 1; i <= count[name]; i++) {
            if (i == 2) gsub(/[^ \t]/, " ", prefix)
            if (match(chunk[name,i], /<<.+>>/)) {
                tag = substr(chunk[name,i], RSTART + 2, RLENGTH - 4)
                if (tag in count) {
                    suffix = substr(chunk[name,i], RSTART + RLENGTH)
                    tangle(tag, prefix substr(chunk[name,i], 1, RSTART - 1))
                    printf "%s", suffix }
                else printf "%s%s", prefix, chunk[name,i] }
            else printf "%s%s", prefix, chunk[name,i]
            if (i < count[name]) printf "\n" } }

He explained:

> The essence of literate programming is rearranging chunks
> of code, and a dozen and a half lines of awk is all you
> need for that.
> 
> Of course, with so little code it's not possible for
> everything to be perfect. … Even so, this microscopic
> system provides a useful tool that encompasses the essence
> of literate programming.

### Overview of handaxeweb's features ###

Unfortunately, handaxeweb is 208 lines of code, twice the
size of the previous Python version, and 
more than ten
times the size of The Essence of Literate Programming (a full
sixth of the size of CWEB!). But it 
solves a couple of other problems that I need for my
purposes:

* versioning: multiple versions of the same program in the
  same version of the same document;
* multiple separate programs in the same document;
* listing the programs and versions in a document;
* indentation (needed for languages like Python);
* support for Markdown, which is how I write most
  human-readable documents these days.

Literate programs may contain multiple versions of the program
--------------------------------------------------------------

Versioning is one of the biggest problems I've had with the
previous version of handaxeweb, written in Python.

When I write a literate program, there are often bits of it
that are present for scaffolding in initial versions which
then should be removed in future versions. This is especially
true with these bootstrapping-compiler things 
I've been writing lately, where the
initial version of the bootstrapping compiler supports a
minimal number of features and can barely compile itself,
while later versions share a lot of code with the first
version --- but all the versions coexist simultaneously, and
I want to be able to make a bug-fix in the shared code.

The programming language itself can provide some support for
this, as e.g. CSS does. But what about the case where the
language itself doesn’t help much?

One obviously possible approach is to redefine the program
from the root down; e.g., first you say

     in the initial version:
     <<bottom abstraction layer>>
     <<initializations>>
     <<main program>>

And defining each of those pieces:

    in initializations:
    <<initialize I/O layer>>
    <<initialize internal data structures>>

etc., and then for the next version:

    in the new version:
    <<bottom abstraction layer>>
    <<new initializations>>
    <<main program>>

with new versions of whatever treenodes have changed, such as:

    in new initializations:
    <<new initialize I/O layer>>
    <<initialize internal data structures>>

and “new initialize I/O layer”.

Obviously this is pretty suboptimal in terms of requiring a
lot of copy-and-pasted text that doesn’t really help the
reader.

Version numbers on chunks allow such versions gracefully
--------------------------------------------------------

Here’s a better idea. Every named chunk can have several
versions, each with a version number. The name of the chunk
when it’s being defined may end with “v312” to indicate that
the text that follows belongs to version 312. Otherwise, it
belongs to version 0. You can tangle any version N of any
chunk; this will use the highest-numbered version <= N of
each referenced chunk.

This means that you can get the effect of the repetition
above simply by saying:

    in initialize I/O layer v1:

and then tangling v1 of “initial version”.

The syntax of handaxeweb
------------------------

The previous version of handaxeweb uses
indented lines of the form “(in foo)” to start new named
chunks. This is pretty reasonable, but it would be better if
the line could be a valid comment in whatever language, to
better support syntax-highlighting. So the right thing to do
is to omit leading and trailing punctuation, but require a
trailing ":", as in the previous examples in this document.

Beyond that, the syntax of `handaxeweb` is simply that
program code is indented by four spaces, and references to
other chunks are enclosed in `<<>>`.

handaxeweb, the program
-----------------------

    -- in handaxeweb.lua:
    #!/usr/bin/lua
    <<definitions>>

    <<read input literate program>>
    <<carry out specified action on it>>

The main actions desired are to list the possible chunk names
and version numbers, and to tangle a particular chunk with a
particular version number.

    -- in carry out specified action on it:
    chunkname, version = ...
    if chunkname == nil then
        list_chunk_names_and_versions(chunks)
    else
        if version == nil then version = 0 end
        tangle(chunks, chunkname, tonumber(version))
    end

The problem of reading the input program can be factored into
a third subroutine:

    -- in read input literate program:
    local chunks = parse_input()

So, the definitions so far needed:

    -- in definitions:
    <<parse_input>>

    <<list_chunk_names_and_versions>>

    <<tangle>>

These three need to share a common idea of the contents of
the variable `chunks`. I think it should be a hash from chunk
names to lists of chunk versions, where each version contains
a version number and some text, stored as a list of
lines.

    -- in an example of the chunks variable:
    {['read input literate program'] =
        {{v=0, text={"local chunks = parse_input()", ...}, 
         {v=1, ...}
         ...}
        },
     parse_input={{v=0...}, ...},
     ...
    }

### `parse_input` ###

The job of `parse_input` is to turn the input file into such
a structure. It looks for sequences of lines indented by at
least four spaces to use as chunks; they may begin with a
header line specifying their name and version, or they may
just be a continuation of some previous chunk with a name and
version.

We start with a nameless chunk that will be discarded.

    -- in parse_input:
    <<parse_input definitions>>

    function parse_input()
        local chunks, current_chunk, in_chunk = {}, {text={}}, false
        local blank_lines = {}

        for line in io.lines() do
            if string.match(line, "^%s*$") then -- blank line
                <<handle blank line>>
            elseif not in_chunk and is_indented(line) then
                <<handle possible header line>>
                in_chunk = true
            elseif in_chunk and is_indented(line) then
                <<handle normal indented line>>
            else
                blank_lines = {}
                in_chunk = false
            end
        end
        <<handle last chunk>>

        return chunks
    end

Initially `current_chunk` is `nil`, and we don’t start a
`current_chunk` until we see a header line. After that,
`current_chunk.text` is always a list.

We need special handling for blank lines because they can
occur inside of an indented region, but not have any spaces
on them, depending on editor settings. So in this case we
leave untouched the `in_chunk` setting, telling us whether we're in the
middle of an indented chunk, and we append the blank
line to a list that gets incorporated only if more nonblank
indented lines appear.

    -- in handle blank line:
    if in_chunk then table.insert(blank_lines, "") end

Handling a normal indented line is very easy. Any parsing
will be handled later by `tangle`.

    -- in handle normal indented line:
    -- incorporate any blank lines seen in between indented lines
    for _, blank_line in ipairs(blank_lines) do
        table.insert(current_chunk.text, blank_line)
    end
    blank_lines = {}

    table.insert(current_chunk.text, unindented(line))

The possible header line may be either a header line (not
included in the chunk itself) or an ordinary chunk line,
possibly adding more lines onto the previous chunk.

    -- in handle possible header line:
    local label = get_chunk_label(line)

    if label then  -- if that succeeded, change chunks
        register_chunk(chunks, current_chunk)
        local name, ver = parse_chunk_label(label)
        current_chunk = {name = name, v = ver, text = {}}
    else
        <<handle normal indented line>>
    end

At the end of input, we just need to handle the last chunk:

    -- in handle last chunk:
    register_chunk(chunks, current_chunk)

So the `parse_input` function itself depends on a few other
functions:

    -- in parse_input definitions:
    <<register_chunk>>

    <<is_indented>>

    <<unindented>>

    <<get_chunk_label>>

    <<parse_chunk_label>>

`register_chunk` is the only thing that actually builds the
table `chunks`. It has to deal with questions of
duplicate-handling, and discard the initial nil chunk.

With regard to duplicate-handling: if there are multiple
chunks with the same name and version, then we concatenate
them. This supports two important uses:

1. It allows you to intersperse formatted text with the lines
   of a chunk without having to add header lines all over the
   place. If you like, you can write your entire program this
   way, with just a single header line at the top.

2. It allows you to progressively add to multiple sections in
   parallel throughout your document. The example given in
   the CWEB manual is that you might have one section for all
   your global variables, progressively adding things to
   it. Some other examples follow: in C, it’s often
   convenient to put a declaration into a `.h` file at the
   same time as an implementation into a `.c` file; in a
   bytecode virtual machine, it may be convenient to put
   cases into a centralized `switch` statement at the same
   time as defining functions that those cases call.

However, it may run into some difficulty with versioning. If
you define a new version of a chunk, then in that version, it
replaces all of the text in that chunk, not just one
paragraph of it. Clearly if those paragraphs are spread all
over your document, that’s going to be hard to get right.

    -- in register_chunk:
    function register_chunk(chunks, new_chunk)
        if new_chunk.name == nil then return end

        local contents = chunks[new_chunk.name]
        if not contents then 
            contents = {}
            chunks[new_chunk.name] = contents
        end

        -- If there’s a duplicate, append text to it.
        for _, it in ipairs(chunks[new_chunk.name]) do
            if it.v == new_chunk.v then
                for _, line in ipairs(new_chunk.text) do
                    table.insert(it.text, line)
                end
                return
            end
        end

        -- No duplicate. Add to table.
        table.insert(contents, new_chunk)
    end

The indentation functions are very simple.

    -- in is_indented:
    function is_indented(line)
        return string.match(line, "^    ")
    end

    assert(    is_indented("    hi"))
    assert(not is_indented("   hi"))
    assert(not is_indented("   hi    "))

The `unindented` function assumes the line is indented.

    -- in unindented:
    function unindented(line) return string.sub(line, 5) end
    assert(unindented("    hi\n") == "hi\n")

Recognizing the chunk labels is not too hard with Lua’s
pattern-matching:

    -- in get_chunk_label:
    function get_chunk_label(line)
        return string.match(line, "^[^%w]*in (.*):[^%w]*$")
    end

    assert(get_chunk_label("-- in handaxeweb.lua:") ==
           "handaxeweb.lua")
    assert(get_chunk_label("/* in handaxeweb.c: */") ==
           "handaxeweb.c")
    assert(get_chunk_label("# in a minute: #\n") ==
           "a minute")

Pulling the version number out can be done similarly easily.

    -- in parse_chunk_label:
    function parse_chunk_label(label)
        local name, version = 
            string.match(label, "(.*) v(%d+)$")
        if name then return name, tonumber(version)
        else return label, 0 end
    end

    assert(parse_chunk_label("foo") == "foo")
    assert(({parse_chunk_label("foo")})[2] == 0)
    assert(parse_chunk_label("foo v32") == "foo")
    assert(({parse_chunk_label("foo v32")})[2] == 32)

That covers all that’s needed to parse input.

### `tangle` ###

This is the subroutine whose job it is
to produce a runnable version of a
literate program.

Our `tangle` routine in this case is passed the name of an
initial chunk and a version number. In order for it to be
able to invoke itself recursively and still produce readable
output (and, in Python, parseable output) it also takes an
indentation parameter.

    -- in tangle:
    <<tangle definitions>>

    function tangle(chunks, chunkname, version, indent)
        if indent == nil then indent = '' end
        
        <<get the text of the chunk>>

        for _, line in ipairs(text) do
            local nindent, nchunkname = parse_reference(line)
            if nindent then
                tangle(chunks, nchunkname, version, indent..nindent)
            else
                io.write(indent..line.."\n")
            end
        end
    end

This is simple enough: when we encounter a reference, we
recurse, concatenating the indentation; and otherwise we
simply indent the line and output it. (The indentation is
essential for languages like Haskell and Python.)

The process of getting the text must worry about error
conditions.

    -- in get the text of the chunk:
    local contents = chunks[chunkname]
    if contents == nil then
        error(string.format("chunk `%s` does not exist", 
                            chunkname))
    end

    local text = get_chunk_text(contents, version)
    if text == nil then 
        error(string.format("chunk `%s` has no version `%d`",
                            chunkname, version))
    end

This depends on functions `get_chunk_text` and `parse_reference`.

    -- in tangle definitions:
    <<get_chunk_text>>

    <<parse_reference>>

`get_chunk_text` need only walk the relevant part of the
`chunks` table.  Recall that the contents for a chunk are
simply stored as a list of `{v=3, text="foo"}` structs, so we
can pull them out as follows:

    -- in get_chunk_text:
    function get_chunk_text(contents, version)
        local best
        for _, it in ipairs(contents) do
            if it.v <= version and (not best or
                                    it.v > best.v) then
                best = it
            end
        end
        if best then return best.text else return nil end
    end

    do
        local contents = {{v=0, text={"a"}},
                          {v=2, text={"b"}},
                          {v=1, text={"c"}}}
        assert(get_chunk_text(contents, 0)[1] == "a")
        assert(get_chunk_text(contents, 1)[1] == "c")
        assert(get_chunk_text(contents, 2)[1] == "b")
        assert(get_chunk_text(contents, 3)[1] == "b")
        assert(get_chunk_text(contents, -1) == nil)
    end

`parse_reference` just needs to match the `<<whatever>>`
references and pull out whatever indentation precedes them;
it turns out Lua’s pattern-matching can do this directly.

    -- in parse_reference:
    function parse_reference(line)
        return string.match(line, "^(%s*)<<(.*)>>(%s*)$")
    end

    do
        local indent, name = parse_reference("  <<foo>>\n")
        assert(indent == "  ")
        assert(name == "foo")
        assert(parse_reference("bits << shiftlen >> 1") == nil)
    end

### `list_chunk_names_and_versions` ###

Given this structure, listing either the chunk names or the
versions should be simple. Unfortunately, listing both of
them is a little annoying, because the output then requires
parsing. But we can take advantage of this to be more
explanatory.

We’d like to only list the names of *root chunks*, that is,
those that aren’t included in any other chunk. Often there
will be only one of them.

    -- in list_chunk_names_and_versions:
    function list_chunk_names_and_versions(chunks)
        <<display help message>>

        <<traverse chunks table>>

        <<display versions>>

        <<display chunk names>>
    end

We’ll output one thing per line:

    -- in display help message:
    io.write("# Listing versions and root chunk names.\n")
    io.write("# Version 12 is displayed as:\n")
    io.write("# v 12\n")
    io.write("# Chunk name foo bar is displayed as:\n")
    io.write("# n foo bar\n")
    io.write("# To tangle a particular root chunk, run:\n")
    io.write("# "..arg[0].." chunkname\n")
    io.write("# That tangles version 0 by default; to specify v69:\n")
    io.write("# "..arg[0].." chunkname 69\n")

We traverse the table to build up information for what we
display later.

    -- in traverse chunks table:
    local versions, referenced_chunks = {}, {}
    for name, contents in pairs(chunks) do
        for _, it in ipairs(contents) do
            versions[it.v] = true

            for _, line in ipairs(it.text) do
                local _, chunkname = parse_reference(line)
                if chunkname ~= nil then 
                    referenced_chunks[chunkname] = true
                end
            end
        end
    end

Then displaying the versions is easy; we need only to produce
the keys from the versions table:

    -- in display versions:
    for version, _ in pairs(versions) do
        io.write(string.format("v %d\n", version))
    end

Displaying the chunk names is almost as easy:

    -- in display chunk names:
    for name, _ in pairs(chunks) do
        if not referenced_chunks[name] then
            io.write("n "..name.."\n")
        end
    end

The build script
----------------

Rebuilding handaxeweb from this document by hand is a
little tedious. So here's a shell script that syntax-checks
and double-compile checks.

    # in build_handaxeweb:
    #!/bin/sh
    if ! [ -x /usr/bin/lua ] ; then
       echo "You need Lua installed at /usr/bin/lua to build Handaxeweb" >&2
       exit 1
    fi

    set -ve
    ./handaxeweb.lua handaxeweb.lua 0 < handaxeweb.md > handaxeweb2.lua

    # test new version
    lua handaxeweb2.lua handaxeweb.lua 0 < handaxeweb.md > handaxeweb3.lua

    # try building it with itself:
    lua handaxeweb3.lua handaxeweb.lua 0 < handaxeweb.md > handaxeweb4.lua

    # verify output is the same:
    diff handaxeweb3.lua handaxeweb4.lua

    # okay, we’ll accept it
    cp handaxeweb4.lua handaxeweb.lua

    ./handaxeweb.lua build_handaxeweb 0 < handaxeweb.md > build_handaxeweb.new
    cp build_handaxeweb.new build_handaxeweb

Flaws in handaxeweb
-------------------

There are several things I could do to improve this program
without changing its functionality.

    (in this part of the document there is no code:)
    (This note is needed because of how 
    Markdown structures nested lists, sigh.)

* The state machine in `parse_input` is obtuse and bug-prone.

* There are a number of subroutines and abstraction layers
  that would simplify the main program logic:

    * Appending one list to another (in two places).
    * Some kind of parsing machinery, probably.
    * An ordered container supporting insertion and
      nearest-match searching.
    * Set arithmetic; in particular, set subtraction.
    * Collections stuff: keys of a table, mapping a function
      over a list, printing all the items in a list.

* Appending to a versioned chunk is still kind of
  inconvenient. If you could say `<<same chunkname v3>>` this
  problem would mostly go away.

* The default to output should probably be the last version,
  not version 0.

* There’s still no syntax highlighting or tables of contents
  in the output.

* Emacs isn’t smart enough to do syntax highlighting in the
  input.

* Compiler error messages are subpar because handaxeweb
  doesn’t know enough to generate `#line` directives. (And
  for some languages, there is no such thing.)

Probably the right thing to do for some of these problems is
to use parsing tools to parse the input.

### a PEG for handaxeweb’s input ###

    # in a PEG for handaxeweb:
    # Top-level constructs, down to the paragraph level:
    litprog   <- (!chunk (bl / textpara / codepara))* chunk*.
    chunk     <- header (textpara* !header codepara)*.
    codepara  <- first: indented+ more: (bl+ indented+)*.
    textpara  <- bl* unindented+ bl*.

    # Types of lines:
    header      <-     indent nonalnum* "in " defname ":" nonalnum* nl.
    indented    <- !bl indent (more: wsp* reference / text: normal+) nl.
    bl          <- wsp* nl.  # Blank line.
    unindented  <- !indent normal+ nl.

    # Syntax within lines:
    defname    <- name: (!version normal)* version.
    version    <- (" v" n: number+ / ) !!":".
    reference  <- "<<" name: (!">>" normal)* ">>".
    indent     <- "    ".

    # Character classes:
    nonalnum  <- !alnum normal.
    alnum     <- uppercase / lowercase / number.
    uppercase <- "A" / "B" / "C" / "D" / "E" / "F" / "G" / 
                 "H" / "I" / "J" / "K" / "L" / "M" / "N" / 
                 "O" / "P" / "Q" / "R" / "S" / "T" / "U" / 
                 "V" / "W" / "X" / "Y" / "Z".
    lowercase <- "a" / "b" / "c" / "d" / "e" / "f" / "g" / 
                 "h" / "i" / "j" / "k" / "l" / "m" / "n" / 
                 "o" / "p" / "q" / "r" / "s" / "t" / "u" / 
                 "v" / "w" / "x" / "y" / "z".
    number    <- "0" / "1" / "2" / "3" / "4" / 
                 "5" / "6" / "7" / "8" / "9".
    normal    <- !nl char.
    nl        <- "\n".
    wsp       <- " " / "\t".

And that pretty much covers the entire deep structure of the
input. All the indentation, logic of blank lines between
other indented lines, parsing of references, version numbers,
carrying chunk headers from one indented region to the next,
and so on, is in there. The only thing that really remains to
be done is specifying what to do with it: concatenate the
`first` and `more` parts of `codepara`s, default version
numbers to zero, dump the codepara parts of chunks into a
dictionary of ordered-search structures, and then run
`tangle`.

(The grammar is slightly different from the one implemented
by my current implementation: it no longer allows : or >>, in
different contexts, inside of chunk names.)

<link rel="stylesheet" href="http://canonical.org/~kragen/style.css" />
<script src="http://canonical.org/~kragen/sw/addtoc.js"></script>
