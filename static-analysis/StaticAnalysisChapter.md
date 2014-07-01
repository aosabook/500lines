# Static Analysis
by Leah Hanson for *500 Lines or Less*

Static Analysis is a way to check for problems in your code without running it. "Static" means at compile-time, rather than at run-time, and "analysis" because we're analyzing the code.

There are three phases to implementing static analysis:

1. Deciding what you want to check for

     This refers to the general problem you'd like to solve, in terms that a user of the programming language would recognize. Examples include:

     * Finding misspelled variable names
     * Finding race conditions in parallel code
     * Finding calls to unimplemented functions
2. Deciding how exactly to check for it

    While we could ask a friend to do one of the tasks listed above, they aren't specific enough to explain to a computer. To tackle "misspelled variable names", for example, we'd need to decide what misspelled means here. One option would be to claim variable names should be composed of English words from the dictionary; another, more useful, option is to look for variables that are only used once (the one time you mis-typed it).

    Now that we know we're looking for variables only used once, we can talk about kinds of variable usages (having their value assigned vs. read) and what code would or would not trigger a warning.

3. Implementation details

    This covers the actual act of writing the code, the time spent reading the documentation for libraries you use, and figuring out how to get at the information you need to write the analysis.

We're going to work through these steps for each of the individual checks implemented in this chapter. Step 1 requires enough understanding of the language we're analyzing to empathize with the kinds of problems it's users face. All the code in this chapter is Julia code, written to analyze Julia code.

## A Brief Introduction to Julia

Julia is a young language aimed at technical computing. It was released, at version 0.1 in the Spring of 2012; as of the summer of 2014, it is reaching version 0.3. Julia is a procedural language; it is not object-oriented and while it has functional features (anonymous functions, higher order functions, immutable data), it does encourage a functional style of coding. The feature that most programmers will find novel in Julia is multiple dispatch, which is also central to the design of most of it's APIs.

Here is a snippet of Julia code:

~~~jl
# A comment about increment
function increment(x::Int)
  return x + 1
end

increment(5)
~~~

This code defines a method of the function `increment` that takes one argument, named `x`, of type `Int`. The method returns the value of `x + 1`. Then, this freshly defined method is called with the value `5`; the function call, as you may have guessed, will evaluate to `6`.

The name `increment` refers to a generic function, which may have many methods. We have just defined one method of it. Let's define another:

~~~jl
# Increment x by y
function increment(x::Int, y::Number)
  return x + y
end

increment(5) # => 6
increment(5,4) # => 9
~~~

Now increment has two methods. Julia decides which method to run for a given call based on the number and types of the arguments; this is called dynamic multiple dispatch.

* *dynamic* because it's based on the types of the values used at run-time
* *multiple* because it looks at the types and order of all the arguments. Object-oriented languages use single dispatch because they only consider the first argument (In `x.foo(y)`, the first argument is `x`.)
* *dispatch* because this a way of matching function calls to method definitions.

We haven't really seen the "multiple" part yet, but if you're curious about Julia you'll have to look that up on your own. We need to move on to a few implementation details.

## Introspection in Julia.

When you or I introspect, we're thinking about how and why we think and feel. When code introspects, it examines the representation or execution properties of code in the same language (possibly it's own code). When code's introspection extends to modifying the examined code, it's called metaprogramming (programs that write or modify programs).

Julia makes it easy to introspect. There are four functions built-in to let us see what that compiler is thinking: `code_lowered`, `code_typed`, `code_llvm`, and `code_native`. Those are listed in order of what step in the compilation process their output is from; the left-most one is closest to the code we'd type and the right-most one is the closest to what the CPU runs. For this chapter, we'll focus on `code_typed`, which gives us the optimized, type-inferred abstract syntax tree (AST).

Let's look at the output of `code_typed` for that first method of `increment` that we defined.
~~~jl
# code_typed takes the function and argument types of the method you want
code_typed(increment,(Int,))
~~~

The output looks like this:
~~~
1-element Array{Any,1}:
 :($(Expr(:lambda, {:x}, {{},{{:x,Int64,0}},{}}, :(begin  # REPL, line 12:
        return top(box)(Int64,top(add_int)(x::Int64,1))::Int64
    end::Int64))))
~~~

The output is an `Array` because you can call `code_typed` with an ambiguous tuple of types -- which would result in multiple methods matching and being returned as results. The `Array` contains `Expr`s, which represent expressions.

An `Expr` has three fields:

* `head`, which is a `Symbol`. In our example, it is `:lambda`.
* `typ`, which is a `Type`. The outer `Expr`s from `code_typed` always set this to `Any`.
* `args`, which is a structure of nested `Array{Any,1}`. It is made of untyped, nested lists.

For `Expr`s from `code_typed`, there are always three elements in `args`.

1. A list of argument names
2. A list of metadata about variables used in the method
3. The body of the function, as another `Expr`

The `Expr` of the function stores the list of expressions in the body of the method in its `args` field. The inferred return type of the method is stored in the `typ` field. We can define some helper functions to make these simple operations more readable; both of these functions will expect an `Expr` of the form returned by `code_typed`.

~~~jl
# given an Expr representing a method, return its inferred return type
returntype(e::Expr) =  e.args[3].typ # arrays index from 1

# given an Expr representing a method, return an Array of Exprs representing its body
body(e::Expr) = e.args[3].args
~~~

We can run these on our first `increment` method:
~~~jl
returntype(code_typed(increment,(Int,))[1]) # => Int64
body(code_typed(increment,(Int,))[1]) # => 2-element Array{Any,1}:
                                      # :( # REPL, line 12:)
                                      # :(return top(box)(Int64,top(add_int)(x::Int64,1))::Int64)
~~~

The `head` of an `Expr` indicates what type of `Expr` it is. For example, `:=` indicates an assignment, like `x = 5`. If we wanted to find all the places a method might return, we'd look for head values of `:return`. We can use the `body` helper function that we just wrote to write a function that takes an `Expr` from `code_typed` and returns all the return statements in its body.

~~~jl
# given an Expr representing a method, return all of the return statement in its body
returns(e::Expr) = filter(x-> typeof(x) == Expr && x.head==:return,body(e))

returns(code_typed(increment,(Int,))[1]) # => 1-element Array{Any,1}:
                                         # :(return top(box)(Int64,top(add_int)(x::Int64,1))
~~~

This `code_typed(increment,(Int,))[1]` stuff is getting rather tedious. Let's write a couple of helper methods so that we can run `code_typed` on a whole function at once.

~~~jl
# return the type-inferred AST for one method of a generic function
function Base.code_typed(m::Method)
 linfo = m.func.code
 (tree,ty) = Base.typeinf(linfo,m.sig,())
 if !isa(tree,Expr)
     ccall(:jl_uncompress_ast, Any, (Any,Any), linfo, tree)
 else
    tree
 end
end

# return the type-inferred AST for each method of a generic function
function Base.code_typed(f::Function)
  Expr[code_typed(m) for m in f.env]
end
~~~

Once we have a `code_typed` that handles `Method`s, handling whole `Function`s is just requires an array-comprehension over the methods of the given function. For a given `Function` `f`, we can get the methods using `f.env`. Handling a `Method` has more details to handle; the implementation is modeled closely on the existing built-in implementation.

`m.func.code` gives us the implementation of the method; `m.sig` gives us the types of it's arguments. Given these, `Base.typeinf` should return the type-inferred AST. However, if it was saved in a compressed state, we'll need to call one of the C functions used to implement parts of Julia, specifically `jl_uncompress_ast`, to get the `Expr` value we want to return.

~~~jl
[returntype(e) for e in code_typed(increment)] # => 2-element Array{Any,1}:
                                               # Int64
                                               # Any
~~~


