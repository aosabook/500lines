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

    This covers the actual act of writing the code, the time spent reading the documentation for libraries you use, and figuring out how to get at the information you need to write the analysis. This could involve reading in a file of code, parsing it to understand the structure, and then making your specific check on the structure. Parsing code into a representative structure is a complicated business, and gets more complicated as the language grows.
    In this chapter, we'll be depending on internal datastructures used by the compiler. This means that we don't have to worry about reading files or parsing them, but it does mean we have to work with data structures that are not in our control and that sometimes feel clumsy or ugly. Besdies all the work we'll save by not having to understand the code by ourselves, working with the same datastructures the compiler uses means that our checks will be based on an accurate assesment of the compilers understanding -- which means they'll be accurate to how the code will actually run.

We're going to work through these steps for each of the individual checks implemented in this chapter. Step 1 requires enough understanding of the language we're analyzing to empathize with the kinds of problems it's users face. All the code in this chapter is Julia code, written to analyze Julia code.

# A Very Brief Introduction to Julia

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

We haven't really seen the "multiple" part yet, but if you're curious about Julia you'll have to look that up on your own. We need to move on to our first check.

# Checking the Types of Variables in Loops

A feature of Julia that sets it apart from other high-level languages is its speed. As in most programming languages, writing very fast code in Julia involves an understanding of how the computer works and how Julia works. In Julia, an important part of helping the compiler create fast code for you is writing type-stable code. When the compiler can see that a variable in a section of code will always be the same specific type, it can do more optimizations than if it believes (correctly or not) that there are many possible types for that variable.

For example, let's write a function that takes an `Int` and then increases it by some amount. If the number is small (less than 10), let's increase it by a big number (50), but if it's big, let's only increase it by a little (0.5).

~~~jl
function increment(x::Int)
  if x < 10
    x = x + 50
  else
    x = x + 0.5
  end
  return x
end
~~~

This function looks pretty straight-forward, but the type of `x` is unstable. At the end of this function, `return x` might return an `Int` or it might return a `Float64`. This is because of the `else` clause; if you add an Int, like `22`, to `0.5`, which is a `Float64`, then you'll get a `Float64`, like `22.5`. This means that `5` will become `55` (an `Int`), but `22` will become `22.5` (a `Float64`). If there were more involved code after this function, then it would have to handle both types for `x`, since the compiler expects to need to handle both.

As with most efficiency problems, this issue is more pronounced when it happens during loops. Code inside for-loops and while-loops is run many, many times, so making it fast is more important than speeding up code that is only run a couple of times. Our first check is going to look for variables inside loops that have unstable types. First, let's look at an example of what we want to catch.

We'll be looking at two functions. Each of them sums the numbers 1 to 100, but instead of summing the whole numbers, it divides each one by 2 before summing it. Both functions will get the same answer (`2525.0`); both will return the same type (`Float64`). However, the first function, `unstable`, suffers from type-instability, while the second one, `stable`, does not.

~~~jl
function unstable()
  sum = 0
  for i=1:100
    sum += i/2
  end
  return sum
end
~~~

~~~.jl
function stable()
  sum = 0.0
  for i=1:100
    sum += i/2
  end
  return sum
end
~~~

The only textual difference between the two functions is in the initialization of `sum`: `sum = 0` vs `sum = 0.0`. In Julia, `0` is an `Int` literal and `0.0` is a `Float64` literal. How big of a difference could this tiny change even make?

Because Julia is Just-In-Time (JIT) compiled, the first run of a function will take longer than subsequent runs (because the first run includes the time it takes to compile it). When we benchmark functions, we have to be sure to run them once (or precompile them) before timing them.

~~~jl
julia> unstable()
2525.0

julia> stable()
2525.0

julia> @time unstable()
elapsed time: 9.517e-6 seconds (3248 bytes allocated)
2525.0

julia> @time stable()
elapsed time: 2.285e-6 seconds (64 bytes allocated)
2525.0
~~~

The `@time` macro prints out how long the function took to run and how many bytes were allocated while it was running. The number of bytes allocated increases every time new memory is needed; it does not decrease when the garbage collector vacuums up memory that's no longer being used. This means that the bytes allocated is related to the amount of time we spend allocating memory, but does not imply that we had all of that memory in-use at the same time.

If we wanted to get solid numbers for `stable` vs `unstable` we would need to make the loop much longer or run the functions many times. However, we can already see that `unstable` seems to be slower. More interestingly, we can see a large gap in the number of bytes allocated; `unstable` is allocated around 3kb of memory, where `stable` is using 64 bytes.

Since we can see how simple `unstable` is, we might guess that this allocation is happening in the loop. To test this, we can make the loop longer and see if the allocations increase accordingly. Let's make the loop go from 1 to 10000, which is 100 times more iterations; we'll look for the number of bytes allocated to also increase about 100 times, to around 300kb.

~~~jl
function unstable()
  sum = 0
  for i=1:10000
    sum += i/2
  end
  return sum
end
~~~

Since we redefined the function, we'll need to run it to have it compiled before we measure it. We expect to get a different, larger answer from the new function defintion, since it's summing more numbers now.

~~~jl
julia> unstable()
2.50025e7

julia>@time unstable()
elapsed time: 0.000667613 seconds (320048 bytes allocated)
2.50025e7
~~~

The new `unstable` allocated about 320kb, which is what we would expect if the allocations are happening in the loop. This difference between `unstable` and `stable` is because `unstable`'s `sum` must be boxed while `stable`'s `sum` can be unboxed. Boxed values consist of a type tag and the actual bits that represent the value; unboxed values only have their actual bits. The type tag is small, so that's not why boxing values allocates a lot more memory. The difference comes from what optimizations the compiler can make. When a variable has a concrete, immutable type, the compiler can unbox it inside the function. If that's not the case, then the variable must be allocated on the heap, and participate in the garbage collector. Immutable types are usually types that represent values, rather than collections of values; most numeric types, including `Int` and `Float64` are immutable, while `Array`s and `Dict`s are mutable. Because immutable types cannot be modified, you must make a new copy every time you change one. For example `4 + 6` must make a new `Int` to hold the result. In constract, the members of a mutable type can be updated in-place.

Because `sum` in `stable` has a concrete type (`Flaot64`), the compiler know that it can store it unboxed locally in the function and mutate it's value; `sum` will not be allocated on the heap and new copies don't have to be made every time we add `i/2`. Because `sum` in `unstable` does not have a concrete type, the compiler allocates it on the heap. Every time we modify sum, we allocated a new value on the heap. All this time spent allocating values on the heap (and retrieving them everytime we want to read the value of `sum`) is expensive.

Using `0` vs `0.0` is an easy mistake to make, especially when you're new to Julia. Automatically checking that variables used in loops are type-stable helps programmers get more insight into what the types of their variables are, in performance-critical sections of their code.

The type of `sum` in `unstable` is `Union(Float64,Int64)`. This is a `UnionType`. A variable of type `Union(Float64,Int64)` can hold values of type `Int64` or `Float64`; a value can only have one type. A `UnionType` can have more than two possible values. The specific thing that we're going to look for is `UnionType`d variables inside loops.

## Implmentation

In order to find those variables, we'll need to find what variables are used inside of loops and we'll need to find the types of those variables. After we have those results, we'll need to decide how to print them in a human-readable way.

* How do we find loops in Exprs
* How do we find the types of variables
* How do we print the results

// Saving for first implementation section
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
null
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


# Looking for Unused Variables

# Checking Functions for Type Statbility

# Tools for Insight into Variable Types
