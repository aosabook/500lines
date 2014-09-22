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

    Now that we know we're looking for variables that are only used once, we can talk about kinds of variable usages (having their value assigned vs. read) and what code would or would not trigger a warning.

3. Implementation details

    This covers the actual act of writing the code, the time spent reading the documentation for libraries you use, and figuring out how to get at the information you need to write the analysis. This could involve reading in a file of code, parsing it to understand the structure, and then making your specific check on that structure. 

     Parsing code into a representative structure is a complicated business, and gets more complicated as the language grows. In this chapter, we'll be depending on internal data structures used by the compiler. This means that we don't have to worry about reading files or parsing them, but it does mean we have to work with data structures that are not in our control and that sometimes feel clumsy or ugly.

     Besides all the work we'll save by not having to parse the code by ourselves, working with the same data structures that the compiler uses means that our checks will be based on an accurate assessment of the compilers understanding -- which means our check will be accurate to how the code actually runs.

We're going to work through these steps for each of the individual checks implemented in this chapter. Step 1 requires enough understanding of the language we're analyzing to empathize with the kinds of problems it's users face. All the code in this chapter is Julia code, written to analyze Julia code.

# A Very Brief Introduction to Julia

Julia is a young language aimed at technical computing. It was released at version 0.1 in the Spring of 2012; as of the summer of 2014, it has reached version 0.3. In general, Julia looks a lot like Python, but with some type annotations and without any object-oriented stuff. The feature that most programmers will find novel in Julia is multiple dispatch, which has a pervasive impact on both API design and on other design choices in the language.

Here is a snippet of Julia code:

~~~jl
# A comment about increment
function increment(x::Int64)
  return x + 1
end

increment(5)
~~~

This code defines a method of the function `increment` that takes one argument, named `x`, of type `Int64`. The method returns the value of `x + 1`. Then, this freshly defined method is called with the value `5`; the function call, as you may have guessed, will evaluate to `6`. 

`Int64` is a type whose values are signed integers represented in memory by 64 bits; they are the integers that your hardware understands if your computer has a 64-bit processor. Types in Julia define the representation of data in memory, in addition to influencing method dispatch.

The name `increment` refers to a generic function, which may have many methods. We have just defined one method of it. Let's define another:

~~~jl
# Increment x by y
function increment(x::Int64, y::Number)
  return x + y
end

increment(5) # => 6
increment(5,4) # => 9
~~~

Now increment has two methods. Julia decides which method to run for a given call based on the number and types of the arguments; this is called dynamic multiple dispatch.

* *dynamic* because it's based on the types of the values used at run-time
* *multiple* because it looks at the types and order of all the arguments. Object-oriented languages use single dispatch because they only consider the first argument (In `x.foo(y)`, the first argument is `x`.) [This is true for Python and Ruby, but not Java and C++ which can have multiple methods of the same name within a class.]
* *dispatch* because this is a way of matching function calls to method definitions.

We haven't really seen the "multiple" part yet, but if you're curious about Julia, you'll have to look that up on your own. We need to move on to our first check.

# Checking the Types of Variables in Loops

A feature of Julia that sets it apart from other high-level languages is its speed. As in most programming languages, writing very fast code in Julia involves an understanding of how the computer works and how Julia works. In Julia, an important part of helping the compiler create fast code for you is writing type-stable code. When the compiler can see that a variable in a section of code will always contain the same specific type, the compiler can do more optimizations than if it believes (correctly or not) that there are many possible types for that variable.

For example, let's write a function that takes an `Int64` and then increases it by some amount. If the number is small (less than 10), let's increase it by a big number (50), but if it's big, let's only increase it by a little (0.5).

~~~jl
function increment(x::Int64)
  if x < 10
    x = x + 50
  else
    x = x + 0.5
  end
  return x
end
~~~

This function looks pretty straight-forward, but the type of `x` is unstable. At the end of this function, `return x` might return an `Int64` or it might return a `Float64`. This is because of the `else` clause; if you add an `Int64`, like `22`, to `0.5`, which is a `Float64`, then you'll get a `Float64` (`22.5`).

`Float64` is a type that represents floating-point values stored in 64 bits; in C, it is called a `double`. This is one of the floating-point types that 64-bit processors understand.

In this definition of `increment`, this means that `5` will become `55` (an `Int64`), but `22` will become `22.5` (a `Float64`). If there were more code in or after this function, then it would have to handle both possible types for `x`, since the compiler (correctly) expects to need to handle both.

As with most efficiency problems, this issue is more pronounced when it happens during loops. Code inside for-loops and while-loops is run many, many times, so making it fast is more important than speeding up code that is only run once or twice. Therefore, our first check is going to look for variables inside loops that have unstable types.

First, let's look at an example of what we want to catch. We'll be looking at two functions. Each of them sums the numbers 1 to 100, but instead of summing the whole numbers, they divide each one by 2 before summing it. Both functions will get the same answer (`2525.0`); both will return the same type (`Float64`). However, the first function, `unstable`, suffers from type-instability, while the second one, `stable`, does not.

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

The only textual difference between the two functions is in the initialization of `sum`: `sum = 0` vs `sum = 0.0`. In Julia, `0` is an `Int64` literal and `0.0` is a `Float64` literal. How big of a difference could this tiny change even make?

Because Julia is Just-In-Time (JIT) compiled, the first run of a function will take longer than subsequent runs. (The first run includes the time it takes to compile the function for these argument types.) When we benchmark functions, we have to be sure to run them once (or precompile them) before timing them.

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

The `@time` macro prints out how long the function took to run and how many bytes were allocated while it was running. The number of bytes allocated increases every time new memory is needed; it does not decrease when the garbage collector vacuums up memory that's no longer being used. This means that the bytes allocated is related to the amount of time we spend allocating and managing memory, but does not imply that we had all of that memory in-use at the same time.

If we wanted to get solid numbers for `stable` vs `unstable` we would need to make the loop much longer or run the functions many times. However, it looks like `unstable` is probably slower. More interestingly, we can see a large gap in the number of bytes allocated; `unstable` has allocated around 3kb of memory, where `stable` is using 64 bytes.

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

Since we redefined the function, we'll need to run it so it gets compiled before we measure it. We expect to get a different, larger answer from the new function definition, since it's summing more numbers now.

~~~jl
julia> unstable()
2.50025e7

julia>@time unstable()
elapsed time: 0.000667613 seconds (320048 bytes allocated)
2.50025e7
~~~

The new `unstable` allocated about 320kb, which is what we would expect if the allocations are happening in the loop. To explain what's going on here, we're going to dive into how Julia works under the hood. 

This difference between `unstable` and `stable` is because `unstable`'s `sum` must be boxed while `stable`'s `sum` can be unboxed. Boxed values consist of a type tag and the actual bits that represent the value; unboxed values only have their actual bits. The type tag is small, so that's not why boxing values allocates a lot more memory.

The difference comes from what optimizations the compiler can make. When a variable has a concrete, immutable type, the compiler can unbox it inside the function. If that's not the case, then the variable must be allocated on the heap, and participate in the garbage collector. Immutable types are usually types that represent values, rather than collections of values; most numeric types, including `Int64` and `Float64`, are immutable. Because immutable types cannot be modified, you must make a new copy every time you change one. For example `4 + 6` must make a new `Int64` to hold the result. In contrast, the members of a mutable type can be updated in-place; this means you don't have to make a copy of the whole thing to make a change.

Because `sum` in `stable` has a concrete type (`Flaot64`), the compiler know that it can store it unboxed locally in the function and mutate it's value; `sum` will not be allocated on the heap and new copies don't have to be made every time we add `i/2`.

Because `sum` in `unstable` does not have a concrete type, the compiler allocates it on the heap. Every time we modify sum, we allocated a new value on the heap. All this time spent allocating values on the heap (and retrieving them every time we want to read the value of `sum`) is expensive.

Using `0` vs `0.0` is an easy mistake to make, especially when you're new to Julia. Automatically checking that variables used in loops are type-stable helps programmers get more insight into what the types of their variables are in performance-critical sections of their code.

The type of `sum` in `unstable` is `Union(Float64,Int64)`. This is a `UnionType`. A variable of type `Union(Float64,Int64)` can hold values of type `Int64` or `Float64`; a value can only have one of those types. A `UnionType` join any number of types (e.g. `UnionType(Float64, Int64, Int32)` joins three types). The specific thing that we're going to look for is `UnionType`d variables inside loops.

## Implementation

In order to find those variables, we'll need to find what variables are used inside of loops and we'll need to find the types of those variables. After we have those results, we'll need to decide how to print them in a human-readable format.

* How do we find loops in `Expr`s
* How do we find the types of variables
* How do we print the results

This process of examining Julia code and finding information about, from other Julia code, is called introspection. When you or I introspect, we're thinking about how and why we think and feel. When code introspects, it examines the representation or execution properties of code in the same language (possibly it's own code). When code's introspection extends to modifying the examined code, it's called metaprogramming (programs that write or modify programs).

Julia makes it easy to introspect. There are four functions built-in to let us see what that compiler is thinking: `code_lowered`, `code_typed`, `code_llvm`, and `code_native`. Those are listed in order of what step in the compilation process their output is from; the left-most one is closest to the code we'd type in and the right-most one is the closest to what the CPU runs. For this chapter, we'll focus on `code_typed`, which gives us the optimized, type-inferred abstract syntax tree (AST).

Anyway, we need to detect those pesky mistyped variable names. To implement this, we'll be using some built-in data structures. There is a function that exposes the type-inferred and optimized AST: `code_typed`. 

`code_typed` takes two arguments: the function of interest, and a tuple of argument types. For example, if we wanted to see the AST for a function `foo` when called with two Int64`s, then we would call `code_typed(foo, (Int64,Int64))`.

~~~jl
function foo(x,y)
  z = x + y
  return 2 * z
end

code_typed(foo,(Int64,Int64))
~~~

This is the structure that code_typed_ would return:
~~~
1-element Array{Any,1}:
 :($(Expr(:lambda, {:x,:y}, {{:z},{{:x,Int64,0},{:y,Int64,0},{:z,Int64,18}},{}}, :(begin  # none, line 2:
        z = (top(box))(Int64,(top(add_int))(x::Int64,y::Int64))::Int64 # line 3:
        return (top(box))(Int64,(top(mul_int))(2,z::Int64))::Int64
    end::Int64))))
~~~

First, this is an `Array`; this allows `code_typed` to return multiple matching methods. Some combinations of functions and argument types may not completely determine which method should be called. For exmaple, you could pass in an type like `Any`, which is the type at the top of the type hierarchy; all types are subtypes of `Any` (including `Any`). If we included `Any`s in our tuple of argument types, and had multiple potentially matching methods, then the `Array` from `code_typed` would have more than one element in it.

The structure we're interested in is inside the `Array`: it is an `Expr`. Julia uses `Expr`s (short for expression) to represent its AST. (An abstract syntax tree is how the compiler thinks about the meaning of your code; it's kind of like when you had to diagram sentences in grade school.) The `Expr` we get back represents one method. It has some metadata (about the variables that appear in the method) and the expressions that make up the body of the method.

First, let's pull our example `Expr` out to make it easier to talk about.
~~~jl
julia> e = code_typed(foo,(Int64,Int64))[1]
:($(Expr(:lambda, {:x,:y}, {{:z},{{:x,Int64,0},{:y,Int64,0},{:z,Int64,18}},{}}, :(begin  # none, line 2:
        z = (top(box))(Int64,(top(add_int))(x::Int64,y::Int64))::Int64 # line 3:
        return (top(box))(Int64,(top(mul_int))(2,z::Int64))::Int64
    end::Int64))))
~~~

Now we can ask some questions about `e`:
~~~.jl
julia> names(e)
3-element Array{Symbol,1}:
 :head
 :args
 :typ 

julia> e.head
:lambda

julia> e.args
3-element Array{Any,1}:
 {:x,:y}                                                                                                                                                                                     
 {{:z},{{:x,Int64,0},{:y,Int64,0},{:z,Int64,18}},{}}                                                                                                                                         
 :(begin  # none, line 2:
        z = (top(box))(Int64,(top(add_int))(x::Int64,y::Int64))::Int64 # line 3:
        return (top(box))(Int64,(top(mul_int))(2,z::Int64))::Int64
    end::Int64)

julia> e.typ
Any
~~~

We just asked `e` what names it has, and then asked what value each name corresponds to. An `Expr` has three properties: `head`, `typ` and `args`.

* `head` tells us what kind of expression this is; normally, you'd use ex separate types for this in Julia, but this is a type that models the structure used in the Lisp parser. Anyway, head tells us how the rest of the `Expr` is structured, and what it represents.
* `typ` is the inferred return type of the expression; every expresision in Julia results in some value when evaluated. `typ` is the type of the value that the expression will evaluate to. For nearly all `Expr`s, this value will be `Any`. Only the `body` of type-inferred methods and most expressions inside them will have their `typ`s set to something else. (Because `type` is a keyword, this field can't use that word as its name.)
* `args` is the most complicated part of Expr; its structure varies based on `head`. It's always an `Array{Any}` of `Array{Any}`s . This is means it's an untyped list of lists (very Lisp-y).

In this case, there will be three elements in `e.args`:

~~~jl
julia> e.args[1] # names of arguments as symbols
2-element Array{Any,1}:
 :x
 :y

julia> e.args[2] # three lists of variable metadata (names of locals, (variable name, type, bitflags) tuples, and captured variable names)
3-element Array{Any,1}:
 {:z}                                     
 {{:x,Int64,0},{:y,Int64,0},{:z,Int64,18}}
 {}                                       

julia> e.args[3] # an Expr containing the body of the method
:(begin  # none, line 2:
        z = (top(box))(Int64,(top(add_int))(x::Int64,y::Int64))::Int64 # line 3:
        return (top(box))(Int64,(top(mul_int))(2,z::Int64))::Int64
    end::Int64)
~~~

While the metadata is very interesting, it isn't necessary right now. The important part is the body of the method, which is the third argument. This is another `Expr`.

~~~.jl
julia> body = e.args[3]
:(begin  # none, line 2:
        z = (top(box))(Int64,(top(add_int))(x::Int64,y::Int64))::Int64 # line 3:
        return (top(box))(Int64,(top(mul_int))(2,z::Int64))::Int64
    end::Int64)

julia> body.head
:body

julia> body.type
ERROR: type Expr has no field type

julia> body.typ
Int64

julia> body.args
4-element Array{Any,1}:
 :( # none, line 2:)                                              
 :(z = (top(box))(Int64,(top(add_int))(x::Int64,y::Int64))::Int64)
 :( # line 3:)                                                    
 :(return (top(box))(Int64,(top(mul_int))(2,z::Int64))::Int64)    ~~
~~~

This `Expr` has head `:body` because it's the body of the method. The `typ` is the inferred return type of the method. The `args` holds a list of expressions; the list of expressions in the method definition.

There are a couple of annotations of line numbers, but most of it is setting the value of `z` (`z = x + y`) and returning `2 * z`. Notice that these operations have been replaced by `Int64`-specific intrinsic functions. The `top(function-name)` indicates an intrinsic function; something that is implemented in Julia's code generation, rather in Julia.

The metadata gave us the names and types of all variables appearing in this function. Now we need to look at a function body with a loop, in order to see what that looks like.

~~~jl
julia> function lloop(x)
         for x = 1:100
           x *= 2
         end
       end
lloop (generic function with 1 method)

julia> code_typed(lloop, (Int,))[1].args[3]
:(begin  # none, line 2:
        #s120 = $(Expr(:new, UnitRange{Int64}, 1, :(((top(getfield))(Intrinsics,:select_value))((top(sle_int))(1,100)::Bool,100,(top(box))(Int64,(top(sub_int))(1,1))::Int64)::Int64)))::UnitRange{Int64}
        #s119 = (top(getfield))(#s120::UnitRange{Int64},:start)::Int64        unless (top(box))(Bool,(top(not_int))(#s119::Int64 === (top(box))(Int64,(top(add_int))((top(getfield))(#s120::UnitRange{Int64},:stop)::Int64,1))::Int64::Bool))::Bool goto 1
        2: 
        _var0 = #s119::Int64
        _var1 = (top(box))(Int64,(top(add_int))(#s119::Int64,1))::Int64
        x = _var0::Int64
        #s119 = _var1::Int64 # line 3:
        x = (top(box))(Int64,(top(mul_int))(x::Int64,2))::Int64
        3: 
        unless (top(box))(Bool,(top(not_int))((top(box))(Bool,(top(not_int))(#s119::Int64 === (top(box))(Int64,(top(add_int))((top(getfield))(#s120::UnitRange{Int64},:stop)::Int64,1))::Int64::Bool))::Bool))::Bool goto 2
        1:         0: 
        return
    end::Nothing)
~~~

I skipped straight to the method body here. You'll notice there's no `for` or `while` loop keyword. Instead, the loop has been lowered to `label`s and `goto`s. The `goto` has a number in it; each `label` also has a number. The `goto` jumps to the the `label` with the same number. We're going to find loops by looking for `goto`s that jump backwards.

First, we'll need to find the labels and gotos, and figure out where which ones match.

~~~~.jl
# This is a function for trying to detect loops in the body of a Method
# Returns lines that are inside one or more loops
function loopcontents(e::Expr)
  b = body(e)
  loops = Int[]
  nesting = 0
  lines = {}
  for i in 1:length(b)
    if typeof(b[i]) == LabelNode
      l = b[i].label
      jumpback = findnext(
        x-> (typeof(x) == GotoNode && x.label == l) || (Base.is_expr(x,:gotoifnot) && x.args[end] == l),
        b, i)
      if jumpback != 0
        push!(loops,jumpback)
        nesting += 1
      end
    end
    if nesting > 0
      push!(lines,(i,b[i]))
    end

    if typeof(b[i]) == GotoNode && in(i,loops)
      splice!(loops,findfirst(loops,i))
      nesting -= 1
    end
  end
  lines
end
~~~

Above, we start by getting all the expressions in the body of method, as an `Array`.

~~~.jl
# Return the body of a Method.
# Takes an Expr representing a Method,
# returns Vector{Expr}.
body(e::Expr) = e.args[3].args
~~~

`loops` is an `Array` of label line numbers where `GoTo`s that are loops occur. `nesting` indicates the number of loops we are currently inside. `lines` is an `Array` of (index, `Expr`) tuples. 

We look at each expression in the body of `e`. If it is a lable, we check to see if there is a `goto` that jubmps to this label (and occurs after the current index). If the result of `findnext` is greater than zero, then such a goto node exists, so we'll add that to `loops` (the `Array` of loops we are currently in) and increment our `nesting` level.

If we're currently inside a loop, we push the current line to our array of lines to return.

If we're at a GotoNode, then we check to see if it's the end of a loop. If so, we remove the entry from loops and reduce our nesting level.

~~~.jl
# given `lr`, a Vector of expressions (Expr + literals, etc)
# try to find all occurances of a variables in `lr`
# and determine their types
function loosetypes(lr::Vector)
  symbols = SymbolNode[]
  for (i,e) in lr
    if typeof(e) == Expr
      es = copy(e.args)
      while !isempty(es)
        e1 = pop!(es)
        if typeof(e1) == Expr
          append!(es,e1.args)
        elseif typeof(e1) == SymbolNode
          push!(symbols,e1)
        end
      end
    end
  end
  loose_types = SymbolNode[]
  for symnode in symbols
    if !isleaftype(symnode.typ) && typeof(symnode.typ) == UnionType
      push!(loose_types, symnode)
    end
  end
  return loose_types
end
~~~~

We'll pass the output of `loopcontents` into `loosetypes`. The goal of this function is to find all the variables and their types in our lines-from-inside-loops input `Vector`.

In each expression that occurred inside a loop, `loosetypes` searches for occurrences of symbols and their associated types. Variable usages show up as `SymbolNode`s in the AST; `SymbolNode`s hold the name and inferred type of the variable.

We can't just check each expression that `loopcontents` collected to see if it's a `SymbolNode`. The problem is that each `Expr` may contain one or more `Expr`; each `Expr` may contain one or more `SymbolNode`s. This means we need to pull out any nested `Expr`s, so that we can look in each of them for `SymbolNode`s.

The while loop goes through the guts of all the `Expr`s, recursively, until it's seen all the `Expr`s (and hopefully all the `SymbolNode`s). Every time the loop finds a `SymbolNode`, it adds it to the vector `symbols`.

Now we have a list of variables and their types, so it's easy to check if a type is loose. `loosetypes` does that by looking for a specific kind of non-concrete type, a `UnionType`. We get a lot more "failing" results when we consider all non-concrete types to be "failing". This is because we're evaluating each method with it's annotated argument types -- which are likely to be abstract.

Now that we can do the check on an expression, we should make it easier to call on a users's code:

~~~.jl
# for a given Function, run checklooptypes on each Method
function checklooptypes(f::Callable;kwargs...)
  lrs = LoopResult[]
  for e in code_typed(f)
    lr = checklooptypes(e)
    if length(lr.lines) > 0 push!(lrs,lr) end
  end
  LoopResults(f.env.name,lrs)
end

# for an Expr representing a Method,
# check that the type of each variable used in a loop
# has a concrete type
checklooptypes(e::Expr;kwargs...) = LoopResult(MethodSignature(e),loosetypes(loopcontents(e)))
~~~

Now we have two ways to call `checklooptypes`:

1. On a whole function; this will check each method of the given function.

2. On a specific expression; this will work if the user extracts the results of `code_typed` themselves.

We can see both options work about the same for a function with one method:

~~~.jl
julia> using TypeCheck

julia> function foo(x::Int)
         s = 0
         for i = 1:x
           s += i/2
         end
         return s
       end
foo (generic function with 1 method)

julia> checklooptypes(foo)
foo(Int64)::Union(Int64,Float64)
	s::Union(Int64,Float64)
	s::Union(Int64,Float64)



julia> checklooptypes(code_typed(foo,(Int,))[1])
(Int64)::Union(Int64,Float64)
	s::Union(Int64,Float64)
	s::Union(Int64,Float64)
~~~

I've skipped an implementation detail here: how did we get the results to print out to the REPL like that?

The `checklooptypes` function returns a special type, `LoopResults`. This type has a function called `show` defined for it. The REPL calls `display` on values it wants to display; `display` will then call our `show` implementation.

`LoopResults` is the result of checking a whole function; it has the function name and the results for each method. `LoopResult` is the result of checking one method; it has the argument types and the loosely typed variables.

~~~.jl
type LoopResult
  msig::MethodSignature
  lines::Vector{SymbolNode}
  LoopResult(ms::MethodSignature,ls::Vector{SymbolNode}) = new(ms,unique(ls))
end

function Base.show(io::IO, x::LoopResult)
  display(x.msig)
  for snode in x.lines
    println(io,"\t",string(snode.name),"::",string(snode.typ))
  end
end

type LoopResults
  name::Symbol
  methods::Vector{LoopResult}
end

function Base.show(io::IO, x::LoopResults)
  for lr in x.methods
    print(io,string(x.name))
    display(lr)
  end
end
~~~

# Looking For Unused Variables

* Example of the problem we're checking for
* LHS vs RHS variable usages
* Looking for single-use variables
* Checking for `x += 2` single usages

# Checking Functions for Type Stability

* What does type stability mean
* Example of failing function
* Checking function argument & return types
* Where this fails

# Tools for Insight into Variable Types

* Implementing `whos` for functions
  + What is `whos` (modules)
  + Getting the variables in a function
  + implementation

* Implicit interfaces
  + Walking the type hierarchy
  + Getting the methods implemented for a type
  + Implementing the function

