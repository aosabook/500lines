TypeCheck.jl
============
[![Build Status](https://travis-ci.org/astrieanna/TypeCheck.jl.png?branch=master)](https://travis-ci.org/astrieanna/TypeCheck.jl)

Type-based static analysis for the Julia programming language.

There are three main checks you can run: `checkreturntypes`, `checklooptypes`, and `checkmethodcalls`.
Running a check on a function checks each method; running a check on a module checks each function (by checking each method of each function).

To use any of these functions, you'll need to `Pkg.add("TypeCheck")` once to install the package on your computer and then import it using `using TypeCheck`. You'll need to re-import every time you restart the REPL.

### `checkreturntypes`: do the return types of your functions depend on the types, not the values of your arguments?

It is considered good style in Julia to have the return type of functions depend only on their argument types, not on the argument values.
This function tries to check that you did so.

You can run this on a generic function or on a module:
* `checkreturntypes(istext)`
* `checkreturntypes(Base)`

It is only effective at catching functions with annotated argument types.

It will catch things like:
~~~
julia> foo1(x::Int) = isprime(x) ? x: false
foo1 (generic function with 1 method)

julia> checkreturntypes(foo1)
foo1(Int64)::Union(Bool,Int64)
~~~

However, it will not catch:
~~~
julia> foo2(x) = isprime(x) ? x : false
foo2 (generic function with 1 method)

julia> checkreturntypes(foo2)

~~~

Additionally, it does a check to see if the return type of the function depends on a function call in the return statement.
This prevents the analysis from complaining about every function that calls a "bad" function.
However, it's possible that this silences too many alerts.

### `checklooptypes`: do the variables in your loops have stable types?

A common performance problem is having unstable (numeric) variable types in an important loop.
Having stable types within loops allows Julia's JIT compiler to output code as fast as a static compiler;
having unstable types means resorting to slower, dynamic code.

You can run this on a generic function or on a module:
* `checklooptypes(sort)`
* `checklooptypes(Base)`

It will complain about:
~~~
julia> function barr1()
         x=4
         for i in 1:10
           x *= 2.5
         end
         x
       end
barr1 (generic function with 1 method)

julia> checklooptypes(barr1)
barr1()::Union(Float64,Int64)
	x::Union(Float64,Int64)
~~~

It will correctly not complain about:
~~~
julia> function barr2()
         x = 4
         x = 2.5
         for i=1:10
           x *= 2.5
         end
       end
barr2 (generic function with 1 method)

julia> checklooptypes(barr2)

~~~
and
~~~
julia> function barr3()
         x::Int = 4
         for i=1:10
           x *= 2.5
         end       
       end       
barr3 (generic function with 1 method)

julia> checklooptypes(barr3)

~~~
(`barr3()` will throw an error rather than actually making `x` a `Float64`.)


It is possible that it misses loose types in some cases, but I am not currently aware of them. Please let me know if you find one.

### `checkmethodcalls`: could your functions have run-time NoMethodErrors?

`NoMethodError`s are probably the most common error in Julia. This is an attempt to find them statically.

You can run this on a generic function or on a module:
* `checkmethodcalls(sort)`
* `checkmethodcalls(Base)`

This functionality is still clearly imperfect. I'm working on refining it to be more useful.

### More Helper Functions
This package also defined `code_typed(f::Function)` to get the Expr for each method of a function
and `whos(f::Function)` to get a listing of the names and types of all the variables in the function.

`whos`'s output is modeled on the output of the existing methods in Base:
~~~
julia> function xyz(x::Int,y)
         p = pi
         z = x + y * pi
       end
xyz (generic function with 1 method)

julia> whos(xyz)
(Int64,Any)::Any
	#s38	Any
	p	MathConst{:π}
	x	Int64
	y	Any
	z	Any
~~~

####`methodswithdescendants(t::DataType;onlyleaves::Bool=false,lim::Int=10)`

This method goes through the descendents of a given type and finds what methods are implemented for them. It returns a list of (Symbol,Float64) tuples, where the Symbol is the name of a function and the Float64 is the percentage of subtypes whose `methodswith` shows a result for that function.

Here's an example of calling it:
~~~julia
julia> using TypeCheck

julia> methodswithdescendants(Real)
10-element Array{(Symbol,Float64),1}:
 (:<,0.9166666666666666)      
 (:convert,0.9166666666666666)
 (:<=,0.9166666666666666)     
 (:+,0.75)                    
 (:-,0.7083333333333334)      
 (:*,0.6666666666666666)      
 (:~,0.5833333333333334)      
 (:|,0.5833333333333334)      
 (:&,0.5833333333333334)      
 (:$,0.5833333333333334)      

julia> methodswithdescendants(Real;onlyleaves=true)
10-element Array{(Symbol,Float64),1}:
 (:<,1.0)                   
 (:convert,1.0)             
 (:<=,1.0)                  
 (:~,0.7647058823529411)    
 (:bswap,0.7647058823529411)
 (:|,0.7647058823529411)    
 (:&,0.7647058823529411)    
 (:$,0.7647058823529411)    
 (:>>,0.7058823529411765)   
 (:>>>,0.7058823529411765)  

julia> methodswithdescendants(Real;onlyleaves=true,lim=20)
20-element Array{(Symbol,Float64),1}:
 (:<,1.0)                            
 (:convert,1.0)                      
 (:<=,1.0)                           
 (:~,0.7647058823529411)             
 (:bswap,0.7647058823529411)         
 (:|,0.7647058823529411)             
 (:&,0.7647058823529411)             
 (:$,0.7647058823529411)             
 (:>>,0.7058823529411765)            
 (:>>>,0.7058823529411765)           
 (:<<,0.7058823529411765)            
 (:*,0.6470588235294118)             
 (:count_ones,0.6470588235294118)    
 (:-,0.6470588235294118)             
 (:+,0.6470588235294118)             
 (:trailing_zeros,0.6470588235294118)
 (:leading_zeros,0.5882352941176471) 
 (:signbit,0.5882352941176471)       
 (:^,0.4117647058823529)             
 (:rem,0.4117647058823529)
~~~



### Other Ways to Run Checks
If you want to run these only on a single method, you can get the `Expr` for the method from `code_typed` and then pass that into the check you would like to run.

