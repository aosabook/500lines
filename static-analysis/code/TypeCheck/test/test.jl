module TestTypeCheck
  using TypeCheck, FactCheck
 
  istype(t) = isa(t,TypeCheck.AType)
 
  facts("Check Return Types: Make Sure It Runs on Base") do
    for n in names(Base)
      if isdefined(Base,n)
        f = eval(Base,n)
        if isgeneric(f) && typeof(f) == Function
          context(string(n)) do
            @fact TypeCheck.check_return_types(f) => anything # => FunctionSignature([],Symbol)
            [@fact istype(TypeCheck.returntype(e)) => true for e in code_typed(f)]
            [@fact TypeCheck.returntype(e) => istype for e in code_typed(f)]
          end
        end
      else
        @fact n => x->isdefined(Base,x)
      end
    end
  end

  caught(x) = x[2] == true
  notcaught(x) = x[2] == false
  function check_return(f,check)
    @fact length(code_typed(f)) => 1
    @fact TypeCheck.check_return_type(code_typed(f)[1]) => check
  end

  facts("Check Return Types: True Positives") do
    barr(x::Int) = isprime(x) ? x : false
    check_return(barr, caught)
  end

  facts("Check Return Types: False Negatives") do
    foo(x::Any) = isprime(x) ? x : false
    check_return(foo, notcaught)
  end


  facts("Check Loop Types: Make Sure It Runs on Base") do
    for n in names(Base)
      if isdefined(Base,n)
        f = eval(Base,n)
        if isgeneric(f) && typeof(f) == Function
          context(string(n)) do
            @fact TypeCheck.check_loop_types(f) => anything # => LoopResults
          end
        end
      else
        @fact n => x->isdefined(Base,x)
      end
    end
  end

  passed(x) = isempty(x.methods)
  failed(x) = !passed(x)
  function check_loops(f,check)
    @fact length(code_typed(f)) => 1
    @fact check_loop_types(f) => check
  end

  facts("Check Loop Types: True Positives") do
    function f1(x::Int)
      for n in 1:x
        x /= n
      end
      return x
    end
    check_loops(f1,failed)
  end

  facts("Check Loop Types: True Negatives") do
    function g1()
      x::Int = 5
      for i = 1:100
        x *= 2.5
      end
      return x
    end
    check_loops(g1,passed)
    function g2()
      x = 5
      x = 0.2
      for i = 1:10
        x *= 2
      end
      return x
    end
    check_loops(g2,passed)
  end


  facts("Check Method Calls: Make Sure It Runs on Base") do
    for n in names(Base)
      if isdefined(Base,n)
        f = eval(Base,n)
        if isgeneric(f) && typeof(f) == Function
          context(string(n)) do
            @fact TypeCheck.check_method_calls(f) => anything # => FunctionCalls
          end
        end
      else
        @fact n => x->isdefined(Base,x)
      end
    end
  end

  facts("Check Method Calls: True Positives") do
  end

  facts("Check Method Calls: False Negatives") do
  end

  exitstatus()
end
