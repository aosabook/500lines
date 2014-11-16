class Trial

  attr_reader :name, :rate, :steps, :method

  def initialize(name = nil, rate = nil, steps = nil, method = nil)
    @name   = name
    # @rate   = (rate.to_f.round > 0) ? rate.to_f.round : 100
    # @steps  = steps.to_f.round if steps.to_s != '' && steps.to_f.round >= 0
    @rate   = integer_or_default(rate, 100, 0)
    @steps  = integer_or_default(steps, nil, -1)
    @method = method
  end

private

  def integer_or_default(val, default, greater_than_val)
    int = Integer(val)
    (int > greater_than_val) ? int : default
  rescue ArgumentError, TypeError
    default
  end
  
  # def integer_or_default(val, default, greater_than_val)
  #   int = val.to_i
  #   if (int.to_s == val.to_s)
  #     (int > greater_than_val) ? int : default
  #   else
  #     default
  #   end
  # end

end
