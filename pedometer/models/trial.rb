class Trial

  attr_reader :name, :rate, :steps, :method

  def initialize(name = nil, rate = nil, steps = nil, method = nil)
    @name   = name
    @rate   = (rate.to_f.round > 0) ? rate.to_f.round : 100
    @steps  = steps.to_f.round if steps.to_s != '' && steps.to_f.round >= 0
    @method = method
  end

end