class Device

  attr_reader :rate, :method, :steps, :trial

  def initialize(rate = nil, steps = nil, trial = nil, method = nil)
    rate_param = rate.to_f.round
    steps_param = steps.to_f.round

    @rate   = (rate_param > 0) ? rate_param : 100
    @steps  = steps_param if steps_param > 0
    @trial  = trial
    @method = method
  end

end