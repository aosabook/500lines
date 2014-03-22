class Device

  # TODO:
  # - Trial should be description? Method isn't great? State, ...
  attr_reader :rate, :method, :steps, :trial

  def initialize(rate = nil, method = nil, steps = nil, trial = nil)
    rate_param = rate.to_f.round
    steps_param = steps.to_f.round

    @rate   = (rate_param > 0) ? rate_param : 100
    @method = method
    @steps  = steps_param if steps_param > 0
    @trial  = trial
  end

end