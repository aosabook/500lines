class Device

  attr_reader :rate, :method, :steps, :trial

  def initialize(rate = nil, steps = nil, trial = nil, method = nil)
    @rate   = (rate.to_f.round > 0) ? rate.to_f.round : 100
    @steps  = steps.to_f.round if steps.to_f.round > 0
    @trial  = trial
    @method = method
  end

end