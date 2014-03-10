class Device

  # TODO:
  # - Trial should be description? Method isn't great? State, ...
  attr_reader :data, :rate, :method, :steps, :trial, :format

  def initialize(data = nil, rate = 100, method = nil, steps = nil, trial = nil)
    rate_param = rate.to_f.round
    steps_param = steps.to_f.round

    @data   = data
    @rate   = (rate_param > 0) ? rate_param : 100
    @method = method
    @steps  = steps_param if steps_param > 0
    @trial  = trial

    set_format
  end

private

  # TODO: 
  # - Does this belong here? 
  # - Can blow up on parsing, don't need to make sure it's in the right format
  def set_format
    any_decimal = '-?\d+(?:\.\d+)?'
    regexp_accl = Regexp.new('^((' + any_decimal + ',){2}' + 
                                              any_decimal + ';)+$')
    regexp_grav = Regexp.new('^(((' + any_decimal + ',){2}(' +
                                         any_decimal + ')){1}\|(' +
                                         any_decimal + ',){2}(' +
                                         any_decimal + ';){1})+$')
    @format = 'accelerometer' if regexp_accl.match(@data)
    @format = 'gravity'       if regexp_grav.match(@data)
    raise "Bad Input. Ensure accelerometer or gravity data is properly formatted." unless @format
  end

end