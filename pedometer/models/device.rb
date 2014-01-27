class Device

  attr_reader :data, :format, :rate, :latency

  def initialize(data, rate=nil, latency=nil)
    @data = data
    set_format

    rate    = rate.to_f.round
    latency = latency.to_f

    @rate    = (rate > 0) ? rate : 100
    @latency = (latency > 0) ? latency : 0
  end

private

  def set_format
    # TODO: Can we clean this up? 
    any_decimal = '-?\d+(?:\.\d+)?'
    regexp_accl = Regexp.new('^((' + any_decimal + ',){2}' + 
                                              any_decimal + ';)+$')
    regexp_grav = Regexp.new('^(((' + any_decimal + ',){2}(' +
                                         any_decimal + ')){1}[|](' +
                                         any_decimal + ',){2}(' +
                                         any_decimal + ';){1})+$')
    @format = 1 if regexp_accl.match(@data)
    @format = 2 if regexp_grav.match(@data)
    raise "Bad Input. Ensure accelerometer or gravity data is properly formatted." unless @format
  end

end