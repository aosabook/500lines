class Device

  attr_reader :data, :format, :rate, :latency, :method, :steps, :trial

  def initialize(params = {})
    params = {} unless params.kind_of? Hash

    @data = params[:data]
    set_format

    rate    = params[:rate].to_f.round
    latency = params[:latency].to_f

    @rate    = (rate > 0) ? rate : 100
    @latency = (latency > 0) ? latency : 0

    if meta_data = (params[:meta_data] && params[:meta_data].split(','))
      @method = meta_data[0]
      @steps  = meta_data[1].to_i
      @trial  = meta_data[2]
    end
  end

private

  def set_format
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