class Device

  attr_reader :data, :format, :rate, :method, :steps, :trial

  def initialize(params = {})
    params = {} unless params.kind_of? Hash

    @data = params[:data]
    set_format
    
    @rate   = ((rate = params[:rate].to_f.round) > 0) ? rate : 100
    @method = params[:method]
    @steps  = ((steps = params[:steps].to_f.round) > 0) ? steps : nil
    @trial  = params[:trial]
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
    @format = 'accelerometer' if regexp_accl.match(@data)
    @format = 'gravity'       if regexp_grav.match(@data)
    raise "Bad Input. Ensure accelerometer or gravity data is properly formatted." unless @format
  end

end