class Parser

  GRAVITY_COEFFICIENTS = {
    alpha: [1, -1.979133761292768, 0.979521463540373],
    beta:  [0.000086384997973502, 0.000172769995947004, 0.000086384997973502]
  }
  
  # Chebyshev II, Astop = 2, Fstop = 5, Fs = 100
  SMOOTHING_COEFFICIENTS = {
    alpha: [1, -1.80898117793047, 0.827224480562408], 
    beta:  [0.095465967120306, -0.172688631608676, 0.095465967120306]
  }  

  attr_reader :data, :format, :parsed_data, :dot_product_data, :filtered_data

  # TODO: 
  # - 78 character limit to prevent long lines
  def initialize(data)
    @data = data.to_s

    set_format
    parse_raw_data
    dot_product_parsed_data
    filter_dot_product_data
  end

private

  # TODO: Get rid of this. Just parse raw data and if it blows up throw exception.
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

  # TODO:
  # - Combine, try to split on pipe, determine format from that
  def parse_raw_data
    case @format
    when 'accelerometer'
      coordinates = @data.split(';')
      # TODO:
      # - [:x, :y, :z].each_with_index do |axis, index| to remove repetition
      x_series = coordinates.collect {|data| data.split(',')[0].to_f }
      y_series = coordinates.collect {|data| data.split(',')[1].to_f }
      z_series = coordinates.collect {|data| data.split(',')[2].to_f }
      
      xg_series = chebyshev_filter(x_series, GRAVITY_COEFFICIENTS)
      yg_series = chebyshev_filter(y_series, GRAVITY_COEFFICIENTS)
      zg_series = chebyshev_filter(z_series, GRAVITY_COEFFICIENTS)

      @parsed_data = []
      coordinates.length.times do |i|
        @parsed_data << {:x => (x_series[i]-xg_series[i]), 
                         :y => (y_series[i]-yg_series[i]), 
                         :z => (z_series[i]-zg_series[i]), 
                         :xg => xg_series[i], 
                         :yg => yg_series[i], 
                         :zg => zg_series[i]}
      end
    when 'gravity'
      @parsed_data = @data.split(';').collect do |data|
        accl, grav = data.split('|')
        accl = accl.split(',')
        grav = grav.split(',')

        {:x => accl[0].to_f, :y => accl[1].to_f, :z => accl[2].to_f,
         :xg => grav[0].to_f, :yg => grav[1].to_f, :zg => grav[2].to_f}
      end
    end
  end

  def dot_product_parsed_data
    @dot_product_data = @parsed_data.collect do |data|
      data[:x] * data[:xg] + data[:y] * data[:yg] + data[:z] * data[:zg]
    end
  end

  def filter_dot_product_data
    @filtered_data = chebyshev_filter(@dot_product_data, SMOOTHING_COEFFICIENTS)
  end

  def chebyshev_filter(input_data, coefficients)
    # TODO: 
    # - Fix loop to start from index 2
    output_data = [0,0]
    input_data.length.times do |i|
      next if i < 2
      output_data << coefficients[:alpha][0] * 
                      (input_data[i]   * coefficients[:beta][0] + 
                      input_data[i-1]  * coefficients[:beta][1] + 
                      input_data[i-2]  * coefficients[:beta][2] -
                      output_data[i-1] * coefficients[:alpha][1] -
                      output_data[i-2] * coefficients[:alpha][2])
    end
    output_data
  end
end